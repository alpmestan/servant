{-# LANGUAGE TypeOperators,
             ConstraintKinds,
             MultiParamTypeClasses,
             TypeFamilies,
             TypeSynonymInstances,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             ScopedTypeVariables #-}
{- |
Module      :  Servant.Operation
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

An 'Operation' class along with some standard
and commonly used instances.

Each operation can impose some specific constraints
on the types underlying a 'Resource', like being able
to convert to or from /JSON/ the core type that the 
'Resource' holds. This lets you "pay" only for the
operations you actually use.

-}
module Servant.Operation
  ( -- * The 'Operation' class
    Operation(..)
  , -- * Some standard operations
    Add
  , addWith
  , Delete
  , deleteWith
  , List
  , listWith
  , Update
  , updateWith
  , View
  , viewWith
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Reflection
import Data.Text.Lazy (pack)
import GHC.Exts
import Network.HTTP.Types.Status
import Servant.Context
import Servant.Error
import Servant.Resource
import Servant.Response
import Web.Scotty.Trans

-- | The 'Operation' class is where we specify anything
--   specific to an operation we want to support on resources.
--
--   Note that everything has been designed with the mindset
--   of not tying operations to the types they operate on.
--
--   If you want to define an 'Add' operation for blog posts that 
--   handles all the scotty boilerplate of getting the value from json
--   and returning the appropriate HTTP status depending on whether it
--   succeeded or failed, etc, you don't want to have to redefine everything
--   when you'll be adding say /comments/ instead of /blog posts/. 
--
--   So an operation is meant to basically be a reusable scotty handler.
--
--   All you need to get started is a (generally empty) type that'll just be
--   some kind of token for your operation.
--
--   > data Add
--
--   'Resource's have a 'Show' instance that require the operation
--   types to be somehow convertible to a String. We use
--   @Data.Reflection@ for this, so we'll quickly write an instance
--   so that our operation can be listed when we're printing a 'Resource'.
--
--   > instance Reifies Add String where
--   >   reify _ = "Add"
--
--   And now, let's officially make 'Add' an actual 'Operation'.
--
--   > instance Operation Add where
--
--   The first thing we need to do is what constraint we
--   are putting on:
--
--   * the core type manipulated by a 'Resource', the @a@ in @Resource m e c a i r ops@
--   * the index type manipulated by a 'Resource', the @i@ in @Resource m e c a i r ops@
--   * the result type of update operations ('Bool', @[Only Int]@ from postgresql-simple, etc) of a 'Resource', the @r@ in @Resource m e c a i r ops@
--
--   For our 'Add' handler, we are going to extract the value we're adding
--   from the JSON body of the request, so we will need a 'FromJSON' constraint
--   on @a@. We will also rely on 'UpdateResponse' by asking the return type of
--   the function that will add our value to a database to be "convertible"
--   to 'UpdateResponse' through a call to 'toResponse'. This all can be summed up in:
--
--   >   type DataConstraint Add a i r = (FromJSON a, Response UpdateResponse r)
--
--   Next up, we have to describe what the function for actually adding a value
--   to some kind of database will look like.
--
--   >   type Function Add c a i r = c -> a -> IO r
--
--   An example of such a function would be:
--
--   > addPerson :: PGSQL.Connection -> Person -> IO [Only Int]
--
--   And now, the meat of an operation: we have to set up an endpoint
--   and the corresponding handler (or /action/), by defining 'addHandler' for 'Add',
--   which in our case should have the following signature.
--
--   > addHandler :: (DataConstraint Add a i r, Functor m, MonadIO m, ScottyError e)
--   >            => Function Add c a i r
--   >            -> Resource m e c a i r ops
--   >            -> Resource m e c a i r (Add ': ops)
--
--   (We can see that this is where we're actually making operations
--   pile up in the type-level list.)
--
--   If we replace 'DataConstraint' and 'Function' with the types we've
--   given for 'Add', we get:
--
--   > addHandler :: (FromJSON a, Response UpdateResponse r, Functor m, MonadIO m, ScottyError e)
--   >            => (c -> a -> IO r)
--   >            -> Resource m e c a i r ops
--   >            -> Resource m e c a i r (Add ': ops)
--
--   Alright, let's go ahead and implement it.
--
--   >   addHandler addingFunc resource = 
--   >     resource { setupAction = setupAction resource >> addAction }
--   >    
--   >     where addAction = 
--   >             post (capture $ resourceRoute resource) $ do
--   >               val <- jsonData
--   >               (resp :: UpdateResponse, statuscode)
--   >                 <- toResponse <$> liftIO (withContext (ctx resource) $ \c -> f c val)
--   >               status statuscode
--   >               json resp
--
--   If considering our 'Person' example again, this would:
--
--   * set up an endpoint reached through HTTP POST requests,
--     at the path @/persons@ for example.
--   * when reached, this endpoint would first extract the request body
--     as JSON and convert this to a 'Person' value.
--   * it would then setup a 'Context' to use the provided "adding" function with,
--     and pass it along with the value to the said function, to actually
--     add the value to the database or whatever backend that context
--     represents.
--   * it would then use 'toResponse' from the 'Response' class to convert
--     the return type of /addingFunc/ to servant's reusable 'UpdateResponse'
--     response type which already has a JSON instance. Make sure you have such
--     an instance for the @r@ type of your @Resource m e c a i r ops@.
--
--   And now, just add a cute companion combinator:
--
--   > addWith :: ( Functor m, MonadIO m, ScottyError e
--   >            , FromJSON a, Response UpdateResponse r
--   >            )
--   >         => (c -> a -> IO r)
--   >         -> Resource m e c a i r ops
--   >         -> Resource m e c a i r (Add ': ops)
--   > addWith f r = addHandler f r
--
--   And you are done implementing addition support!
--
--   > mkResourceAt "/persons" "personid" withConnection
--   >   & updateWith personUpdate -- let's suppose it was already there
--
--   can be turned into:
--
--   > mkResourceAt "/persons" "personid" withConnection
--   >   & updateWith personUpdate -- let's suppose it was already there
--   >   & addWith personAdd
class Operation o where
  -- | The set of constraints your this operation
  --   imposes on the core resource type, its index type
  --   and the return type of "effectful" context update
  --   functions.
  type DataConstraint o a i r :: Constraint

  -- | What type the /database operations/ (so to speak)
  --   for this operation should have.
  --
  --   For example, if we consider 'Add', it's reasonable to
  --   expect @c -> a -> IO r@ since we just need a connection
  --   and a value to add. And we're using @r@ as return type 
  --   since it's an /effectful/ (so to speak, again) db operation.
  type Function o c a i r :: *

  -- | How should we update the 'Resource's handler
  addHandler :: ( DataConstraint o a i r
                , Functor m
                , MonadIO m
                , ScottyError e
                )
    		     => Function o c a i r
             -> Resource m e c a i r ops
    			   -> Resource m e c a i r (o ': ops)

-- | A generic 'Add' operation, which
--   relies on 'UpdateResponse'.
--
--   This is just used at the type-level, but is
--   useful through its 'Reifies' and 'Operation' instances.
data Add

instance Reifies Add String where
  reflect _ = "Add"

instance Operation Add where
  type DataConstraint Add a i r
    = (FromJSON a, Response UpdateResponse r)

  type Function Add c a i r = c -> a -> IO r

  addHandler f r = 
    r { setupAction = setupAction r >> addAction }
    
    where addAction = 
            post (capture $ resourceRoute r) $ do
              val <- jsonData
              raiseIfExc (errCatcher r) (withContext (ctx r) $ \c -> f c val) $ \res -> do
                let (resp :: UpdateResponse, statuscode) = toResponse res
                status statuscode
                json resp

-- | Handy combinator for adding 'Add' support to
--   a 'Resource'.
--
-- > mkResourceAt "/persons" "personid" withConnection
-- >   & addWith personAdd
addWith :: ( Functor m, MonadIO m, ScottyError e
           , FromJSON a, Response UpdateResponse r
           )
        => (c -> a -> IO r)
        -> Resource m e c a i r ops
        -> Resource m e c a i r (Add ': ops)
addWith f r = addHandler f r

-- | A generic 'Delete' operation, which
--   relies on 'UpdateResponse'.
--
--   This is just used at the type-level, but is
--   useful through its 'Reifies' and 'Operation' instances.
data Delete

instance Reifies Delete String where
  reflect _ = "Delete"

instance Operation Delete where
  type DataConstraint Delete a i r = 
    (Parsable i, Response UpdateResponse r)

  type Function Delete c a i r = c -> i -> IO r

  addHandler f r =
    r { setupAction = setupAction r >> deleteAction }

    where deleteAction =
            delete (capture $ resourceRoute r ++ "/:" ++ keyName r) $ do
              key <- param (pack $ keyName r)
              raiseIfExc (errCatcher r) (withContext (ctx r) $ \c -> f c key) $ \res -> do
                let (resp :: UpdateResponse, statuscode) = toResponse res
                status statuscode
                json resp

-- | Handy combinator for adding 'Delete' support to
--   a 'Resource'.
--
-- > mkResourceAt "/persons" "personid" withConnection
-- >   & deleteWith personDelete
deleteWith :: ( Functor m, MonadIO m, ScottyError e
              , Parsable i, Response UpdateResponse r
              )
           => (c -> i -> IO r)
           -> Resource m e c a i r ops
           -> Resource m e c a i r (Delete ': ops)
deleteWith r f = addHandler r f

-- | A generic 'List' operation.
--
--   This is just used at the type-level, but is
--   useful through its 'Reifies' and 'Operation' instances.
data List

instance Reifies List String where
  reflect _ = "List"

instance Operation List where

  type DataConstraint List a i r = ToJSON a

  type Function List c a i r = c -> IO [a]

  addHandler f r =
    r { setupAction = setupAction r >> listAction }

    where listAction = 
            get (capture $ resourceRoute r) $ do
              elems <- liftIO (withContext (ctx r) f)
              status status200
              json elems

-- | Handy combinator for adding 'List' support to
--   a 'Resource'.
--
-- > mkResourceAt "/persons" "personid" withConnection
-- >   & listWith personList
listWith :: ( Functor m, MonadIO m, ScottyError e
            , ToJSON a
            )
         => (c -> IO [a])
         -> Resource m e c a i r ops
         -> Resource m e c a i r (List ': ops)
listWith f r = addHandler f r

-- | A generic 'Update' operation, which
--   relies on 'UpdateResponse'.
--
--   This is just used at the type-level, but is
--   useful through its 'Reifies' and 'Operation' instances.
data Update

instance Reifies Update String where
  reflect _ = "Update"

instance Operation Update where

  type DataConstraint Update a i r =
    ( FromJSON a
    , Parsable i
    , Response UpdateResponse r
    )

  type Function Update c a i r = c -> i -> a -> IO r

  addHandler f r =
    r { setupAction = setupAction r >> updateAction }

    where updateAction =
            put (capture $ resourceRoute r ++ "/:" ++ keyName r) $ do
              key <- param (pack $ keyName r)
              newVal <- jsonData
              raiseIfExc (errCatcher r) (withContext (ctx r) $ \c -> f c key newVal) $ \res -> do
                let (resp :: UpdateResponse, statuscode) = toResponse res
                status statuscode
                json resp

-- | Handy combinator for adding 'Update' support to
--   a 'Resource'.
--
-- > mkResourceAt "/persons" "personid" withConnection
-- >   & updateWith personUpdate
updateWith :: ( Functor m, MonadIO m, ScottyError e
              , FromJSON a, Parsable i, Response UpdateResponse r
              )
           => (c -> i -> a -> IO r)
           -> Resource m e c a i r ops
           -> Resource m e c a i r (Update ': ops)
updateWith f r = addHandler f r

-- | A generic 'View' operation, which
--   relies on 'LookupResponse'.
--
--   This is just used at the type-level, but is
--   useful through its 'Reifies' and 'Operation' instances.
data View

instance Reifies View String where
  reflect _ = "View"

instance Operation View where

  type DataConstraint View a i r =
    ( Parsable i
    , ToJSON a
    )

  type Function View c a i r = c -> i -> IO (Maybe a)

  addHandler f r =
    r { setupAction = setupAction r >> viewAction }

    where viewAction =
            get (capture $ resourceRoute r ++ "/:" ++ keyName r) $ do
              key <- param (pack $ keyName r)
              lookupResp <- liftIO (withContext (ctx r) $ \c -> f c key)
              let (resp, statuscode) = maybe (NotFound, status404)  
                                             (\x -> (Found x, status200)) 
                                             lookupResp
              status statuscode
              json resp

-- | Handy combinator for adding 'View' support to
--   a 'Resource'.
--
-- > mkResourceAt "/persons" "personid" withConnection
-- >   & viewWith personView
viewWith :: ( Functor m, MonadIO m, ScottyError e
            , Parsable i, ToJSON a
            )
         => (c -> i -> IO (Maybe a))
         -> Resource m e c a i r ops
         -> Resource m e c a i r (View ': ops)
viewWith r f = addHandler r f
