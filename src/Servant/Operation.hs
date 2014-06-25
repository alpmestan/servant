{-# LANGUAGE TypeOperators,
             ConstraintKinds,
             MultiParamTypeClasses,
             TypeFamilies,
             TypeSynonymInstances,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             ScopedTypeVariables #-}

module Servant.Operation where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Reflection
import Data.Text.Lazy (pack)
import GHC.Exts
import Network.HTTP.Types.Status
import Servant.Context
import Servant.Resource
import Servant.Response
import Web.Scotty.Trans

class Operation o where
  type DataConstraint o a i r :: Constraint
  type Function o c a i r :: *

  addHandler :: ( DataConstraint o a i r
                , Context c
                , Functor m
                , MonadIO m
                , ScottyError e
                )
    		     => Function o c a i r
             -> Resource m e c a i r ops
    			   -> Resource m e c a i r (o ': ops)

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
              (resp :: UpdateResponse, statuscode)
                <- toResponse <$> liftIO (withContext $ \c -> f c val)
              status statuscode
              json resp

addWith :: ( Functor m, MonadIO m, ScottyError e
           , Context c
           , FromJSON a, Response UpdateResponse r
           )
        => (c -> a -> IO r)
        -> Resource m e c a i r ops
        -> Resource m e c a i r (Add ': ops)
addWith f r = addHandler f r


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
              (resp :: UpdateResponse, statuscode)
                <- toResponse <$> liftIO (withContext $ \c -> f c key)
              status statuscode
              json resp

deleteWith :: ( Functor m, MonadIO m, ScottyError e
              , Context c, Parsable i, Response UpdateResponse r
              )
           => (c -> i -> IO r)
           -> Resource m e c a i r ops
           -> Resource m e c a i r (Delete ': ops)
deleteWith r f = addHandler r f

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
              elems <- liftIO (withContext f)
              status status200
              json elems

listWith :: ( Functor m, MonadIO m, ScottyError e
            , Context c, ToJSON a
            )
         => (c -> IO [a])
         -> Resource m e c a i r ops
         -> Resource m e c a i r (List ': ops)
listWith f r = addHandler f r

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
              (resp :: UpdateResponse, statuscode)
                <- toResponse <$> liftIO (withContext $ \c -> f c key newVal)
              status statuscode
              json resp

updateWith :: ( Functor m, MonadIO m, ScottyError e
              , Context c
              , FromJSON a, Parsable i, Response UpdateResponse r
              )
           => (c -> i -> a -> IO r)
           -> Resource m e c a i r ops
           -> Resource m e c a i r (Update ': ops)
updateWith f r = addHandler f r

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
              lookupResp <- liftIO (withContext $ \c -> f c key)
              let (resp, statuscode) = maybe (NotFound, status404)  
                                             (\x -> (Found x, status200)) 
                                             lookupResp
              status statuscode
              json resp

viewWith :: ( Functor m, MonadIO m, ScottyError e
            , Context c, Parsable i, ToJSON a
            )
         => (c -> i -> IO (Maybe a))
         -> Resource m e c a i r ops
         -> Resource m e c a i r (View ': ops)
viewWith r f = addHandler r f
