{-# LANGUAGE DataKinds, 
             KindSignatures, 
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             ScopedTypeVariables #-}

{- |
Module      :  Servant.Resource
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

An abstraction for 'Resource's that can be added, updated, deleted, 
viewed and listed, but which can support only some of these operations,
depending on what the user code needs/wants.

Here's a complete specification for a 'Person' resource:

> personResource = 
>   mkResourceAt "/persons" "personid"
>     & addWith addPerson
>     & listWith listPersons
>     & deleteWith deletePerson

provided you have 'addPerson', 'listPersons' and 'deletePerson',
as described in the documentation for the 'mkResourceAt' function.

See the "Servant.Example" module for a full (but admittedly dumb) example
of specification.
-}
module Servant.Resource where

import Data.Proxy
import Data.Reflection

-- | Our base type representing the
--   operations a resource /may/ support
data Op = Add
        | Delete
        | Update
        | View
        | List
  deriving Show

-- | E.g: PGSQL.Connection -> Person -> IO Bool
type Adder   c a   r = c      -> a -> IO r
-- | E.g: PGSQL.Connection -> PersonId -> IO Bool
type Deleter c   i r = c -> i      -> IO r
-- | E.g: PGSQL.Connection -> IO [Person]
type Lister  c a     = c           -> IO [a]
-- | E.g: PGSQL.Connection -> PersonId -> Person -> IO Bool
type Updater c a i r = c -> i -> a -> IO r
-- | E.g: PGSQL.Connection -> PersonId -> IO (Maybe Person)
type Viewer  c a i   = c -> i      -> IO (Maybe a)

{-| 
  Since data constructors can now becomes types,
  we're using this to promote the constructors of 'Op'
  to the type level, with the goal to carry them at the type level
  in a list, so that a Resource will embed the list of the operations
  it supports at the type level.

  For that, at some point, we'll need to make up values just from these
  type-level operations, which are all types of kind 'Op'.

  This is what the following lets us do.
-}

instance Reifies Add Op where
  reflect _ = Add

instance Reifies Delete Op where
  reflect _ = Delete

instance Reifies List Op where
  reflect _ = List

instance Reifies Update Op where
  reflect _ = Update

instance Reifies View Op where
  reflect _ = View

-- | A 'Resource'.
--
-- * the @c@ type parameter is the context/connection type (think DB connection)
-- * the @a@ type parameter is the type representing values for the 'Resource'
-- * the @i@ type parameter is the type by which we will index values of type @a@
-- * the @r@ type parameter is the type returned by the effectful db functions (add, update, delete)
-- * the @ops@ param is the type-level list of operations supported by the resource.
data Resource c a i r (ops :: [Op]) =
  Resource { resourceRoute :: String  -- ^ base URI of the resource,
                                      --   e.g @/persons@
           , keyName       :: String  -- ^ a name to look up the identifier
                                      --   with in URIs
           , adder         :: Maybe (Adder c a r)     -- ^ "database" function used
                                                      --   to add a value
           , deleter       :: Maybe (Deleter c i r)   -- ^ "database" function used
                                                      --   to delete a value with a
                                                      --   particular id
           , lister        :: Maybe (Lister c a)      -- ^ "database" function used
                                                      --   to list values
           , updater       :: Maybe (Updater c a i r) -- ^ "database" function used
                                                      --   to update a value with a
                                                      --   particular id
           , viewer        :: Maybe (Viewer c a i)    -- ^ "database" function used
                                                      --   to view a value with a
                                                      --   particular id
           }

instance Show (Resource c a i r '[]) where
  show (Resource route kn _ _ _ _ _) 
    = "Resource at: " ++ route
    ++ "\n  Indexed by key: " ++ kn
    ++ "\n  Supports:\n"

instance (Reifies o Op, Show (Resource c a i r ops))
      => Show (Resource c a i r (o ': ops)) where
  show r
    = s' ++ "    - " ++ opString ++ "\n"

    where s'       = show $ dropHeadOp r
          opString = show (reflect proxy :: Op)
          proxy    = Proxy :: Proxy o

-- | This is a helper function that lets us refer to the same
--   resource, but dropping the first operation from the list.
--   It's used when doing recursion in type classe instances.
dropHeadOp :: Resource c a i r (o ': ops) -> Resource c a i r ops
dropHeadOp (Resource route kname a d l u v) =
  Resource route kname a d l u v

-- | Create "an empty" 'Resource', i.e one that supports no
--   operation on it.
--
--   This is used to have a default, "empty" resource on which
--   we can add support for the different operations (see 'Op').
--
--   Intended to be used like this:
--
-- > mkResourceAt "/persons" "personid"
-- >   -- & addWith addPerson
-- >   -- & deleteWith deletePerson
-- >   -- & listWith listPersons
-- >   -- & updateWith updatePerson
-- >   -- & viewWith viewPerson
--
--    where you just uncomment any line for which you want
--    the associated operation enabled.
--
--    In the persons example, the user-provided /xxxPerson(s)/ functions
--    would have to look like this:
--
-- > addPerson :: c -> Person -> IO r
-- > deletePerson :: c -> PersonId -> IO r
-- > listPersons :: c -> IO [Person]
-- > updatePerson :: c -> PersonId -> Person -> IO r
-- > viewPerson :: c -> PersonId -> IO (Maybe Person)
--
--    where:
--  
--    * @c@ is some connection/context type through which
--      you actually access and update values (think database connections,
--      'IORef's, in-memory database)
--    * @r@ is the type returned by your "effectful database functions",
--      i.e those that add, update or remove entries. This can be 'Bool',
--      @postgresql-simple@'s @[Only Int]@, etc.

mkResourceAt :: String -- ^ base URI of the resource
             -> String -- ^ name to lookup the identifier with in URIs
             -> Resource c a i r '[]
mkResourceAt pat kname = 
  Resource pat kname Nothing Nothing Nothing Nothing Nothing

-- | Reversed function application
--
-- > x & f = f x
(&) :: a -> (a -> b) -> b
x & f = f x

-- | Add support for adding @a@s using the given function
--
--   A perfectly valid 'Adder' would be:
--
-- > addPerson :: PGSQL.Connection -> Person -> IO Bool
--
--   Intended to be used in the following way:
--
-- > mkResourceAt "/persons" "personid"
-- >   & addWith addPerson
addWith :: Adder c a r 
        -> Resource c a i r ops 
        -> Resource c a i r (Add ': ops)
addWith a r = r { adder = Just a }

-- | Add support for deleting @a@s using the given function
--
--   A perfectly valid 'Deleter' would be:
--
-- > deletePerson :: PGSQL.Connection -> PersonId -> IO Bool
--
--   Intended to be used in the following way:
--
-- > mkResourceAt "/persons" "personid"
-- >   & deleteWith deletePerson
deleteWith :: Deleter c i r 
           -> Resource c a i r ops 
           -> Resource c a i r (Delete ': ops)
deleteWith del r = r { deleter = Just del }

-- | Add support for listing @a@s using the given function
--
--   A perfectly valid 'Lister' would be:
--
-- > listPersons :: PGSQL.Connection -> IO [Person]
--
--   Intended to be used in the following way:
--
-- > mkResourceAt "/persons" "personid"
-- >   & listWith listPersons
listWith :: Lister c a
         -> Resource c a i r ops
         -> Resource c a i r (List ': ops)
listWith list r = r { lister = Just list }

-- | Add support for updating @a@s given an index, using the given function
--
--   A perfectly valid 'Updater' would be:
--
-- > updatePerson :: PGSQL.Connection -> PersonId -> Person -> IO Bool
--
--   Intended to be used in the following way:
--
-- > mkResourceAt "/persons" "personid"
-- >   & updateWith updatePerson
updateWith :: Updater c a i r 
           -> Resource c a i r ops 
           -> Resource c a i r (Update ': ops)
updateWith up r = r { updater = Just up }

-- | Add support for viewing a particular @a@ (given its index) using the given function
--
--   A perfectly valid 'Viewer' would be:
--
-- > viewPerson :: PGSQL.Connection -> PersonId -> IO (Maybe Person)
--
--   Intended to be used in the following way:
--
-- > mkResourceAt "/persons" "personid"
-- >   & viewWith viewPerson
viewWith :: Viewer c a i 
         -> Resource c a i r ops 
         -> Resource c a i r (View ': ops)
viewWith vi r = r { viewer = Just vi }
