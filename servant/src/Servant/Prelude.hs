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
Module      :  Servant.Prelude
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

Some standard REST-y operations.
-}
module Servant.Prelude where

import Servant.Resource

-- | A dummy type representing an operation that adds an entry.
data Add
instance Show Add where show _ = "Add"

-- | A dummy type representing an operation that deletes an entry.
data Delete
instance Show Delete where show _ = "Delete"

-- | A dummy type representing an operation that lists all entries.
data ListAll
instance Show ListAll where show _ = "ListAll"

-- | A dummy type representing an operation that updates an entry.
data Update
instance Show Update where show _ = "Update"

-- | A dummy type representing an operation that looks up an entry.
data View
instance Show View where show _ = "View"

-- | To 'Add' an entry, we require a function of type @c -> a -> IO r@.
type instance Operation Add c a i r     =      a -> c -> IO r
-- | To 'Delete' an entry, we require a function of type @c -> i -> IO r@.
type instance Operation Delete c a i r  = i      -> c -> IO r
-- | To 'List' all entries, we require a function of type @c -> IO [a]@.
type instance Operation ListAll c a i r =           c -> IO [a]
-- | To 'Update' an entry, we require a function of type @c -> i -> a -> IO r@.
type instance Operation Update c a i r  = i -> a -> c -> IO r
-- | To 'View' an entry, we require a function of type @c -> i -> IO (Maybe a)@.
type instance Operation View c a i r    = i      -> c -> IO (Maybe a)

-- | Make a 'Resource' support the 'Add'
--   operation by using your function.
--
--   Given:
--
--   > addPerson :: PGSQL.Connection -> Person -> IO Bool
--
--   you could do:
--
--   > mkResource "persons" postgresContext noErrorHandling
--   >   & addWith addPerson
addWith :: Contains Add ops ~ False
        => (a -> c -> IO r)
        -> Resource c a i r e ops
        -> Resource c a i r e (Add ': ops)
addWith = addOperation

-- | Make a 'Resource' support the 'Delete'
--   operation by using your function.
--
--   Given:
--
--   > deletePerson :: PGSQL.Connection -> PersonId -> IO Bool
--
--   you could do:
--
--   > mkResource "persons" postgresContext noErrorHandling
--   >   & deleteWith addPerson
deleteWith :: Contains Delete ops ~ False
           => (i -> c -> IO r)
           -> Resource c a i r e ops
           -> Resource c a i r e (Delete ': ops)
deleteWith = addOperation

-- | Make a 'Resource' support the 'ListAll'
--   operation by using your function.
--
--   Given:
--
--   > listAllPersons :: PGSQL.Connection -> IO [Person]
--
--   you could do:
--
--   > mkResource "persons" postgresContext noErrorHandling
--   >   & listAllWith listAllPersons
listAllWith :: Contains ListAll ops ~ False
            => (c -> IO [a])
            -> Resource c a i r e ops
            -> Resource c a i r e (ListAll ': ops)
listAllWith = addOperation

-- | Make a 'Resource' support the 'Update'
--   operation by using your function.
--
--   Given:
--
--   > updatePerson :: PGSQL.Connection -> PersonId -> Person -> IO Bool
--
--   you could do:
--
--   > mkResource "persons" postgresContext noErrorHandling
--   >   & updateWith updatePerson
updateWith :: Contains Update ops ~ False
           => (i -> a -> c -> IO r)
           -> Resource c a i r e ops
           -> Resource c a i r e (Update ': ops)
updateWith = addOperation

-- | Make a 'Resource' support the 'View'
--   operation by using your function.
--
--   Given:
--
--   > viewPerson :: PGSQL.Connection -> PersonId -> IO (Maybe Person)
--
--   you could do:
--
--   > mkResource "persons" postgresContext noErrorHandling
--   >   & viewWith viewPerson
viewWith :: Contains View ops ~ False
         => (i -> c -> IO (Maybe a))
         -> Resource c a i r e ops
         -> Resource c a i r e (View ': ops)
viewWith = addOperation
