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

An abstraction for 'Resource's that can support any number
of operations, which will be tagged at the type-level.

Here's a complete specification for a 'Person' resource:

> personResource = 
>   mkResourceAt "/persons" "personid"
>     & addWith addPerson
>     & listWith listPersons
>     & deleteWith deletePerson

provided you have 'addPerson', 'listPersons' and 'deletePerson',
as described in "Servant.Operation".

-}
module Servant.Resource
  ( Resource(..)
  , mkResourceAt
  , catchingWith
  , (&)
  ) where

import Data.Proxy
import Data.Reflection
import Servant.Context
import Servant.Error
import Web.Scotty.Trans

-- | A 'Resource'.
--
-- * the @m@ type parameter is the monad on top of which Scotty will sit
-- * the @e@ type parameter is the error type used by your scotty monad
-- * the @c@ type parameter is the context/connection type (think DB connection)
-- * the @a@ type parameter is the type representing values for the 'Resource'
-- * the @i@ type parameter is the type by which we will index values of type @a@
-- * the @r@ type parameter is the type returned by the effectful db functions (add, update, delete)
-- * the @ops@ param is the type-level list of operations supported by the resource.
data Resource (m :: * -> *) e c a i r (ops :: [*]) =
  Resource { resourceRoute :: String  -- ^ base URI of the resource,
                                      --   e.g @/persons@
           , keyName       :: String  -- ^ a name to look up the identifier
                                      --   with in URIs
           , setupAction   :: ScottyT e m () -- ^ action to run when setting
                                             --   up the resource's handlers
           , ctx           :: Context c      -- ^ how do we access the context
           , errCatcher    :: ExceptionCatcher e -- ^ zero or more functions for catching
                                                 --   any time of exception. each of these
                                                 --   usually catch their own type of exceptions.
                                                 --   We only ask them to be convertible to the
                                                 --   error type used in your scotty application
           }

instance Show (Resource m e c a i r '[]) where
  show (Resource route kn _ _ _)
    = "Resource at: " ++ route
    ++ "\n  Indexed by key: " ++ kn
    ++ "\n  Supports:\n"

instance (Reifies o String, Show (Resource m e c a i r ops))
      => Show (Resource m e c a i r (o ': ops)) where
  show r
    = s' ++ "    - " ++ opString ++ "\n"

    where s'       = show $ dropHeadOp r
          opString = reflect (Proxy :: Proxy o)

-- | This is a helper function that lets us refer to the same
--   resource, but dropping the first operation from the list.
--   It's used when doing recursion in type classe instances.
dropHeadOp :: Resource m e c a i r (o ': ops) -> Resource m e c a i r ops
dropHeadOp (Resource route kname act withctx cs) =
  Resource route kname act withctx cs

-- | Create "an empty" 'Resource', i.e one that supports no
--   operation on it.
--
--   This is used to have a default, "empty" resource on which
--   we can add support for the different operations (see 'Op').
--
--   Intended to be used like this:
--
-- > mkResourceAt "/persons" "personid" withPGConnection
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
mkResourceAt :: Monad m
             => String -- ^ base URI of the resource
             -> String -- ^ name to lookup the identifier with in URIs
             -> Context c -- ^ how do we get our hand on the context (db connection, etc)
             -> Resource m e c a i r '[]
mkResourceAt pat kname withctx = 
  Resource pat kname (return ()) withctx noErrorHandling

-- | Add exception catching capability to
--   the 'Resource' using the given 'ExceptionCatcher'.
--
--   See "Servant.Error" for more information about 'ExceptionCatcher'.
catchingWith :: ExceptionCatcher e
             -> Resource m e c a i r ops
             -> Resource m e c a i r ops
catchingWith c r = r { errCatcher = c }

-- | Reversed function application
--
-- > x & f = f x
(&) :: a -> (a -> b) -> b
x & f = f x
