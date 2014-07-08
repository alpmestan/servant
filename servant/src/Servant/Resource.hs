{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Servant.Resource
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

Module for defining 'Resource's.

> EXAMPLE HERE
-}
module Servant.Resource
  ( Contains
  , Resource
  , name
  , context
  , excCatcher
  , withHeadOperation
  , dropHeadOperation
  , mkResource
  , addOperation
  , Operation
  , (&)
  , Ops
  ) where

import Servant.Context
import Servant.Error

-- | Heterogeneous list
data HList :: [*] -> * where
  Nil :: HList '[]
  Cons :: a -> HList as -> HList (a ': as)

hhead :: HList (a ': as) -> a
hhead (Cons h _) = h

-- | Get the tail of an heterogeneous list
htail :: HList (a ': as) -> HList as
htail (Cons _ t) = t

-- | Utility (closed) type family to detect whether a type
--   is contained in a type-level list of types
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 781
type family Contains (elem :: *) (list :: [*]) :: Bool where
  Contains elem '[] = False
  Contains elem (elem ': xs) = True
  Contains elem (x ': xs) = Contains elem xs
#else
type Contains (elem :: *) (list :: [*]) = False
#endif

-- | A resource that:
--
--   * uses some context type @c@ (think database connection)
--   * manages entries of type @a@
--   * (optionally) supports indexing through the @i@ type (a dumb ID, or something like
--     @data FooId = ByToken Token | ByID Integer@). That can be useful when trying to view,
--     update or delete a particular entry, for example.
--   * uses @r@ as the return type for /effectful/ database operations (e.g. adding, updating, deleting entries
--     for example)
--   * can catch exceptions, converting them to some error type
--     @e@ of yours
--   * supports the operations listed in the @ops@ type list. a corresponding
--     (heterogeneous) list of "database functions" is held internally and we
--     ask the compiler to make the types of these functions match with the ones
--     expected for the operations listed at the type-level.
data Resource c a i r e (ops :: [*])
  = Resource { name       :: String    -- ^ Get the name of the 'Resource'
             , context    :: Context c -- ^ Gives the 'Context' attached to this 'Resource'
             , excCatcher :: ExceptionCatcher e -- ^ Hands you the 'ExceptionCatcher' you can
                                                --   'handledWith' with to make your \"database operations\"
                                                --   exception safe.
             , operations :: HList (Ops ops c a i r)
             }

instance Show (Resource c a i r e '[]) where
  show r = name r

instance (Show o, Show (Resource c a i r e ops)) 
      => Show (Resource c a i r e (o ': ops)) where
  show r = 
    show (dropHeadOperation r) ++
    opstring

    where opstring = "\n  - " ++ show (undefined :: o)

-- | Typically, functions that will use our operations will need access
--   to the resource's name and what not, so we need to provide them with
--   the resource. But we obviously also need the \"database function\"
--   associated to our operation. So we provide it too.
--
--   Just give this function a 'Resource' and a function that uses it,
--   most likely to run the handler for an operation,
--   and it'll give your function the right arguments.
withHeadOperation :: Resource c a i r e (o ': ops)
                  -> (Resource c a i r e (o ': ops) -> Operation o c a i r -> b)
                  -> b
withHeadOperation res runop =
  runop res (hhead $ operations res)

-- | Type-safely \"unconsider\" the first operation in the list
--
--   Helpful when performing recursion on the type-level list
--   and the internal list of \"database functions\"
--   simultaneously.
dropHeadOperation :: Resource c a i r e (o ': ops) -> Resource c a i r e ops
dropHeadOperation r = r { operations = operations' }

  where operations' = htail (operations r)

-- | Create an /empty/ resource that doesn't support any operation
--   and catches exceptions using the given 'ExceptionCatcher'.
--   Any operation supported later on can make use of the provided
--   'Context', by simply doing:
--
--   > withContext (context resource) $ \c -> ...
--
--   where @c@ could be a PostgreSQL connection, for example.
mkResource :: String
           -> Context c
           -> ExceptionCatcher e
           -> Resource c a i r e '[]
mkResource n ctx catcher = Resource n ctx catcher Nil

-- | Add an operation to a resource by specifying the \"database function\"
--   that'll actually perform the lookup, update, listing, search and what not.
--
--   We statically enforce that the operation we're adding isn't
--   already supported by the 'Resource', when built with @ghc >= 7.8@.
addOperation :: Contains o ops ~ False
             => Operation o c a i r 
             -> Resource c a i r e ops 
             -> Resource c a i r e (o ': ops)
addOperation opfunc resource =
  resource { operations = Cons opfunc (operations resource) }

-- | Type level 'map'-like function that replaces an operation's tag
--   by the type of the associated \"database function\"
--
--   For example:
--
--   > Ops [Add, List] c a i r
--
--   will result in:
--
--   > [ a -> c -> IO r -- what 'Add' is replaced by
--   > , c -> IO [a]    -- what 'List' is replaced by
--   > ]
--
--   This is useful as we can exactly determine the type of the heterogeneous
--   list that holds the actual \"dtabase functions\" that will perform the
--   operations, using 'Ops'. This among other things enforces a strong
--   correspondance between the type-level list of operations and the
--   (heterogeneous) list of functions held in the 'Resource' we're interested in.
--
--   That means we can't magically convert a 'Resource' into one that supports one more
--   operations without registering a function for it (which /must have/ the right type,
--   or your code won't compile.
type family Ops (ops :: [*]) c a i r :: [*]
type instance Ops (o ': ops) c a i r = Operation o c a i r ': Ops ops c a i r
type instance Ops '[] c a i r = '[]

-- | Map an operation tag @o@ to some combination of the other type
--   parameters.
--
--   For instance, if we look at 'Add', we know that we'll need our
--   \"connection type\" @c@ and a value to add, of type @a@. The result will
--   be of type @IO r@. If we put this all together, we get:
--
--   > type instance Operation Add c a i r = a -> c -> IO r
--
--   Whereas for listing all entries ('ListAll'), we just want some kind
--   of connection @c@ and we get back @[a]@.
--
--   > type instance Operation ListAll c a i r = c -> IO [a]
type family Operation o c a i r :: *

-- | Reversed function application.
--
--   > x & f = f x
(&) :: a -> (a -> b) -> b
x & f = f x
{-# INLINE (&) #-}
