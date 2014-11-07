{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.CanonicalType where

import Data.Proxy
import Servant.API
import GHC.TypeLits

--------------------------------------------------------------------------
-- Canonicalize
--
-- Combine the individual transformations into a single one.
--------------------------------------------------------------------------

class Canonicalize orig new | orig -> new where
    canonicalize :: orig -> new

instance (Canonicalize a b) => Canonicalize (Proxy a) (Proxy b) where
    canonicalize _ = Proxy
instance (RBLast a b) => Canonicalize a b where
    canonicalize = rblast -- TODO: eventually include others

--------------------------------------------------------------------------
-- Type directed sort
--
-- Adapted from an example by Roman Leshchinskiy [0] to include value-level
-- lock-step logic. We don't have enough information at the value-level to
-- perform the sort, so we need to use the types.
--
-- [0] https://www.haskell.org/haskellwiki/Type_arithmetic#An_Advanced_Example_:_Type-Level_Quicksort
--
--  Quicksort is certainly the wrong idea, though.
--------------------------------------------------------------------------

{-
class Cmp x y c | x y -> c where
    cmp :: Proxy x -> Proxy y -> Ordering
instance (KnownSymbol a, KnownSymbol b, o ~ CmpSymbol a b) => Cmp a b o where
    cmp x y = symbolVal x `compare` symbolVal y
instance (KnownSymbol a) => Cmp a (Capture n t) LT where
    cmp _ _ = LT
instance (KnownSymbol b) => Cmp (Capture n t) b GT where
    cmp _ _ = GT
instance                    Cmp (Capture n t) (Capture n' t') EQ where
    cmp _ _ = EQ
instance (Cmp a' b' o)     => Cmp (a :> a') (a :> b') o where
    cmp (Proxy::Proxy(a :> a'))
        (Proxy::Proxy(b :> b')) = cmp (Proxy::Proxy a') (Proxy::Proxy b')
instance (Cmp a b o)     => Cmp (a :> a') (b :> b') o where
    cmp (Proxy::Proxy(a :> a'))
        (Proxy::Proxy(b :> b')) = cmp (Proxy::Proxy a) (Proxy::Proxy b)
instance (Cmp a b o)     => Cmp a (b :> b') o where
    cmp (Proxy::Proxy a)
        (Proxy::Proxy(b :> b')) = cmp (Proxy::Proxy a) (Proxy::Proxy b)
instance (Cmp a b o)     => Cmp (a :> a') b o where
    cmp (Proxy::Proxy (a :> a'))
        (Proxy::Proxy b) = cmp (Proxy::Proxy a) (Proxy::Proxy b)
instance                    Cmp (Get x) (Get y) EQ where
    cmp _ _ = EQ
-- add queryparams etc.
-- is @cmp@ really necessary (or useful)?


-- put a value into one of three lists according to a pivot element
class Pick c x ls eqs gs ls' eqs' gs' | c x ls eqs gs -> ls' eqs' gs' where
    pick :: Proxy c -> x -> (ls, eqs, gs) -> (ls', eqs', gs')
instance Pick LT x ls eqs gs (x :<|> ls) eqs gs where
    pick _ x (ls, eqs, gs) = (x :<|> ls, eqs, gs)
instance Pick EQ x ls eqs gs ls (x :<|> eqs) gs where
    pick _ x (ls, eqs, gs) = (ls, x :<|> eqs, gs)
instance Pick GT x ls eqs gs ls eqs (x :<|> gs) where
    pick _ x (ls, eqs, gs) = (ls, eqs, x :<|> gs)

data Nil = Nil

-- split a list into three parts according to a pivot element
class Split n xs ls eqs gs | n xs -> ls eqs gs where
    split :: n -> xs -> (ls, eqs, gs)
instance Split n Nil Nil Nil Nil where
    split _ Nil = (Nil, Nil, Nil)
instance ( Split n xs ls' eqs' gs'
         , Cmp x n c
         , Pick c x ls' eqs' gs' ls eqs gs
         ) => Split n (x :<|> xs) ls eqs gs where
    split n (x :<|> xs) = (ls, eqs, gs)
      where (ls', eqs', gs') = split n xs
            (ls , eqs , gs ) = pick (Proxy::Proxy c) x (ls', eqs', gs')

-- zs = xs ++ ys
class App xs ys zs | xs ys -> zs where
    app :: xs -> ys -> zs
instance App Nil ys ys where
    app _ ys = ys
instance App xs ys zs => App (x :<|> xs) ys (x :<|> zs) where
    app (x :<|> xs) ys = x :<|> app xs ys

class App' xs n ys zs | xs n ys -> zs where
    app' :: xs -> n -> ys -> zs
instance App' Nil n ys (n :<|> ys) where
    app' _ n ys = n :<|> ys
instance (App' xs n ys zs) => App' (x :<|> xs) n ys (x :<|> zs) where
    app' (x :<|> xs) n ys = x :<|> app' xs n ys

-- quicksort
class QSort xs ys | xs -> ys where
    qsort :: xs -> ys
instance QSort Nil Nil where
    qsort _ = Nil
instance ( Split x xs ls eqs gs
         , QSort ls ls'
         , QSort gs gs'
         , App eqs gs' geqs
         , App' ls' x geqs ys
         ) => QSort (x :<|> xs) ys where
    qsort (x :<|> xs) = app' ls' x (app eqs gs')
      where (ls, eqs, gs) = split x xs
            ls' = qsort ls
            gs' = qsort gs

-}
--------------------------------------------------------------------------
-- ReqBody last
--------------------------------------------------------------------------

class RBLast orig new | orig -> new where
    rblast :: orig -> new
instance ( RBLast xs ys
         , SubApp (ReqBody x) ys zs
         ) => RBLast (ReqBody x :> xs) zs where
    rblast (_ :> xs) = subapp (undefined::ReqBody x) $ rblast xs
instance ( RBLast xs ys
         ) => RBLast (a :> xs) (a :> ys) where
    rblast (x :> xs) = x :> rblast xs
instance (a ~ b) => RBLast a b where
    rblast = id
instance ( RBLast xs ys
         , RBLast xs' ys'
         ) => RBLast (xs :<|> xs') (ys :<|> ys') where
    rblast (xs :<|> xs') = rblast xs :<|> rblast xs'

-- subapp x xs ==> xs + [x]
class SubApp x xs xxs | x xs -> xxs where
    subapp :: x -> xs -> xxs
instance (SubApp a bs cs) => SubApp a (b :> bs) (b :> cs) where
    subapp a (b :> bs) = b :> subapp a bs
instance SubApp a (Get x) (Get x :> a) where
    subapp a _ = (Proxy :: Proxy (Get x)) :> a


--------------------------------------------------------------------------
-- Type families for testing
--------------------------------------------------------------------------
type family CommonInitial x where
    CommonInitial (a :> b :<|> a :> c) = a :> (CommonInitial b :<|> c)
    CommonInitial a = a

type family Sort x where
    Sort (a :> b :<|> a :> d) = a :> (Sort (a :<|> d))
    Sort (a :> b :<|> c :> d) = Sort' (CmpSymbol a c) (a :> b) (c :> d)
    Sort (a :<|> b) = a :<|> b

type family Sort' (o::Ordering) a b where
    Sort' LT a b = a :<|> b
    Sort' EQ a b = a :<|> b
    Sort' GT a b = b :<|> a

type family ReqBodyLast' x where
    ReqBodyLast' (ReqBody x :> Post y) = Post y :> ReqBody x
    ReqBodyLast' (ReqBody x :> Put y)  = Put y :> ReqBody x
    ReqBodyLast' (a :> b) = a :> ReqBodyLast' b
    ReqBodyLast' a = a

type family ReqBodyLast x where
    ReqBodyLast (a :<|> b) = ReqBodyLast' a :<|> ReqBodyLast b
    ReqBodyLast a = ReqBodyLast' a

--------------------------------------------------------------------------
-- Playground
--------------------------------------------------------------------------

type TestApi =
       "d" :> Get Int
  :<|> "b" :> Get Int
  :<|> "a" :> Get Int
  :<|> "c" :> Get Int



