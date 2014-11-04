{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API.CanonicalType where

import GHC.TypeLits

data APIRoute x where
    Post :: a -> APIRoute (a)
    Get  :: a -> APIRoute (a)
    Put  :: a -> APIRoute (a)
    Delete :: APIRoute Delete

{-
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

data Greet

type TestApi =
       "hello" :> Capture "name" String :> QueryParam "capital" Bool :> Get Greet
  :<|> "greet" :> ReqBody Greet :> Post Greet
  :<|> "delete" :> Capture "greetid" Bool :> Delete
  :<|> "hello" :> Capture "true" Bool :> QueryParam "capital" Bool :> Get Greet
type TestApi2 = "greet" :> ReqBody Greet :> Post Greet
-}
