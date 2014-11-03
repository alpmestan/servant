{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | This module lets you implement 'Server's for defined APIs.  You will
-- probably need 'serve' (and look at the 'HasServer' type family), but
-- 'toApplication' and 'route' are rather internals.

module Servant.Server where

import Servant.API.ContTypes
import Control.Monad.Cont
import Control.Applicative
import Data.Monoid
import Data.Proxy
import Data.Void
import Network.HTTP.Types
import Network.Wai

-- * Implementing Servers

-- | 'serve' allows you to implement an API and produce a wai 'Application'.
serve :: HasServer layout (IsRouteMismatchH Void) => Proxy layout -> Server layout -> Application
serve p server = toApplication . runContApp $ route p server

toApplication :: RoutingApplication -> Application
toApplication ra request respond = do
  ra request (routingRespond . routeResult)
 where
  routingRespond :: Either RouteMismatch Response -> IO ResponseReceived
  routingRespond (Left NotFound) =
    respond $ responseLBS notFound404 [] "not found"
  routingRespond (Left WrongMethod) =
    respond $ responseLBS methodNotAllowed405 [] "method not allowed"
  routingRespond (Left InvalidBody) =
    respond $ responseLBS badRequest400 [] "Invalid JSON in request body"
  routingRespond (Right response) =
    respond response

runContApp :: RoutingApplicationCont (IsRouteMismatchH Void) -> RoutingApplication
runContApp ra request respond = runContT (ra request respond) $ \x -> case x of
  IsMismatch -> respond $ failWith NotFound
  PartialMismatch k -> runContT k absurd
  _ -> respond $ failWith WrongMethod

-- * Route mismatch
data RouteMismatch =
    NotFound    -- ^ the usual "not found" error
  | WrongMethod -- ^ a more informative "you just got the HTTP method wrong" error
  | InvalidBody -- ^ an even more informative "your json request body wasn't valid" error
  deriving (Eq, Show)

-- |
-- @
-- > mempty = NotFound
-- >
-- > NotFound    `mappend`           x = x
-- > WrongMethod `mappend` InvalidBody = InvalidBody
-- > WrongMethod `mappend`           _ = WrongMethod
-- > InvalidBody `mappend`           _ = InvalidBody
-- @
instance Monoid RouteMismatch where
  mempty = NotFound

  NotFound    `mappend`           x = x
  WrongMethod `mappend` InvalidBody = InvalidBody
  WrongMethod `mappend`           _ = WrongMethod
  InvalidBody `mappend`           _ = InvalidBody

-- | A wrapper around @'Either' 'RouteMismatch' a@.
newtype RouteResult a =
  RR { routeResult :: Either RouteMismatch a }
  deriving (Eq, Show)

failWith :: RouteMismatch -> RouteResult a
failWith = RR . Left

succeedWith :: a -> RouteResult a
succeedWith = RR . Right

isMismatch :: RouteResult a -> Bool
isMismatch (RR (Left _)) = True
isMismatch _             = False

-- | If we get a `Right`, it has precedence over everything else.
--
-- This in particular means that if we could get several 'Right's,
-- only the first we encounter would be taken into account.
instance Monoid (RouteResult a) where
  mempty = RR $ Left mempty

  RR (Left x)  `mappend` RR (Left y)  = RR $ Left (x <> y)
  RR (Left _)  `mappend` RR (Right y) = RR $ Right y
  r            `mappend` _            = r

type RoutingApplication =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived

type RoutingApplicationCont a =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> IO ResponseReceived) -> ContT ResponseReceived IO a

class HasServer layout a where
  type Server layout :: *
  route :: Proxy layout -> Server layout -> RoutingApplicationCont a
