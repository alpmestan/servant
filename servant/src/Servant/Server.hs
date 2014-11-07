{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module lets you implement 'Server's for defined APIs.  You will
-- probably need 'serve' (and look at the 'HasServer' type family), but
-- 'toApplication' and 'route' are rather internals.

module Servant.Server where

import Data.Monoid
import Data.Proxy
import Network.Wai


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

class HasServer layout where
  type Server layout :: *
  route :: Proxy layout -> Server layout -> RoutingApplication
