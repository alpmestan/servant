{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{- |
Module      :  Servant.Context
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

Useful functions and instances for using servant with a PostgreSQL context.

* Use 'contextOfConnInfo' or 'contextOfConnStr' to create 'Context's.
* Use the 'PGResult'-related functions and instances to benefit from the
  'UpdateResponse' instances (for 'Add', 'Delete' and 'Update').
-}
module Servant.Context.PostgreSQL
  ( -- * PostgreSQL 'Context'
    contextOfConnInfo
  , contextOfConnStr
  , -- * 'PGResult' and 'Response' instances
    PGResult
  , resultOfInts
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Monoid
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status
import Servant.Operation
import Servant.Response
import Servant.Context

-- | Create a @'Context' 'Connection'@ from the given
--   'ConnectInfo'
contextOfConnInfo :: ConnectInfo -> Context Connection
contextOfConnInfo ci = mkContext f
  where f act = bracket (connect ci) close act

-- | Create a @'Context' 'Connection'@ from the given
--   connection string.
contextOfConnStr :: ByteString -> Context Connection
contextOfConnStr str = mkContext f
  where f act = bracket (connectPostgreSQL str) close act

-- | Simple wrapper around @postgresql-simple@'s @['Only' 'Int']@
newtype PGResult =
  PGResult { onlyInts :: [Only Int] }
  deriving (Eq, Show)

-- | Create a 'PGResult' by simply wrapping the argument in a /newtype/
resultOfInts :: [Only Int] -> PGResult
resultOfInts = PGResult

-- | Send status 201 if successful, 400 if not along with an error
instance Response (UpdateResponse Add) PGResult where
  toResponse result =
    (UpdateResponse successful m, responseStatus)

    where affected = getSum . foldMap (Sum . fromOnly) $ onlyInts result
          successful = affected > 0
          m = 
            if affected < 1 
              then "Unknown error. please report this to maintainers" 
              else ""
          responseStatus =
            if successful then status201 else status400

-- | Send status 200 if successful, 404 if target not found along with "Not found"
instance Response (UpdateResponse Update) PGResult where
  toResponse result =
    (UpdateResponse successful m, responseStatus)

    where affected = getSum . foldMap (Sum . fromOnly) $ onlyInts result
          successful = affected > 0
          m = if affected < 1 then "Not found" else ""
          responseStatus =
            if successful then status200 else notFound404

-- | Send status 200 if successful, 404 if target not found along with "Not found"
instance Response (UpdateResponse Delete) PGResult where
  toResponse result =
    (UpdateResponse successful m, responseStatus)

    where affected = getSum . foldMap (Sum . fromOnly) $ onlyInts result
          successful = affected > 0
          m = if affected < 1 then "Not found" else ""
          responseStatus =
            if successful then status200 else notFound404

