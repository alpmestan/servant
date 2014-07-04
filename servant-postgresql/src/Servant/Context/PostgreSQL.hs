{- |
Module      :  Servant.Context.PostgreSQL
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

Useful functions and instances for using servant with a PostgreSQL database.

* Use 'contextOfConnInfo' or 'contextOfConnStr' to create a PostgreSQL 'Context's.
* If you want connection-pooling, use 'pooledContextOfConnInfo' and
  'pooledContextOfConnStr'.
-}
module Servant.Context.PostgreSQL
  ( -- * PostgreSQL 'Context'
    contextOfConnInfo
  , contextOfConnStr
  , -- * PostgreSQL 'Context' with connection pooling
    pooledContextOfConnInfo
  , pooledContextOfConnStr

  ) where

import Control.Exception
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple
import Servant.Context
import Servant.Context.Pool

-- | Create a @'Context' 'Connection'@ from the given
--   'ConnectInfo'.
--
--   This means a new connection will be fired whenever you
--   perform a database operation. If you want to avoid that,
--   see the /pooledContextOfXXX/ functions.
contextOfConnInfo :: ConnectInfo -> Context Connection
contextOfConnInfo ci = mkContext f
  where f act = bracket (connect ci) close act

-- | Create a @'Context' 'Connection'@ from the given
--   connection string.
--
--   This means a new connection will be fired whenever you
--   perform a database operation. If you want to avoid that,
--   see the /pooledContextOfXXX/ functions.
contextOfConnStr :: ByteString -> Context Connection
contextOfConnStr str = mkContext f
  where f act = bracket (connectPostgreSQL str) close act

-- | Create a 'Context' that'll use a 'Pool' of
--   PostgreSQL 'Connection's internally, from
--   a 'ConnectInfo' value.
pooledContextOfConnInfo :: Int             -- ^ Number of stripes (sub-pools). /Minimum: 1/
                        -> NominalDiffTime -- ^ amount of time during which an unused
                                           --   'Connection' is kept open
                        -> Int             -- ^ Maximum number of resources to keep open
                                           --   per stripe. /Minimum: 1/
                        -> ConnectInfo     -- ^ connection information
                        -> IO (Context Connection)
pooledContextOfConnInfo nstripes idleDuration maxOpen ci =
  pooledContext (connect ci)
                close
                nstripes
                idleDuration
                maxOpen

-- | Create a 'Context' that'll use a 'Pool' of
--   PostgreSQL 'Connection's internally, from
--   a connection string (a 'ByteString').
pooledContextOfConnStr :: Int             -- ^ Number of stripes (sub-pools). /Minimum: 1/
                       -> NominalDiffTime -- ^ amount of time during which an unused
                                          --   'Connection' is kept open
                       -> Int             -- ^ Maximum number of resources to keep open
                                          --   per stripe. /Minimum: 1/
                       -> ByteString      -- ^ connection string
                       -> IO (Context Connection)
pooledContextOfConnStr nstripes idleDuration maxOpen connstr =
  pooledContext (connectPostgreSQL connstr)
                close
                nstripes
                idleDuration
                maxOpen

{-
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
-}
