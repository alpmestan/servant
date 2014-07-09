{-# LANGUAGE FlexibleInstances,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             OverloadedStrings #-}
{- |
Module      :  Servant.PostgreSQL.Prelude
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

An helpful wrapper around 'Int64' that you can tie to
the standard response types in "Servant.Response.Prelude" with
the instances defined in this module.
-}
module Servant.PostgreSQL.Prelude
  ( PGResult
  , toPGResult
  , module Servant.Context.PostgreSQL
  ) where

import Data.Int
import Servant.Context.PostgreSQL
import Servant.Prelude
import Servant.Response.Prelude

-- | A wrapper around 'Int64', which is what
--   PG hands us back when running 
--   'Database.PostgreSQL.Simple.execute'.
--
--   The @o@ type parameter lets us tag
--   the result with the operation that
--   we're running. This lets us turn
--   results into a proper response
--   (response body + status) differently
--   for 'Add' and 'Update' for example.
newtype PGResult o = PGResult { pgres :: Int64 }
  deriving (Eq, Ord, Num, Show)

-- | Convert an 'Int64' to a 'PGResult'
toPGResult :: IO Int64 -> IO (PGResult o)
toPGResult = fmap PGResult

-- | If the 'Int64' is smaller than 1, status 400 and a
--   suitable error message. Status 201 and empty message otherwise.
instance Response (UpdateResponse Add) (PGResult Add) where
  toResponse n = (response, statuscode)

    where response   = UpdateResponse successful msg
          successful = n > 0
          msg        = if successful then "" else "no entry added"
          statuscode = if successful then status201 else status400

-- | If the 'Int64' is smaller than 1, status 400 and a
--   suitable error message. Status200 and empty message otherwise.
instance Response (UpdateResponse Delete) (PGResult Delete) where
  toResponse n = (response, statuscode)

    where response   = UpdateResponse successful msg
          successful = n > 0
          msg        = if successful then "" else "couldn't delete: target entry doesn't exist"
          statuscode = if successful then status200 else status400

-- | If the 'Int64' is smaller than 1, status 400 and a
--   suitable error message. Status200 and empty message otherwise.
instance Response (UpdateResponse Update) (PGResult Update) where
  toResponse n = (response, statuscode)

    where response   = UpdateResponse successful msg
          successful = n > 0
          msg        = if successful then "" else "couldn't update: target entry doesn't exist"
          statuscode = if successful then status200 else status400