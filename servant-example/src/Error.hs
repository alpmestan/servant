{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Error where

import Data.Aeson
import Data.Monoid
import Data.Text (Text, pack)
import Data.Text.Lazy (fromStrict)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import GHC.Generics
import Servant.Error
import Web.Scotty.Trans

newtype ServiceError =
	ServiceError { msg :: Text
               } deriving (Eq, Show, Generic)

instance FromJSON ServiceError where
instance ToJSON ServiceError where

instance ScottyError ServiceError where
  stringError s = ServiceError (pack s)

  showError (ServiceError err) = fromStrict err

sqlerrorCatcher :: ExceptionCatcher ServiceError
sqlerrorCatcher = catchAnd convertError
  where convertError sqlerr =
          ServiceError $
            "sql error: " <> decodeUtf8 (sqlErrorMsg sqlerr)

violationsCatcher :: ExceptionCatcher ServiceError
violationsCatcher = catchAnd (ServiceError . cvToText)
  where
    cvToText :: ConstraintViolation -> Text
    cvToText (NotNullViolation field) =
      "field '" <> decodeUtf8 field <> "' shouldn't be NULL"

    cvToText (ForeignKeyViolation table constraint) =
      "foreign key constraint '" <> decodeUtf8 constraint <>
      "' on table '" <> decodeUtf8 table <> "' violated"

    cvToText (UniqueViolation constraint) = 
      "unique constraint '" <> decodeUtf8 constraint <> "' violated"

    cvToText (CheckViolation table constraint) =
      "check of constraint '" <> decodeUtf8 constraint <>
      "' on table '" <> decodeUtf8 table <> "' violated"
