{-# LANGUAGE MultiParamTypeClasses,
             GeneralizedNewtypeDeriving,
             DeriveGeneric,
             OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Aeson
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics
import Network.HTTP.Types.Status
import System.IO.Unsafe
import Web.Scotty

import Servant.Context
import Servant.Resource
import Servant.Response
import Servant.Operation
import Servant.Service

newtype ItemId = ItemId { iId :: Int }
  deriving (Eq, Ord, Num, Show, Parsable, FromJSON, ToJSON)

data Item = Item { itemId :: ItemId, itemName :: String }
  deriving (Eq, Show, Generic)

instance FromJSON Item where
instance ToJSON Item where

newtype Connection = Connection (IORef (Map ItemId Item))

withConnection :: (Connection -> IO r) -> IO r
withConnection f = f db

db :: Connection
db = Connection . unsafePerformIO $ newIORef M.empty
{-# NOINLINE db #-}

itemAdd :: Connection -> Item -> IO Bool
itemAdd (Connection ref) item = 
  atomicModifyIORef' ref f

  where f m = (M.insert (itemId item) item m, True) 

itemView :: Connection -> ItemId -> IO (Maybe Item)
itemView (Connection ref) itemid = M.lookup itemid <$> readIORef ref

itemDelete :: Connection -> ItemId -> IO Bool
itemDelete (Connection ref) itemid = atomicModifyIORef' ref f

  where f m = (M.delete itemid m, True)

itemUpdate :: Connection -> ItemId -> Item -> IO Bool
itemUpdate (Connection ref) itemid item = atomicModifyIORef' ref f

  where f m = (M.insert itemid item m, True)

itemList :: Connection -> IO [Item]
itemList (Connection ref) = M.elems <$> readIORef ref

instance Response UpdateResponse Bool where
  toResponse False = (UpdateResponse False "Not found", status404)
  toResponse True  = (UpdateResponse True "", status200)

{-
itemResource :: Resource IO
                         Text
                         Connection
                         Item
                         ItemId
                         Bool
                         '[List, View, Delete, Update, Add]

but this is all infered.
-}
itemResource = mkResourceAt "/items" "itemid" (mkContext withConnection)
             & addWith itemAdd
             & updateWith itemUpdate
             & deleteWith itemDelete
             & viewWith itemView
             & listWith itemList

itemService :: ScottyM ()
itemService = runResource itemResource

main :: IO ()
main = do
  print itemResource
  scotty 3000 itemService
