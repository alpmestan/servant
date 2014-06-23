{-# LANGUAGE DeriveGeneric,
             GeneralizedNewtypeDeriving,
             DataKinds,
             FlexibleInstances,
             MultiParamTypeClasses,
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
import Servant.Service

newtype ItemId = ItemId { iId :: Int }
  deriving (Eq, Ord, Num, Show, Parsable, FromJSON, ToJSON)

data Item = Item { itemId :: ItemId, itemName :: String }
  deriving (Eq, Show, Generic)

instance FromJSON Item where
instance ToJSON Item where

type Connection = IORef (Map ItemId Item)

db :: Connection
db = unsafePerformIO $ newIORef M.empty
{-# NOINLINE db #-}

instance Context Connection where
  withContext act = act db

itemAdd :: Connection -> Item -> IO Bool
itemAdd ref item = atomicModifyIORef' ref f

  where f m = (M.insert (itemId item) item m, True) 

itemView :: Connection -> ItemId -> IO (Maybe Item)
itemView ref itemid = M.lookup itemid <$> readIORef ref

itemDelete :: Connection -> ItemId -> IO Bool
itemDelete ref itemid = atomicModifyIORef' ref f

  where f m = (M.delete itemid m, True)

itemUpdate :: Connection -> ItemId -> Item -> IO Bool
itemUpdate ref itemid item = atomicModifyIORef' ref f

  where f m = (M.insert itemid item m, True)

itemList :: Connection -> IO [Item]
itemList ref = M.elems <$> readIORef ref

instance Response UpdateResponse Bool where
  toResponse False = (UpdateResponse False "Not found", status404)
  toResponse True  = (UpdateResponse True "", status200)

itemResource :: Resource Connection -- resource acquired through an IORef
                         Item       -- containing 'Item's
                         ItemId     -- indexed by 'ItemId's
                         Bool       -- update operations return 'Bool'
                         ['List, 'View, 'Delete, 'Update, 'Add] -- supported operations
itemResource = mkResourceAt "/items" "itemid"
             & addWith itemAdd
             & updateWith itemUpdate
             & deleteWith itemDelete
             & viewWith itemView
             & listWith itemList

itemService :: ScottyM ()
itemService = declareResource itemResource

main :: IO ()
main = scotty 3000 itemService 