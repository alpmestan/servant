{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DataKinds #-}
module Servant.Example where

import Control.Applicative
import Data.Aeson
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics
import System.IO.Unsafe
import Web.Scotty

import Servant.Resource

newtype ItemId = ItemId { iId :: Int }
  deriving (Eq, Ord, Num, Show, Parsable, FromJSON, ToJSON)

data Item = Item { itemId :: ItemId, itemName :: String }
  deriving (Eq, Show, Generic)

instance FromJSON Item where
instance ToJSON Item where

type Connection = IORef (Map ItemId Item)

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
