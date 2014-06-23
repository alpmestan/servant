module Servant.Context where

class Context c where
	withContext :: (c -> IO r) -> IO r
