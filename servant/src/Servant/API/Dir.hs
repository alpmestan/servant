{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.Dir where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Proxy
import Data.Text (pack, unpack, intercalate)
import Data.Typeable
import GHC.TypeLits
import Network.HTTP.Types
import Network.Mime
import Network.URI
import Network.Wai
import Servant.API.Sub
import Servant.Client
import Servant.Common.BaseUrl
import Servant.Common.Req
import Servant.Docs
import Servant.Server
import System.Directory
import System.Environment
import System.FilePath

import qualified Data.ByteString.Lazy.Char8 as LB

-- | Serves any file under @$SERVANT_FILES/dirname@, where @$SERVANT_FILES@ is an
--   environment variable you have to set before/when running your program.
data Dir dirname
  deriving Typeable

-- | A "dummy" handler for serving static files.
--
-- > type MySite = StaticDir "photos"   -- serve anything file/dir 'f' in 'photos' under /photos/f
-- >          :<|> File "index.html" -- under /, serve "index.html"
-- >
-- > server :: Server MySite
-- > server = dir :<|> file
dir :: ()
dir = ()

-- | Equivalent to @dirname :> 'Dir' dirname@.
type StaticDir dirname = dirname :> Dir dirname

instance KnownSymbol dirname => HasServer (Dir dirname) where
  type Server (Dir dirname) = ()

  route Proxy () request respond
    | not (null (pathInfo request)) && requestMethod request == methodGet = do
        mfile <- fmap (fmap toFP) $ lookupEnv servantFilesEnv
        case mfile of
          Nothing  -> respond $ failWith NotFound
          Just file -> do
            fileExists <- liftIO $ doesFileExist file
            if fileExists
              then respond . succeedWith $
                     responseFile ok200 mimeHeader file Nothing
              else respond $ failWith NotFound

    | not (null (pathInfo request)) && requestMethod request /= methodGet =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound


    where dirname = symbolVal (Proxy :: Proxy dirname)
          mimeHeader = [("Content-Type", mime)]
          mime = defaultMimeLookup (last $ pathInfo request)
          toFP servantFilesDir =
                servantFilesDir
            </> dirname
            </> unpack (intercalate "/" (pathInfo request))

          servantFilesEnv :: String
          servantFilesEnv = "SERVANT_FILES"

{-
instance KnownSymbol filename => HasClient (File filename) where
  type Client (File filename) = BaseUrl -> EitherT String IO LB.ByteString
  clientWithRoute Proxy req host =
    -- fixme: we don't add "Accept" headers yet
    performRequest' methodGet req 200 host

instance KnownSymbol filename => HasDocs (File filename) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocGET
          action' = action & response.respBody .~ Just ("<Content of $SERVANT_FILES/" <> filename <> ">")
          filename = LB.pack $ symbolVal (Proxy :: Proxy filename)
-}