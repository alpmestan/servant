{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, killThread)
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.URI
import Network.Wai
import Network.Wai.Handler.Warp

import Servant.API
import Servant.Client
import Servant.Server

-- * Example

data Greet = Greet { msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi = 
       "hello" :> Capture "name" Text :> GetParam "capital" Bool :> Get Greet
  :<|> "greet" :> RQBody Greet :> Post Greet

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers
server :: Server TestApi
server = hello :<|> greet

  where hello name Nothing = hello name (Just False)
        hello name (Just False) = return . Greet $ "Hello, " <> name
        hello name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        greet = return

-- Client-side query functions
clientApi :: Client TestApi
clientApi = client testApi

getGreet :: Text -> Maybe Bool -> URI -> EitherT String IO Greet
postGreet :: Greet -> URI -> EitherT String IO Greet
getGreet :<|> postGreet = clientApi

-- Turn the server into a WAI app
test :: Application
test = serve testApi server

-- Run the server
runTestServer :: Port -> IO ()
runTestServer port = run port test

-- Run some queries against the server
main :: IO ()
main = do
  tid <- forkIO $ runTestServer 8001
  let Just uri = parseURI "http://localhost:8001"
  print =<< runEitherT (getGreet "alp" (Just True) uri)
  print =<< runEitherT (getGreet "alp" (Just False) uri)
  let g = Greet "yo"
  print =<< runEitherT (postGreet g uri)
  killThread tid
