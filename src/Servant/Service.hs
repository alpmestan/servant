{-# LANGUAGE DataKinds, 
             FlexibleContexts,
             FlexibleInstances,
             FunctionalDependencies,
             KindSignatures,
             TypeOperators,
             ScopedTypeVariables,
             MultiParamTypeClasses #-}

module Servant.Service where

import Control.Applicative
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe
import Data.Proxy
import Data.Reflection
import Data.Text.Lazy (pack)
import Network.HTTP.Types.Status
import Web.Scotty.Trans

import Servant.Context
import Servant.Resource
import Servant.Response

handlerFor :: ( MonadIO m
              , ScottyError e
              , Parsable i
              , FromJSON a
              , Reifies o Op
              , Response UpdateResponse r
              , ToJSON a
              , Context c
              , Functor m) 
           => proxy o
           -> Resource c a i r (o ': ops)
           -> ScottyT e m ()
handlerFor op resource = handlerFor' (reflect op) resource

handlerFor' :: ( MonadIO m
               , ScottyError e
               , Parsable i
               , FromJSON a
               , Response UpdateResponse r
               , ToJSON a
               , Context c
               , Functor m
               ) 
            => Op 
            -> Resource c a i r ops 
            -> ScottyT e m ()
handlerFor' Add r =
  post (capture $ resourceRoute r) $ do
    val <- jsonData
    let addF = fromJust (adder r)
    (resp :: UpdateResponse, statuscode)
      <- toResponse <$> liftIO (withContext $ \c -> addF c val)
    status statuscode
    json resp

handlerFor' Delete r =
  delete (capture $ resourceRoute r ++ "/:" ++ keyName r) $ do
    key <- param (pack $ keyName r)
    let deleteF = fromJust (deleter r)
    (resp :: UpdateResponse, statuscode)
      <- toResponse <$> liftIO (withContext $ \c -> deleteF c key)
    status statuscode
    json resp

handlerFor' List r =
  get (capture $ resourceRoute r) $ do
    let listF = fromJust (lister r)
    elems <- liftIO (withContext listF)
    status status200
    json elems

handlerFor' Update r =
  put (capture $ resourceRoute r ++ "/:" ++ keyName r) $ do
    key <- param (pack $ keyName r)
    newVal <- jsonData
    let updateF = fromJust (updater r)
    (resp :: UpdateResponse, statuscode)
      <- toResponse <$> liftIO (withContext $ \c -> updateF c key newVal)
    status statuscode
    json resp

handlerFor' View r =
  get (capture $ resourceRoute r ++ "/:" ++ keyName r) $ do
    key <- param (pack $ keyName r)
    let viewF = fromJust (viewer r)
    lookupResp <- liftIO (withContext $ \c -> viewF c key)
    let (resp, statuscode) = maybe (NotFound, status404)  
                                   (\x -> (Found x, status200)) 
                                   lookupResp
    status statuscode
    json resp

class ScottyResource r where
  declareResource :: (Functor m, MonadIO m, ScottyError e) 
                  => r -> ScottyT e m ()

instance ScottyResource (Resource c a i r '[]) where
  declareResource _ = return ()

instance ( ScottyResource (Resource c a i r ops)
         , FromJSON a, ToJSON a, Parsable i
         , Response UpdateResponse r
         , Context c
         , Reifies o Op
         ) 
      => ScottyResource (Resource c a i r (o ': ops)) where
  declareResource r = do
    handlerFor proxy r
    declareResource (dropHeadOp r)

    where proxy = Proxy :: Proxy o
