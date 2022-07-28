{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Lib
  ( startApp
  , mkApp
  , CompileRequest (..)
  , Config (..)
  )
where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, fieldLabelModifier, parseJSON, genericParseJSON, toJSON, genericToJSON)
import Control.Monad.Except (ExceptT)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Char (toLower)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Katip (Environment (..), KatipT, initLogEnv, runKatipT)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (corsRequestHeaders, simpleCorsResourcePolicy, cors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import System.IO (hClose)
import System.IO.Temp
import System.Process
import System.Exit (ExitCode (ExitSuccess, ExitFailure))

-- Swagger
import Data.Proxy (Proxy (..))
import Servant.Swagger
import Data.Swagger.Schema (toSchema, ToSchema)
import Data.Swagger.ParamSchema (ToParamSchema)
import Servant.Swagger.UI

type API = "compile" :> ReqBody '[JSON] CompileRequest :> Post '[JSON] Build
type SwaggeredAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"
                  :<|> API

newtype Build = Build Text deriving stock (Show, Generic)

instance ToJSON Build where
  toJSON = genericToJSON defaultOptions

instance ToSchema CompileRequest
instance ToSchema Build
instance ToParamSchema Build

data Config = Config
  { cLigoPath :: FilePath
  , cPort :: Int
  , cVerbose :: Bool
  }

newtype Source = Source {unSource :: Text} deriving stock (Eq, Show, Ord, Generic)

instance ToSchema Source
instance ToSchema File

instance FromJSON Source where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON Source where
  toJSON = genericToJSON defaultOptions

newtype File = File Text deriving stock (Eq, Show, Ord, Generic)

instance FromJSON File where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON File where
  toJSON = genericToJSON defaultOptions

data CompileRequest = CompileRequest
  { rFileExtension :: Text
  , rSources :: [(File, Source)]
  , rMain :: File
  } deriving stock (Eq, Show, Ord, Generic)

prepareField :: Int -> String -> String
prepareField n = lowercaseInitial . drop n

lowercaseInitial :: String -> String
lowercaseInitial [] = []
lowercaseInitial (c:s) = toLower c : s

instance FromJSON CompileRequest where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prepareField 1}

instance ToJSON CompileRequest where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = prepareField 1}

type WebIDEM = KatipT (ReaderT Config (ExceptT ServerError IO))

startApp :: Config -> IO ()
startApp config = run (cPort config) (mkApp config)

-- | Allow Content-Type header with values other then allowed by simpleCors.
corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"] }

mkApp :: Config -> Application
mkApp config = maybeLogRequests . corsWithContentType $ serve (Proxy :: Proxy SwaggeredAPI) server
  where
    api :: Proxy API
    api = Proxy

    server :: Server SwaggeredAPI
    server = swaggerSchemaUIServer (toSwagger (Proxy :: Proxy API))
          :<|> (hoistServer api hoist compile)

    hoist :: WebIDEM a -> Handler a
    hoist x = Handler $ do
      logEnv <- liftIO $ initLogEnv "ligo-webide" (Environment "devel")
      runReaderT (runKatipT logEnv x) config

    maybeLogRequests :: Middleware
    maybeLogRequests =
      if cVerbose config
      then logStdoutDev
      else id

compile :: CompileRequest -> WebIDEM Build
compile request =
  let filename = "input." ++ Text.unpack (rFileExtension request)
   in withSystemTempFile filename $ \fp handle -> do
        liftIO $ hClose handle
        liftIO $ Text.writeFile fp (unSource (snd (head (rSources request))))
        ligoPath <- lift (asks cLigoPath)
        (ec, out, err) <- liftIO $
          readProcessWithExitCode ligoPath ["compile", "contract", fp] ""
        case ec of
          ExitSuccess -> pure (Build $ Text.pack out)
          ExitFailure _ -> pure (Build $ Text.pack err)
