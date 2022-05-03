{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp
  , mkApp
  , Config (..)
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
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

type API = "compile" :> ReqBody '[JSON] Text :> Post '[JSON] Text

data Config = Config
  { cLigoPath :: FilePath
  , cPort :: Int
  , cVerbose :: Bool
  }

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
mkApp config = maybeLogRequests . corsWithContentType $ serve api server
  where
    api :: Proxy API
    api = Proxy

    server :: ServerT API Handler
    server = hoistServer api hoist compile

    hoist :: WebIDEM a -> Handler a
    hoist x = Handler $ do
      logEnv <- liftIO $ initLogEnv "ligo-webide" (Environment "devel")
      runReaderT (runKatipT logEnv x) config

    maybeLogRequests :: Middleware
    maybeLogRequests =
      if cVerbose config
      then logStdoutDev
      else id

compile :: Text -> WebIDEM Text
compile source = withSystemTempFile "input.mligo" $ \fp handle -> do
  liftIO $ hClose handle
  liftIO $ Text.writeFile fp source
  ligoPath <- lift (asks cLigoPath)
  (ec, out, err) <- liftIO $
    readProcessWithExitCode ligoPath ["compile", "contract", fp] ""
  case ec of
    ExitSuccess -> pure (Text.pack out)
    ExitFailure _ -> pure (Text.pack err)
