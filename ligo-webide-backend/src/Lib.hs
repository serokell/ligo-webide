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

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Katip (Environment (..), KatipT, initLogEnv, runKatipT)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO (hClose)
import System.IO.Temp
import System.Process
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import qualified GHC.IO.Handle as Text

type API = "compile" :> ReqBody '[JSON] Text :> Post '[JSON] Text

newtype Config = Config { cLigoPath :: FilePath }

type WebIDEM = KatipT (ReaderT Config (ExceptT ServerError IO))

ligoPath :: FilePath
ligoPath = "/home/pgujjula/.local/bin/ligo"

startApp :: Config -> IO ()
startApp config = run 8080 (mkApp config)

mkApp :: Config -> Application
mkApp config = serve api server
  where
    api :: Proxy API
    api = Proxy

    server :: ServerT API Handler
    server = hoistServer api hoist compile

    hoist :: WebIDEM a -> Handler a
    hoist x = Handler $ do
      logEnv <- liftIO $ initLogEnv "ligo-webide" (Environment "devel")
      runReaderT (runKatipT logEnv x) config

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
