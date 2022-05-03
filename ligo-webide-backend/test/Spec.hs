{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson qualified as Aeson
import Data.Text.IO qualified as Text
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Lib (mkApp, Config (..))
import Network.HTTP.Types.Method (methodPost)
import Network.Wai.Test (SResponse, simpleBody)
import System.Environment (lookupEnv)
import Test.Hspec
import Test.Hspec.Expectations (shouldBe)
import Test.Hspec.Wai (WaiSession, request, with)
import Test.Hspec.Wai.JSON

main :: IO ()
main = do
  ligoPath <- fromMaybe (error "need to set LIGO_PATH") 
           <$> lookupEnv "LIGO_PATH"
  hspec (spec (Config ligoPath))

post :: BS.ByteString -> LBS.ByteString -> WaiSession () SResponse
post path =
  request
    methodPost
    path
    [ ("Accept", "application/json"),
      ("Content-type", "application/json")
    ]

spec :: Config -> Spec
spec config = with (return (mkApp config)) $ do
  describe "POST /compile" $ do
    it "compiles unit test correctly" $ do
      input <- liftIO $ Text.readFile "test/input.mligo"
      response <- post "/compile" (Aeson.encode input)
      expected <- liftIO $ Text.readFile "test/output.tz"
      liftIO $
        case Aeson.decode (simpleBody response) of
          Nothing -> expectationFailure "could not decode response"
          Just actual -> actual `shouldBe` expected
