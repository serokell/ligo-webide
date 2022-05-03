module Main where

import Control.Monad (join)
import Lib
import Options.Applicative

main :: IO ()
main =
  join . customExecParser (prefs showHelpOnError) $
    info
      (helper <*> parser)
      ( fullDesc
          <> header "LIGO WebIDE backend"
          <> progDesc "compile LIGO contracts to Tezos"
      )
  where
    parser :: Parser (IO ())
    parser =
      startApp . Config
        <$> strOption
          ( long "ligo-path"
              <> short 'l'
              <> metavar "STRING"
              <> help "string parameter"
          )
