module Main where

import Options.Applicative

data Options = Options
  { optDeploy :: String,
    optInit :: String
  }
-- data Input
--   = DeployInput
--   | InitInput

opts :: Parser Options
opts =
  Options
    <$> strOption
      ( long "deploy"
          <> metavar "TARGET"
          <> help "Deploy code"
      )
    <*> strOption
      ( long "init"
          <> metavar "TEMPLATE"
          <> help "Codegen starter template"
      )

main :: IO ()
main = greet =<< execParser opts'
  where
    opts' =
      info
        (opts <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )

greet :: Options -> IO ()
greet _ = putStrLn $ "Hello!"
-- greet (Options h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
-- greet _ = return ()

