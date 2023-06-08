module Main where

import Options.Applicative

data Input
  = DeployInput String
  | InitInput String

deployInput :: Parser Input
deployInput =
  DeployInput
    <$> strOption
      ( long "deploy"
          <> short 'd'
          <> metavar "FILENAME"
          <> help "Deploy code"
      )

initInput :: Parser Input
initInput =
  InitInput
    <$> strOption
      ( long "init"
          <> short 'i'
          <> metavar "TEMPLATE"
          <> help "Codegen starter template"
      )

input :: Parser Input
input = deployInput <|> initInput

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (input <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )

run :: Input -> IO ()
run = \case
  DeployInput fileName -> putStrLn $ "Deploy! " ++ fileName
  InitInput template -> putStrLn $ "Init! " ++ template
