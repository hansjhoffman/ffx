module Main where

-- import Control.Monad (join)
import Options.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Opts

-- data Options = Options
--   { optCommand :: !Command
--   }
--
-- data Command
--   = Deploy TargetFile
--   | Init String
--   deriving (Eq, Show)
--
-- type TargetFile = String

data Input
  = DeployInput String
  | InitInput String

deployInput :: Opts.Parser Input
deployInput =
  DeployInput
    <$> Opts.strOption
      ( Opts.long "deploy"
          <> Opts.short 'd'
          <> Opts.metavar "FILENAME"
          <> Opts.help "Deploy code"
      )

initInput :: Opts.Parser Input
initInput =
  InitInput
    <$> Opts.strOption
      ( Opts.long "init"
          <> Opts.short 'i'
          <> Opts.metavar "TEMPLATE"
          <> Opts.help "Codegen starter template"
      )

input :: Opts.Parser Input
input = deployInput <|> initInput

main :: IO ()
main = run =<< Opts.execParser opts
  where
    opts =
      Opts.info
        (input <**> Opts.helper)
        ( Opts.fullDesc
            <> Opts.progDesc "Print a greeting for TARGET"
            <> Opts.header "hello - a test for optparse-applicative"
        )

run :: Input -> IO ()
run = \case
  DeployInput fileName -> putStrLn $ "Deploy! " ++ fileName
  InitInput template -> putStrLn $ "Init! " ++ template
