module Main (main) where

import Api.Id (EnvironmentId (..))
import Data.Version qualified as V
import Options.Applicative qualified as Opt
import Paths_ffx qualified as Meta
import RIO
import RIO.Process
import RIO.Text qualified as T
import Run
import System.Environment (getEnv)
import Types

main :: IO ()
main = do
  opts <- Opt.customExecParser (Opt.prefs Opt.showHelpOnEmpty) optsParser
  logOpts <- logOptionsHandle stderr $ aoDebug opts
  processCtx <- mkDefaultProcessContext
  flatfileEnvId <- getEnv "FFX_ENV_ID"
  flatfileSecretKey <- getEnv "FFX_SECRET_KEY"
  withLogFunc logOpts $ \logFn ->
    let app =
          App
            { appFlatfileEnvId = EnvironmentId $ T.pack flatfileEnvId,
              appFlatfileSecretKey = T.pack flatfileSecretKey,
              appLogFn = logFn,
              appName = "ffx",
              appOptions = opts,
              appProcessContext = processCtx
            }
     in runRIO app run

optsParser :: Opt.ParserInfo AppOptions
optsParser =
  Opt.info (Opt.helper <*> versionParser <*> programOptions) $
    Opt.fullDesc
      <> Opt.header "Flatfile 'X' CLI"
      <> Opt.progDesc "Create a starter project and publish your code."
      <> Opt.footer "For more information on ffx, please visit https://foobar.com"

versionParser :: Opt.Parser (a -> a)
versionParser =
  Opt.infoOption
    (prettyVersion Meta.version)
    (Opt.long "version" <> Opt.help "Show version")
  where
    prettyVersion :: V.Version -> [Char]
    prettyVersion = (++) "ffx v" . V.showVersion

programOptions :: Opt.Parser AppOptions
programOptions =
  AppOptions
    <$> debugParser
    <*> Opt.hsubparser (initCmd <> publishCmd)

debugParser :: Opt.Parser Bool
debugParser =
  Opt.switch $
    Opt.long "debug"
      <> Opt.short 'd'
      <> Opt.help "Output information useful for debugging"

initCmd :: Opt.Mod Opt.CommandFields Command
initCmd =
  Opt.command "init" $ Opt.info parser (Opt.progDesc "Initialize a Flatfile 'X' configuration project")
  where
    parser :: Opt.Parser Command
    parser =
      Init
        <$> Opt.option
          templateReader
          ( Opt.long "template"
              <> Opt.metavar "<TEMPLATE>"
              <> Opt.value TypeScript
              <> Opt.showDefault
              <> Opt.help "Project template. Either: 'javascript', 'typescript', 'local:', 'remote:'"
          )
    templateReader :: Opt.ReadM Template
    templateReader = Opt.eitherReader $ \case
      "javascript" -> Right JavaScript
      "typescript" -> Right TypeScript
      "local:" -> Right (Local "asdf")
      "remote:" -> Right (Remote "asdf")
      bad -> Left $ "Unknown value: " <> bad

publishCmd :: Opt.Mod Opt.CommandFields Command
publishCmd =
  Opt.command "publish" $ Opt.info parser (Opt.progDesc "Publish your configuration to Flatfile")
  where
    parser :: Opt.Parser Command
    parser =
      Publish
        <$> Opt.strOption
          ( Opt.long "file"
              <> Opt.metavar "<FILE_PATH>"
              <> Opt.help "Path to bundled file"
          )
