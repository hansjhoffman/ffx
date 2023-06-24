module Main (main) where

import Api.Id (EnvironmentId (..))
import Data.Version qualified as V
import Options.Applicative
import Paths_ffx qualified as Meta
import RIO
import RIO.Process
import RIO.Text qualified as T
import Run
import System.Environment (getEnv)
import Types

initCmd :: Mod CommandFields Command
initCmd =
  command "init" $ info parser (progDesc "Initialize a Flatfile 'X' configuration project")
  where
    parser =
      Init
        <$> option
          templateReader
          ( long "template"
              <> metavar "<TEMPLATE>"
              <> value TypeScript
              <> showDefault
              <> help "Project template. Either: 'javascript', 'typescript', 'local:', 'remote:'"
          )
    templateReader :: ReadM Template
    templateReader = eitherReader $ \case
      "javascript" -> Right JavaScript
      "typescript" -> Right TypeScript
      "local:" -> Right (Local "asdf")
      "remote:" -> Right (Remote "asdf")
      bad -> Left $ "Unknown value: " <> bad

publishCmd :: Mod CommandFields Command
publishCmd =
  command "publish" $ info parser (progDesc "Publish your configuration to Flatfile")
  where
    parser =
      Publish
        <$> strOption
          ( long "file"
              <> metavar "<FILE_PATH>"
              <> help "Path to bundled file"
          )

programOptions :: Parser AppOptions
programOptions =
  AppOptions
    <$> switch
      ( long "debug"
          <> short 'd'
          <> help "Output information useful for debugging"
      )
    <*> hsubparser (initCmd <> publishCmd)

versionParser :: Parser (a -> a)
versionParser =
  infoOption
    (prettyVersion Meta.version)
    (long "version" <> help "Show version")
  where
    prettyVersion :: V.Version -> [Char]
    prettyVersion = (++) "ffx v" . V.showVersion

optsParser :: ParserInfo AppOptions
optsParser =
  info (helper <*> versionParser <*> programOptions) $
    fullDesc
      <> header "Flatfile 'X' CLI"
      <> progDesc "Create a starter project and publish your code."
      <> footer "For more information on ffx, please visit https://foobar.com"

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnEmpty) optsParser
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
