{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Api.Id (EnvironmentId (..))
import Data.Version qualified as V
import Options.Applicative qualified as Opts
import Paths_ffx qualified as Meta
import RIO
import RIO.Process
import RIO.Text qualified as T
import Run
import System.Environment (getEnv)
import Types

initCmd :: Opts.Mod Opts.CommandFields Command
initCmd =
  Opts.command
    "init"
    ( Opts.info parser $
        Opts.progDesc "Initialize a Flatfile 'X' configuration project"
    )
  where
    parser =
      Init
        <$> Opts.option
          templateReader
          ( Opts.long "template"
              <> Opts.metavar "TEMPLATE"
              <> Opts.value TypeScript
              <> Opts.showDefault
              <> Opts.help "Project template. Either 'javascript', 'typescript', 'file:', 'remote:')"
          )
    templateReader :: Opts.ReadM Template
    templateReader = Opts.eitherReader $ \case
      "javascript" -> Right JavaScript
      "typescript" -> Right TypeScript
      "local:" -> Right (Local "asdf")
      "remote:" -> Right (Remote "asdf")
      bad -> Left $ "Unknown template: " <> bad

publishCmd :: Opts.Mod Opts.CommandFields Command
publishCmd =
  Opts.command
    "publish"
    ( Opts.info parser $
        Opts.progDesc "Publish your configuration to Flatfile"
    )
  where
    parser =
      Publish
        <$> Opts.strOption
          ( Opts.long "file"
              <> Opts.metavar "FILEPATH"
              <> Opts.help "Path to CJS file."
          )

programOptions :: Opts.Parser AppOptions
programOptions =
  AppOptions
    <$> Opts.switch
      ( Opts.long "debug"
          <> Opts.short 'd'
          <> Opts.help "Output information useful for debugging"
      )
    <*> Opts.subparser (initCmd <> publishCmd)

versionOption :: Opts.Parser (a -> a)
versionOption =
  Opts.infoOption
    (prettyVersion Meta.version)
    (Opts.long "version" <> Opts.help "Show version")
  where
    prettyVersion :: V.Version -> [Char]
    prettyVersion = (++) "ffx v" . V.showVersion

optsParser :: Opts.ParserInfo AppOptions
optsParser =
  Opts.info
    (Opts.helper <*> versionOption <*> programOptions)
    ( Opts.fullDesc
        <> Opts.header "Flatfile 'X' CLI"
        <> Opts.progDesc "Create a starter project and publish your code."
        <> Opts.footer "For more information on ffx, please visit https://foobar.com"
    )

main :: IO ()
main = do
  opts <- Opts.customExecParser (Opts.prefs Opts.showHelpOnEmpty) optsParser
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
              appOptions = opts,
              appProcessContext = processCtx
            }
     in runRIO app run
