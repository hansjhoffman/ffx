{-# LANGUAGE TemplateHaskell #-}

module Main where

import Options.Applicative.Simple qualified as Opts
import Paths_ffx qualified as Meta
import RIO
import RIO.Process
import RIO.Text qualified as T
import Run
import System.Environment (getEnv)
-- import System.Directory (getHomeDirectory)
-- import System.FilePath ((</>))

import Types

runCmd :: Command -> ()
runCmd = \case
  (Init _) -> ()
  (Publish _) -> ()

initCmdParser :: Opts.Parser Command
initCmdParser =
  Init
    <$> Opts.argument
      templateReader
      ( Opts.metavar "TEMPLATE"
          <> Opts.value TypeScript
          <> Opts.completer
            (Opts.listCompleter [show TypeScript, show JavaScript])
          <> Opts.help "Project template (javascript, typescript, file:, remote:)"
      )
  where
    templateReader :: Opts.ReadM Template
    templateReader = Opts.eitherReader $ \case
      "javascript" -> Right JavaScript
      "typescript" -> Right TypeScript
      "file:" -> Right (Local "asdf")
      "remote:" -> Right (Remote "asdf")
      unknown -> Left $ "Unknown template: " <> unknown

publishCmdParser :: Opts.Parser Command
publishCmdParser =
  Publish
    <$> Opts.strOption
      ( Opts.long "file"
          <> Opts.metavar "FILEPATH"
          <> Opts.help "Path to CJS file."
      )

optionsParser :: IO (Options, ())
optionsParser = do
  Opts.simpleOptions
    $(Opts.simpleVersion Meta.version)
    mempty
    "Flatfile 'X' Code Generator."
    ( Options
        <$> Opts.switch
          ( Opts.long "debug"
              <> Opts.short 'd'
              <> Opts.help "Output information useful for debugging"
          )
    )
    $ do
      Opts.addCommand
        "init"
        "Initialize a Flatfile 'X' configuration project"
        runCmd
        initCmdParser
      Opts.addCommand
        "publish"
        "Publish your configuration to Flatfile"
        runCmd
        publishCmdParser

main :: IO ()
main = do
  (options, ()) <- optionsParser
  logOpts <- logOptionsHandle stderr $ optionsDebug options
  processCtx <- mkDefaultProcessContext
  flatfileSecretKey <- getEnv "FLATFILE_SECRET_KEY"
  withLogFunc logOpts $ \logFn ->
    let app =
          App
            { appFlatfileSecretKey = T.pack flatfileSecretKey,
              appLogFn = logFn,
              appOptions = options,
              appProcessContext = processCtx
            }
     in runRIO app run
