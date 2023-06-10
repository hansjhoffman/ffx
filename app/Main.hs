{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Options.Applicative.Simple as Opts
import qualified Paths_ffx as Meta
import RIO
import RIO.Process
import Run
import qualified RIO.Text as T
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
    templateReader = Opts.maybeReader $ \case
      "javascript" -> Just JavaScript
      "typescript" -> Just TypeScript
      "file:" -> Just (Local "asdf")
      "remote:" -> Just (Remote "asdf")
      _ -> Nothing

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
          ( Opts.short 'v'
              <> Opts.long "verbose"
              <> Opts.help "Verbose output?"
          )
        <*> Opts.switch
          ( Opts.long "debug"
              <> Opts.help "Debug?"
          )
    )
    $ do
      Opts.addCommand
        "init"
        "Initialize a Flatfile 'X' configuration project."
        runCmd
        initCmdParser
      Opts.addCommand
        "publish"
        "Publish your configuration to Flatfile."
        runCmd
        publishCmdParser

main :: IO ()
main = do
  (options, ()) <- optionsParser
  logOpts <- logOptionsHandle stderr $ optionsVerbose options
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

