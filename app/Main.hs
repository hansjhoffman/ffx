{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Options.Applicative.Simple as Opts
import qualified Paths_ffx
import RIO
import RIO.Process
import Run
import qualified RIO.Text as T
import System.Environment (getEnv)

import Types

optionsParser :: IO (Options, ())
optionsParser = do
  Opts.simpleOptions
    $(Opts.simpleVersion Paths_ffx.version)
    "Header for command line arguments"
    "Flatfile 'X' Code Generator."
    ( Options
        <$> Opts.switch
          ( Opts.short 'v'
              <> Opts.long "verbose"
              <> Opts.help "Verbose output?"
          )
    )
    Opts.empty

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

