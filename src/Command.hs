module Command where

import qualified Options.Applicative as Opts

import qualified Command.Deploy as Deploy
import qualified Command.Init as Init

optionsParser :: Opts.ParserInfo (IO ())
optionsParser =
  Opts.info (Opts.commands <**> Opts.helper) $
    Opts.fullDesc <> Opts.progDesc "ASDF" <> Opts.header "asdf"

parseCommand :: Opts.Parser (IO ())
parseCommand =
  Opts.subparser
    ( Opts.command "deploy" Deploy.command
        <> Opts.command "init" Init.command
    )

