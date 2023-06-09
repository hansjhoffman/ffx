module Command.Init (command) where

import qualified Options.Applicative as Opts

optionsParserInit :: Opts.ParserInfo Options
optionsParserInit = Opts.info (Opts.helper <*> optionsParser)
  $ Opts.fullDesc
    <> Opts.progDesc "Test a Sixten program"
    <> Opts.header "sixten test"

command :: Opts.ParserInfo (IO ())
command = optionsParserInit
