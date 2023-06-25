module Run (run) where

import Api.Agent qualified
import Api.Environment qualified
import RIO
import RIO.Text qualified as T
import System.Directory qualified as Dir
import Types
import Prelude (readFile)

-- import System.FilePath ((</>)) -- https://hackage.haskell.org/package/filepath-1.4.100.3/docs/System-FilePath.html

run :: RIO App ()
run = do
  env <- ask
  case aoCommand $ view optsL env of
    Init template -> init template
    Publish targetFile -> publish targetFile

init :: Template -> RIO App ()
init = \case
  JavaScript -> logInfo "JavaScript"
  Local dir -> do
    isValidPath <- liftIO $ Dir.doesPathExist dir
    if isValidPath
      then logInfo "cool cool"
      else logInfo "yo! where you at?"
  Remote _ -> do
    tmpDir <- liftIO Dir.getTemporaryDirectory
    logInfo $ displayShow tmpDir
  TypeScript -> logInfo "TypeScript"

publish :: FilePath -> RIO App ()
publish targetFile = do
  env <- ask
  logDebug $ "publish " <> displayShow targetFile

  sourceCode <- liftIO $ readFile targetFile

  Api.Environment.get (view flatfileEnvIdL env)
    >> Api.Agent.create (T.pack sourceCode)
    >>= \case
      Left _ -> logError "❌ Uh oh! Failed to create an Agent."
      Right agent -> do
        logInfo $ "Created Agent: " <> displayShow (Api.Agent.agentId agent)
        logInfo "Done!! 🎉"
