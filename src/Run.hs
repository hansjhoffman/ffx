module Run (run) where

import Api.Agent qualified
import Api.Environment qualified
import RIO
import RIO.Text qualified as T
import Types
import Prelude (readFile)

-- import System.Directory (getHomeDirectory) -- https://hackage.haskell.org/package/directory-1.3.8.1/docs/System-Directory.html
-- import System.FilePath ((</>)) -- https://hackage.haskell.org/package/filepath-1.4.100.3/docs/System-FilePath.html

init :: Template -> RIO App ()
init = logInfo . (<>) "init " . displayShow

publish :: FilePath -> RIO App ()
publish targetFile = do
  env <- ask
  logInfo $ "publish " <> displayShow targetFile

  sourceCode <- liftIO $ readFile targetFile

  Api.Environment.get (view flatfileEnvIdL env)
    >> Api.Agent.create (T.pack sourceCode)
    >>= logDebug . displayShow

  logInfo "Done!! 🎉"

run :: RIO App ()
run = do
  env <- ask
  case aoCommand $ view optsL env of
    Init template -> init template
    Publish targetFile -> publish targetFile
