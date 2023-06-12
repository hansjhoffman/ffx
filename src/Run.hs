module Run (run) where

import Api.Environment qualified
import RIO
import Types

-- import System.Directory (getHomeDirectory) -- https://hackage.haskell.org/package/directory-1.3.8.1/docs/System-Directory.html
-- import System.FilePath ((</>)) -- https://hackage.haskell.org/package/filepath-1.4.100.3/docs/System-FilePath.html

init :: Template -> RIO App ()
init = logInfo . (<>) "init " . displayShow

publish :: FilePath -> RIO App ()
publish targetFile = do
  env <- ask
  logInfo $ "publish " <> displayShow targetFile
  res <- Api.Environment.get (view flatfileEnvIdL env)
  logInfo $ displayShow res
  logInfo "Done!! 🎉"

run :: RIO App ()
run = do
  env <- ask
  case aoCommand $ view optsL env of
    Init template -> init template
    Publish targetFile -> publish targetFile
