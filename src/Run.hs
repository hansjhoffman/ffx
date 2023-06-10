module Run
  ( run,
  )
where

import RIO
import Types

-- import System.Directory (getHomeDirectory) -- https://hackage.haskell.org/package/directory-1.3.8.1/docs/System-Directory.html
-- import System.FilePath ((</>)) -- https://hackage.haskell.org/package/filepath-1.4.100.3/docs/System-FilePath.html

run :: RIO App ()
run = do
  logDebug "Done!"
  logInfo "Done!!"
