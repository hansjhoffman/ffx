module Run
  ( run,
  )
where

import RIO
import Types

run :: RIO App ()
run = do
  logInfo "Done!"
