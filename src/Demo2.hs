
module Demo2 where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (Reader,ask,runReader)


x :: Reader FilePath (IO ())
x =
  ask >>= \t ->
  liftIO $ putStrLn t 
