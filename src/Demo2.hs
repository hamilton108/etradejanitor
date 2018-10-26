
module Demo2 where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT,Reader,ask,runReader,runReaderT)

data X = X { getA :: Int
           , getB :: Int } deriving (Show)

x :: Int -> ReaderT X IO Int
x  v =
  ask >>= \t ->
  liftIO $
  putStrLn (show t) >>
  return (getA t)

{-
  liftIO $
  putStrLn t >>
  putStrLn t >>
  let
    result = v * 20
  in
  return result
-}
