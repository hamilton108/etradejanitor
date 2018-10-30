
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

data Myp =  Myp { getA :: Int64 } deriving (Show)

mypParams :: HE.Params Myp
mypParams =
  contramap getA (HE.param HE.int8)

xx =
  let
    encoder = HE.param HE.int8
  in
  HQ.Statement "insert into stockmarket.ax (ar) values ($1)" encoder HD.unit True

xxx =
  session $ HS.statement 345 xx

qq =
  HQ.Statement "insert into stockmarket.ax (ar) values ($1)" mypParams HD.unit True

qqq =
  session $ HS.statement (Myp 34095) qq
  
{-
  liftIO $
  putStrLn t >>
  putStrLn t >>
  let
    result = v * 20
  in
  return result
-}
