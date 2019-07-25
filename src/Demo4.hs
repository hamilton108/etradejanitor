{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-top-binds #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS -Wno-unused-imports #-}

module Demo4 where

import Control.Monad.Reader (ReaderT,ask,runReaderT)
import Control.Monad.IO.Class (liftIO)


type REIO = ReaderT Env IO

data Env = Env { getS :: String } deriving (Show)

readerOne :: REIO ()
readerOne =
  ask >>= \env ->
    liftIO $ one (getS env)


one :: String -> IO ()
one s =
    putStrLn s

x :: IO ()
x = 
    let 
        env = Env "jada"
    in 
    runReaderT readerOne env

two :: IO String 
two = pure "jaddddaxxxx"

twox :: REIO String
twox = 
  ask >>= \env ->
    pure (getS env)

x2 = 
    let 
        env = Env "jadaxxxxx"
    in 
    runReaderT twox env
