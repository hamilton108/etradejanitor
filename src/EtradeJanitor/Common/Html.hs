{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Common.Html where

import qualified System.IO                     as IO
import qualified Text.HTML.TagSoup             as TS
import           Text.HTML.TagSoup              ( Tag )

soup :: FilePath -> IO [Tag String]
soup fname = IO.openFile fname IO.ReadMode >>= \inputHandle ->
  IO.hSetEncoding inputHandle IO.latin1
    >> -- utf8
        IO.hGetContents inputHandle
    >>= \theInput -> (pure . TS.parseTags) theInput
