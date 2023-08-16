{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Common.Html where

import qualified System.IO as IO
import Text.HTML.TagSoup (Tag)
import qualified Text.HTML.TagSoup as TS

soup :: FilePath -> IO [Tag String]
soup fname =
  IO.openFile fname IO.ReadMode >>= \inputHandle ->
    IO.hSetEncoding inputHandle IO.latin1
      >> IO.hGetContents inputHandle -- utf8
      >>= \theInput -> (pure . TS.parseTags) theInput
