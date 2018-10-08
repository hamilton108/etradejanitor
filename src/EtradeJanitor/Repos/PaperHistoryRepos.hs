{-# LANGUAGE OverloadedStrings #-}

module EtradeJanitor.Repos.PaperHistoryRepos where

import EtradeJanitor.Repos.Common (conn)

import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import Hasql.Encoders (Params(..),Value(..),text,int8,param) 
import Hasql.Decoders (Row)
import Data.Functor.Contravariant (contramap)

    
import Data.Text (Text,pack)
-- oid | ticker_id |     dx     |  opn  |  hi   |  lo   |  cls  |   vol

demo :: IO ()
demo = Connection.acquire conn >>= \c ->
    let msg = case c of 
            Right cx -> "YES" 
            Left err -> "Nope" 
    in  
    putStrLn msg



data Person =
    Person { name :: Text, gender :: Gender, age :: Int }
    
data Gender =
    Male | Female
    
personParams :: Params Person
personParams =
    contramap name (param text) <>
    contramap gender (param genderValue) <>
    contramap (fromIntegral . age) (param int8)
    
genderValue :: Value Gender
genderValue =
    contramap genderText text
    where
        genderText ge =
            case ge of
                Male -> "male"
                Female -> "female"