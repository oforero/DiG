{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module DiG.Config where

import Data.ByteString.Char8 (ByteString, pack)
import Data.ConfigFile
import Control.Monad.Error

import Data.Either (rights) 

data DocSet = DocSet { name :: ByteString
                     , repo :: ByteString
                     , prefixes :: [ByteString]
                     , rootDir :: Maybe ByteString
                     , index :: ByteString  
                     , posProc :: Maybe ByteString
                     } deriving (Show)

buildConf :: String -> IO (Either CPError [DocSet])
buildConf file = do
    conf <- runErrorT $ do
        conf <- join $ liftIO $ readfile emptyCP file
        let ds = sections conf 
        liftIO $ putStrLn $ show ds
        let ds' =  rights $ map (buildDocSet conf) ds 
        liftIO $ putStrLn $ show ds'
        return ds'
    return conf 

buildDocSet :: ConfigParser -> String -> Either CPError DocSet 
buildDocSet conf n = do
    r <- get conf n "repo"
    p <- get conf n "tags" 
    let d = getOpt' conf n "root"
    i <- get conf n "index"
    let proc = getOpt' conf n "proc"
    return $ doc r p d i proc
    where
        doc r p d i proc = DocSet { name = pack n
                                  , repo = pack r
                                  , prefixes = map pack $ words p
                                  , rootDir = d 
                                  , index = pack i
                                  , posProc = proc 
                                  }


getOpt' :: ConfigParser -> String -> String -> Maybe ByteString
getOpt' conf s i = fromEither $ get conf s i
    where
        fromEither (Left _) = Nothing
        fromEither (Right x) = Just $ pack x

readSection :: ConfigParser -> String-> [(String, String)] 
readSection _ _ = undefined
