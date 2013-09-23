{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import DiG.GitCmd
import DiG.Config
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString, pack, unpack, append) 

import Data.Maybe

import System.Environment (getArgs)
import System.Console.GetOpt 
import Data.ConfigFile

main :: IO ()
main = do
    args <- getArgs
    let ([_, configFile], _, _) = getOpt RequireOrder options args
    docs <- buildConf configFile
    exec docs 
    where
        options =   [ Option ['p']     []           (ReqArg id "PORT")    "Port to start the http server"
                    , Option ['c']     ["config"]   (ReqArg id "FILE")    "Configuration File"
                    ]
        exec (Left _)     = putStrLn "Error reading the configuration"
        exec (Right docs) = quickHttpServe $ site docs

(</>) :: ByteString -> ByteString -> ByteString
a </> b = append a $ append "/" b

routesForDocSet :: DocSet -> Snap () 
routesForDocSet (DocSet n r ps d i pos) = route $ map prefixedTagBuilder ps 
    where
        prefixedTagBuilder p = (n </> p </> ":suffix" </> ":file", prefixedTagHandler r p d i pos)  

--allRoutes :: MonadSnap m => [DocSet] -> m a
--allRoutes = undefined

prefixedTagHandler :: ByteString -> ByteString -> Maybe ByteString -> ByteString -> Maybe ByteString -> Snap ()
prefixedTagHandler r p d i pos = do
    suffix <- getParamString "20130101" "suffix" 
    file <- getParamString i "file" 
    contents <- liftIO $ fileAtTagWithPrefixSuffix r' p suffix d file pos
    writeBS $ contents
    where
        r' = Repo $ unpack r
--
site :: [DocSet] -> Snap ()
site docs = 
    ifTop (writeBS "hello world") <|>
    routesForDocSet (head docs) <|>
    --route [ ("foo", writeBS "bar")
    --      , ("echo/:echoparam", echoHandler)
    --      ] <|>
    --allRoutes docs <|> 
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

--repo = Repo "/Users/Oscar/repos/Cortex-Salmon/"
--
getParamString :: ByteString -> ByteString -> Snap ByteString
getParamString d p = do
    param <- getParam p
    return $ fromMaybe d param
--
--tagHandler :: Snap ()
--tagHandler = do
--    param <- getParam "prefix"
--    tags <- liftIO $ listTags repo
--    writeLBS $ tags
--
--dailyHandler :: Snap ()
--dailyHandler = do
--    day <- getParamString "20130101" "daily" 
--    contents <- liftIO $ listFiles repo day
--    writeLBS $ contents
--
--dailyFileHandler :: Snap ()
--dailyFileHandler = do
--    day <- getParamString "20130101" "daily" 
--    file <- getParamString "index.html" "file" 
--    contents <- liftIO $ showFileContents repo day ("Salmon/" ++ file)
--    writeLBS $ contents
--
--hashHandler :: Snap ()
--hashHandler = do
--    hash <- getParamString "ERROR" "hash" 
--    file <- getParamString "index.html" "file" 
--    contents <- liftIO $ showHashFileContents repo hash ("Salmon/" ++ file)
--    writeLBS $ contents
--
--blobHandler :: Snap ()
--blobHandler = do
--    hash <- getParamString "ERROR" "hash" 
--    contents <- liftIO $ showBlobContents repo hash 
--    writeLBS $ contents
