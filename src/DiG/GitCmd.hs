{-# LANGUAGE OverloadedStrings #-}

module DiG.GitCmd where


import System.Process (createProcess, CreateProcess(..), StdStream(CreatePipe, UseHandle), proc, shell)
import System.IO (Handle)
import Data.ByteString.Char8 (ByteString, hGetContents, append, unpack)


(~~) :: ByteString -> ByteString -> ByteString
(~~) = append

(~+) :: ByteString -> ByteString -> String
a ~+ b = unpack $ a ~~ b 

data Repo = Repo String

posProcessor :: Handle -> ByteString -> IO Handle
posProcessor out cmd = do
    (_ , Just hOut, _, _) <- 
        createProcess cmd'  { std_in = UseHandle out
                            , std_out = CreatePipe }
    return hOut   
    where
        cmd' = shell $ unpack cmd
        
fileAtTag :: Repo -> ByteString -> Maybe ByteString -> ByteString -> Maybe ByteString -> IO ByteString
fileAtTag (Repo dir) tag root file posProc = do
    (_ , Just hOut, _, _) <- 
        createProcess (proc "git" ["show", fileRef root file]) 
                            {   cwd = Just dir
                            , std_out = CreatePipe }
    hOut' <- maybe (return hOut) (posProcessor hOut) posProc 
    putStrLn $ "Getting contents for: " ++ fileRef root file
    hGetContents hOut' 
    where
        fileRef (Just r) f = (tag ~~ ":" ~~ r ~~ "/")  ~+ f
        fileRef Nothing f  = (tag ~~ ":")  ~+ f

fileAtTagWithPrefixSuffix :: Repo -> ByteString -> ByteString -> Maybe ByteString -> ByteString -> Maybe ByteString -> IO ByteString
fileAtTagWithPrefixSuffix repo prefix suffix root file posProc = fileAtTag repo tag root file posProc
    where
        tag = prefix ~~ "-" ~~ suffix 

