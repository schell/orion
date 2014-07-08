{-# LANGUAGE OverloadedStrings #-}
module Web.Orion.Config where

import Data.Configurator
import Data.Configurator.Types
import Data.List
import Control.Monad
import System.Environment
import System.Directory
import System.IO


defaultCfgFileName :: String
defaultCfgFileName = "orion.cfg"


defaultCfgFileContents :: String
defaultCfgFileContents = intercalate "\n"
    [ "port = 9988"
    , "cookie_life = " ++ show (60 * 10 :: Int) -- in seconds.
    , "users-db-filepath = users.db"
    , "new-user-acl-level = 100"
    ]


getCfg :: IO Config
getCfg = do
    -- Get the path to our config file.
    args <- getArgs
    let cfgFilePath = case args of
                          fp:_ -> fp
                          []   -> defaultCfgFileName
    fileExists <- doesFileExist cfgFilePath
    -- Create it if it doesn't exist.
    unless fileExists $ do
        cwd <- getCurrentDirectory
        let d = cwd ++ "/" ++ defaultCfgFileName
        putStrLn $ unwords ["Creating default config file at",d]
        h <- openFile d WriteMode
        hPutStr h defaultCfgFileContents
        hClose h
    -- Get our config file.
    fmap fst $ autoReload autoConfig [Required cfgFilePath]


getCfgPort :: Config -> IO Int
getCfgPort cfg = lookupDefault 9988 cfg "port"


getCfgCookieLife :: Config -> IO Integer
getCfgCookieLife cfg = lookupDefault (10 * 60) cfg "cookie_life"


getCfgUserDBFilePath :: Config -> IO FilePath
getCfgUserDBFilePath cfg = lookupDefault "users.db" cfg "users-db-filepath"


getCfgNewUserAclLevel :: Config -> IO Integer
getCfgNewUserAclLevel cfg = lookupDefault 100 cfg "new-user-acl-level"


getCfgBaseUrl :: Config -> IO String
getCfgBaseUrl cfg = lookupDefault "http://127.0.0.1:9988" cfg "base-url"
