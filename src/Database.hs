module Database where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory
import Web.Orion
import Web.Database.Types
import Control.Monad
import Control.Monad.IO.Class (liftIO)


createUserDatabaseIfNeeded :: OrionM ()
createUserDatabaseIfNeeded = do
    dbfp <- readCfgUserDBFilePath
    dbExists <- liftIO $ doesFileExist dbfp
    unless dbExists $ do
        liftIO $ putStrLn "Creating users.db"
        createUserDatabase


createUserDatabase :: OrionM ()
createUserDatabase = do
    withUserDB createTables
    withUserDB updateTables


createTables :: Connection -> IO ()
createTables conn = forM_ [schemaSQL, usersSQL, accountsSQL] (\sql -> void $ run conn sql [])


updateTables :: Connection -> IO ()
updateTables conn = void $ run conn updateSchemaSQL []


userDBFilePath :: IO FilePath
userDBFilePath = getCfg >>= getCfgUserDBFilePath


