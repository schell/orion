module Database where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory


userDBFilePath :: IO FilePath
userDBFilePath = do
    


createUserDatabaseIfNeeded :: IO ()
createUserDatabaseIfNeeded = do

