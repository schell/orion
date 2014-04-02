{-# LANGUAGE OverloadedStrings #-}
module Web.Database.Types where

import           Web.Auth.Types


schemaSQL :: String
schemaSQL = unwords
    [ "CREATE TABLE IF NOT EXISTS schema ("
    , "version INTEGER NOT NULL DEFAULT (0)"
    , ")"
    ]


updateSchemaSQL :: String
updateSchemaSQL = "INSERT INTO schema VALUES (0)"


data OrionUser = OrionUser { _ouId       :: Integer
                           , _ouAcl      :: Integer
                           , _ouAccounts :: [OrionAccount]
                           } deriving (Show)


usersSQL :: String
usersSQL = unwords
    [ "CREATE TABLE IF NOT EXISTS users ("
    , "id INTEGER PRIMARY KEY ASC AUTOINCREMENT,"
    , "acl_level INTEGER NOT NULL DEFAULT (100)"
    , ")"
    ]


data OrionAccount = OrionAccount { _accId        :: Integer
                                 , _accService   :: AuthService
                                 , _accServiceId :: Integer
                                 , _accLogin     :: String
                                 , _accName      :: String
                                 , _accEmail     :: String
                                 } deriving (Show)


accountsSQL :: String
accountsSQL = unwords
    [ "CREATE TABLE IF NOT EXISTS accounts ("
    , "id INTEGER PRIMARY KEY ASC AUTOINCREMENT,"
    , "user_id INTEGER NOT NULL,"
    , "service VARCHAR (32) NOT NULL,"
    , "service_id INTEGER,"
    , "service_login VARCHAR (128),"
    , "service_name VARCHAR (128),"
    , "service_email VARCHAR (256)"
    , ")"
    ]



