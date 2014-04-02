{-# LANGUAGE RecordWildCards #-}
module Web.Database.Operations where

import Web.Database.Types
import Web.Orion
import Web.Auth.Types
import Web.Auth.Service
import Database.HDBC
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Maybe


lookupAccounts :: AuthService -> UserData -> ActionOM [OrionAccount]
lookupAccounts service UserData{..} = do
    accs <- withUserDB $ accountQuery service _udId
    return $ catMaybes $ map sqlToOrionAccount accs


createNewUser :: AuthService -> UserData -> ActionOM (Maybe OrionUser)
createNewUser service udat = do
    acl <- readCfgNewUserAclLevel
    uid <- withUserDB $ \conn -> do
        idset <- quickQuery' conn "SELECT id FROM users ORDER BY id DESC LIMIT 1" []
        liftIO $ putStrLn $ "Last id: " ++ show idset
        let uId = case idset of
                      [x]:_ -> (fromSql x) + 1
                      _ -> 1
            acl' = if uId == 1 then 0 else acl

        let vars = [ toSql acl' ]
            sql = "INSERT INTO users (acl_level) VALUES (?)"

        void $ run conn sql vars
        return uId
    addAccountToUser service uid udat


addAccountToUser :: AuthService -> Integer -> UserData -> ActionOM (Maybe OrionUser)
addAccountToUser service uid UserData{..} = do
    void $ withUserDB $ \conn -> do
        let vals = [ toSql uid
                   , toSql $ serviceToString service
                   , toSql _udId
                   , toSql _udLogin
                   , toSql _udName
                   , toSql _udEmail
                   ]
            sql = unwords [ "INSERT INTO accounts ("
                          , "user_id, service, service_id, service_login, "
                          , "service_name, service_email"
                          , ") VALUES (?,?,?,?,?,?)"
                          ]
        void $ run conn sql vals
    lookupUser uid


lookupUserByAccount :: OrionAccount -> ActionOM (Maybe OrionUser)
lookupUserByAccount OrionAccount{..} = do
    mId <- withUserDB $ \conn -> do
        let sql = "SELECT user_id FROM accounts WHERE id = ?"
            val = [toSql _accId]
        set <- quickQuery' conn sql val
        case set of
            [uid]:_ -> return $ Just $ fromSql uid
            _       -> return Nothing
    case mId of
        Nothing  -> return Nothing
        Just uid -> lookupUser uid


lookupUser :: Integer -> ActionOM (Maybe OrionUser)
lookupUser uid = do
    let usql = "SELECT * FROM users WHERE id = ?"
        uval = [toSql uid]
        asql = "SELECT * FROM accounts where user_id = ?"

    withUserDB $ \c -> do
        uset <- quickQuery' c usql uval
        case uset of
            []  -> return Nothing
            u:_ -> do aset <- quickQuery' c asql uval
                      let accs = catMaybes $ map sqlToOrionAccount aset
                      return $ fmap (\o -> o{_ouAccounts=accs}) (sqlToOrionUser u)


userExists :: Integer -> ActionOM Bool
userExists uid = do
    mUser <- lookupUser uid
    return $ case mUser of
        Nothing -> False
        Just _  -> True



sqlToOrionAccount :: [SqlValue] -> Maybe OrionAccount
sqlToOrionAccount [aid, _, service, sid, login, name, email] = do
    service' <- stringToService $ fromSql service
    return $ OrionAccount (fromSql aid)
                          service'
                          (fromSql sid)
                          (fromSql login)
                          (fromSql name)
                          (fromSql email)
sqlToOrionAccount _ = Nothing


sqlToOrionUser :: [SqlValue] -> Maybe OrionUser
sqlToOrionUser [uid, acl] = return $ OrionUser (fromSql uid)
                                               (fromSql acl)
                                               []
sqlToOrionUser _ = Nothing


accountQuery :: IConnection c => AuthService -> Integer -> c -> IO [[SqlValue]]
accountQuery service sid conn =
    let vars = [toSql $ serviceToString service, toSql sid]
        sql  = "SELECT * FROM accounts WHERE service = ? AND service_id = ?"
    in quickQuery' conn sql vars
