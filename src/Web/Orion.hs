module Web.Orion (
    module O,
    optionalParam,
    defaultParam,
    orion,
    askAuthStates,
    readAuthStates,
    modifyAuthStates,
    readCfg,
    readCfgPort,
    readCfgCookieLife,
    readCfgUserDBFilePath,
    readCfgNewUserAclLevel,
    withUserDB,
    withDB
) where

import           Web.Orion.Types as O
import           Web.Orion.Config as O
import           Web.Scotty.Trans
import           Network.HTTP.Conduit hiding (port)
import           Data.Configurator.Types
import           Data.Pool
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Concurrent.MVar
import qualified Data.Text.Lazy as LT
import qualified Data.Map as M


optionalParam :: Parsable a => LT.Text -> ActionOM (Maybe a)
optionalParam x = (fmap Just $ param x) `rescue` (const $ return Nothing)


defaultParam :: Parsable a => LT.Text -> a -> ActionOM a
defaultParam x def = do
    mP <- optionalParam x
    return $ case mP of
        Nothing -> def
        Just pm -> pm


runOrion :: OrionApp -> (ReaderT OrionApp IO) a -> IO a
runOrion = flip runReaderT


orion :: KeyStore -> M.Map String (Pool Connection) -> OrionM () -> IO ()
orion k odbs f = do
    statesVar <- newMVar M.empty
    cfg   <- getCfg
    port  <- getCfgPort cfg
    dbFp  <- getCfgUserDBFilePath cfg
    mgr   <- newManager conduitManagerSettings
    users <- createPool (connectSqlite3 dbFp) disconnect 1 60 10

    let dbs   = DatabaseCons { dbUsers = users
                             , dbOther = odbs
                             }
        r = runOrion $ OrionApp cfg statesVar dbs mgr k
    scottyT port r r $ f


--askCfg :: (OrionConfig -> Config) -> m Config
--askCfg = asks _oCfg

--readStates = liftIO $ readMVar $ lift $ asks _oAuthStates

askAuthStates :: ActionOM AuthStatesVar
askAuthStates = lift $ asks _oAuthStates


readAuthStates :: ActionOM AuthStates
readAuthStates = askAuthStates >>= liftIO . readMVar


modifyAuthStates :: (AuthStates -> AuthStates) -> ActionOM ()
modifyAuthStates f = do
    statesVar <- askAuthStates
    liftIO $ modifyMVar_ statesVar $ return . f


type ScottyAction m a = m LT.Text (ReaderT OrionApp IO) a


readCfg :: MonadTrans (m LT.Text) => (Config -> IO a) -> ScottyAction m a
readCfg f = lift $ asks _oCfg >>= liftIO . f


readCfgPort :: MonadTrans (m LT.Text) => ScottyAction m Int
readCfgPort = readCfg getCfgPort


readCfgCookieLife :: MonadTrans (m LT.Text) => ScottyAction m Integer
readCfgCookieLife = readCfg getCfgCookieLife


readCfgUserDBFilePath :: MonadTrans (m LT.Text) => ScottyAction m FilePath
readCfgUserDBFilePath = readCfg getCfgUserDBFilePath

withUserDB :: (Monad (m LT.Text (ReaderT OrionApp IO)),
               MonadTrans (m LT.Text),
               MonadIO (m LT.Text (ReaderT OrionApp IO)))
           => (Connection -> IO a) -> ScottyAction m a
withUserDB f = do
    pool <- lift $ asks (dbUsers . _oDBCons)
    liftIO $ withResource pool $ \conn -> withTransaction conn f

withDB :: (Monad (m LT.Text (ReaderT OrionApp IO)),
           MonadTrans (m LT.Text),
           MonadIO (m LT.Text (ReaderT OrionApp IO)))
           => String -> (Connection -> IO a) -> ScottyAction m (Maybe a)
withDB s f = do
    mPool <- lift $ M.lookup s <$> asks (dbOther . _oDBCons)
    case mPool of
        Nothing -> return Nothing
        Just p  -> do a <- liftIO $ withResource p $ \conn -> withTransaction conn f
                      return $ Just a


readCfgNewUserAclLevel :: ActionOM Integer
readCfgNewUserAclLevel = readCfg getCfgNewUserAclLevel
