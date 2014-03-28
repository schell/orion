module Web.Orion (
    module O,
    orion,
    askAuthStates,
    readAuthStates,
    modifyAuthStates,
    readCfgPort,
    readCfgCookieLife
) where

import           Web.Orion.Types as O
import           Web.Orion.Config
import           Web.Scotty.Trans
import           Control.Monad.Reader
import           Data.Map
import           Control.Concurrent.MVar


runOrion :: OrionApp -> (ReaderT OrionApp IO) a -> IO a
runOrion = flip runReaderT


orion :: OrionM () -> IO ()
orion f = do
    statesVar <- newMVar empty
    cfg  <- getCfg
    port <- getCfgPort cfg
    let run = runOrion $ OrionApp cfg statesVar
    scottyT port run run $ f


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

readCfgPort :: ActionOM Int
readCfgPort = do
    cfg <- lift $ asks _oCfg
    liftIO $ getCfgPort cfg


readCfgCookieLife :: ActionOM Integer
readCfgCookieLife = do
    cfg <- lift $ asks _oCfg
    liftIO $ getCfgCookieLife cfg


