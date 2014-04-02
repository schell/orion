{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Session where

import           Web.Orion
import           Web.Database.Types
import           Web.Database.Operations
import           Web.Scotty.Trans
import           Web.ClientSession
import           Web.Template
import           Web.Template.Renderer
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad
import           Control.Applicative
import           Network.HTTP.Types.Status
import           Network.Wai
import           Data.Time.Clock
import           Data.Aeson
import           Data.Maybe
import qualified Data.Map                        as M
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text                       as TS
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy            as LB


type CookieMap = M.Map TL.Text TL.Text


data UserCookie = UserCookie Integer UTCTime deriving (Show)


instance FromJSON UserCookie where
    parseJSON (Object o) = UserCookie <$> o .: "id"
                                      <*> o .: "expires"
    parseJSON _ = mzero


instance ToJSON UserCookie where
    toJSON (UserCookie uid expires) = object [ "id" .= uid
                                             , "expires" .= expires
                                             ]


withAuthUser :: (OrionUser -> ActionOM ()) -> ActionOM ()
withAuthUser f = authorize $ do
    mUser <- readUser
    case mUser of
        Just u  -> f u
        Nothing -> blazeUnauthorized


authorize :: ActionOM () -> ActionOM ()
authorize f = do
    pInfo <- fmap (TL.fromStrict . TS.intercalate "/" . pathInfo) request
    f `ifAuthorizedOr` redirect ("/login?redirect=" `TL.append` pInfo)


blazeUnauthorized :: ActionOM ()
blazeUnauthorized = (blaze $ wrapper "Unauthorized" $ errorStatus unauthorized401)



ifAuthorizedOr :: ActionOM a -> ActionOM a -> ActionOM a
ifAuthorizedOr f g = do
    mCookie <- readUserCookie
    if isNothing mCookie then g else do
        -- Check the expiry.
        let c@(UserCookie uid _) = fromJust mCookie
        isExpired <- cookieHasExpired c
        validUser <- userExists uid
        if isExpired || not validUser 
        -- Expire it for great good.
        then expireUserCookie >> g 
        -- Update the cookie.
        else writeUserCookie c >> f


renewUserCookie :: ActionOM ()
renewUserCookie = do
    mUser <- readUserCookie
    flip (maybe (liftIO $ putStrLn "Can't renew cookie.")) mUser $ \(UserCookie uid _) -> do
        t <- liftIO $ getCurrentTime
        seconds <- fmap fromIntegral readCfgCookieLife
        let t' = addUTCTime seconds t
        writeUserCookie $ UserCookie uid t'


expireUserCookie :: ActionOM ()
expireUserCookie = do
    mUser <- readUserCookie
    flip (maybe (return ())) mUser $ \(UserCookie uid _) -> do
        t <- liftIO $ getCurrentTime
        let t' = addUTCTime (-60 * 10)  t
        writeUserCookie $ UserCookie uid t'


writeUserCookie :: UserCookie -> ActionOM ()
writeUserCookie u@(UserCookie _ e) = do
    k  <- liftIO getDefaultKey
    u' <- liftIO $ encryptIO k $ LB.toStrict $ encode u
    t  <- liftIO $ getCurrentTime
    let life = diffUTCTime e t
    setHeader "Set-Cookie" $ TL.concat [ cookieName
                                       , "="
                                       , TL.pack $ B.unpack u'
                                       , "; "
                                       , "Path=/; "
                                       , "Max-Age="
                                       , TL.pack $ show life
                                       , "; HttpOnly"
                                       ]


readUserCookie :: ActionOM (Maybe UserCookie)
readUserCookie = do
    -- Retrieve and parse our cookies.
    mCookies <- reqHeader "Cookie"
    if isNothing mCookies then return Nothing else
      -- Make sure we have our specific cookie.
      let cookies = parseCookies $ fromJust mCookies
          mCookie = M.lookup cookieName cookies
      in if isNothing mCookie then return Nothing else
           -- Decrypt our cookie data.
           do k <- liftIO getDefaultKey
              let cookie = fromJust mCookie
                  mData  = decrypt k (B.pack $ TL.unpack cookie)
              if isNothing mData then return Nothing else
                let datum = LB.fromStrict $ fromJust mData
                in return $ decode datum


futureCookieForUser :: OrionUser -> ActionOM UserCookie
futureCookieForUser u = do
    l <- readCfgCookieLife
    t <- liftIO $ getCurrentTime
    let t' = addUTCTime (fromIntegral l) t
    return $ UserCookie (_ouId u) t'


readUser :: ActionOM (Maybe OrionUser)
readUser = do
    mCookie <- readUserCookie
    case mCookie of
        Nothing -> return Nothing
        Just (UserCookie uid _) -> lookupUser uid


cookieHasExpired :: UserCookie -> ActionOM Bool
cookieHasExpired c = fmap (<= 0) $ cookieExpiresIn c


cookieExpiresIn :: UserCookie -> ActionOM NominalDiffTime
cookieExpiresIn (UserCookie _ e) = do
    t <- liftIO getCurrentTime
    let d = diffUTCTime e t
    return d


cookieName :: TL.Text
cookieName = "orioncookie"


parseCookies :: TL.Text -> CookieMap
parseCookies = foldl mapify M.empty . map tuple . splitCookies
    where splitCookies   = TL.split (==';')
          tuple t        = (TL.takeWhile (/= '=') $ TL.dropWhile (== ' ') t, TL.drop 1 $ TL.dropWhile (/= '=') t)
          mapify m (k,v) = M.insert k v m


