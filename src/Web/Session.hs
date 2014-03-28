{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Session where

import           Web.Orion
import           Web.Scotty.Trans
import           Web.Orion.Types
import           Web.User
import           Web.ClientSession
import           Web.Template
import           Web.Template.Renderer
import           Control.Monad.IO.Class (liftIO)
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


withAuthUser :: (User -> ActionOM ()) -> ActionOM ()
withAuthUser f = authorize $ do
    mUser <- readUserCookie
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
        let c = fromJust mCookie
        invalidCookie <- cookieHasExpired c
        if invalidCookie then g else do
            -- Update the cookie.
            writeUserCookie c
            f


renewUserCookie :: ActionOM ()
renewUserCookie = do
    mUser <- readUserCookie
    flip (maybe (liftIO $ putStrLn "Can't renew cookie.")) mUser $ \u -> do
        t <- liftIO $ getCurrentTime
        seconds <- fmap fromIntegral readCfgCookieLife
        let t' = addUTCTime seconds t
        writeUserCookie $ updateExpires t' u


expireUserCookie :: ActionOM ()
expireUserCookie = do
    mUser <- readUserCookie
    flip (maybe (return ())) mUser $ \u -> do
        t <- liftIO $ getCurrentTime
        let t' = addUTCTime (-60 * 10)  t
        writeUserCookie $ updateExpires t' u


writeUserCookie :: User -> ActionOM ()
writeUserCookie u = do
    k  <- liftIO getDefaultKey
    u' <- liftIO $ encryptIO k $ LB.toStrict $ encode u
    t  <- liftIO $ getCurrentTime
    let life = diffUTCTime (_ucExpires $ _uCreds u) t
    setHeader "Set-Cookie" $ TL.concat [ cookieName
                                       , "="
                                       , TL.pack $ B.unpack u'
                                       , "; "
                                       , "Path=/; "
                                       , "Max-Age="
                                       , TL.pack $ show life
                                       , "; HttpOnly"
                                       ]


readUserCookie :: ActionOM (Maybe User)
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


cookieHasExpired :: User -> ActionOM Bool
cookieHasExpired c = fmap (<= 0) $ cookieExpiresIn c


cookieExpiresIn :: User -> ActionOM NominalDiffTime
cookieExpiresIn u = do
    t <- liftIO getCurrentTime
    let d = diffUTCTime (_ucExpires $ _uCreds u) t
    return d


updateExpires :: UTCTime -> User -> User
updateExpires t u =
    let c = _uCreds u
    in u{_uCreds=c{_ucExpires=t}}


cookieName :: TL.Text
cookieName = "orioncookie"


type CookieMap = M.Map TL.Text TL.Text


parseCookies :: TL.Text -> CookieMap
parseCookies = foldl mapify M.empty . map tuple . splitCookies
    where splitCookies   = TL.split (==';')
          tuple t        = (TL.takeWhile (/= '=') $ TL.dropWhile (== ' ') t, TL.drop 1 $ TL.dropWhile (/= '=') t)
          mapify m (k,v) = M.insert k v m


