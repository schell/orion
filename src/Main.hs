{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Keys
import           Web.Scotty as S
import           Web.ClientSession
import           Crypto.BCrypt
import           Web.Template
import           Web.Template.Renderer
import           Network.OAuth.OAuth2
import           Control.Concurrent.MVar
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Text.Blaze.Html
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Aeson
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Map                        as M
import qualified Data.Text.Lazy                  as LT
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy            as LB

--data Users

main :: IO ()
main = do
    scotty 9988 $ do
        get "/" $ do
           mUser <- readUserCookie :: ActionM (Maybe (User GithubUser))
           blaze $ wrapper "Home" $ toHtml $ show mUser
        get "/login" $ blaze $ wrapper "Login" $ loginOptions
        get "/login/:service" $ do
            Just salt <- liftIO $ genSaltUsingPolicy fastBcryptHashingPolicy
            let query   = [("state", salt)]
                authUrl = authorizationUrl githubKey `appendQueryParam` query
            -- | TODO: Add in more services.
            --service <- S.param "service"
            redirect $ bsToLT authUrl
        get "/githubCallback" $ do
            code <- S.param "code"
            state <- S.param "state"
            -- | TODO: Check the state to see if it matches.
            let (url, body') = accessTokenUrl githubKey code
                query        = [("state", state)]
            token <- liftIO $ doJSONPostRequest url $ body' ++ query :: ActionM (OAuth2Result AccessToken)
            case token of
                Right at -> do eUser <- liftIO $ githubUserInfo at
                               case eUser of
                                   Right u -> do t <- liftIO $ getCurrentTime
                                                 let u' = User at t u
                                                 writeUserCookie u'
                                                 blaze $ wrapper "Login" $ toHtml $ show u'
                                   Left l  -> blaze $ wrapper "Login" $ toHtml $ show l
                -- | TODO: Better error handling.
                Left l   -> blaze $ wrapper "Login" $ toHtml $ show l


bsToLT :: B.ByteString -> LT.Text
bsToLT = LT.pack . B.unpack

githubUserInfo :: AccessToken -> IO (OAuth2Result GithubUser)
githubUserInfo token = authGetJSON token "https://api.github.com/user"


instance ToJSON AccessToken where
    toJSON (AccessToken at Nothing) = object [ "access_token" .= B.unpack at ]
    toJSON (AccessToken at (Just rt)) = object [ "access_token" .= B.unpack at
                                               , "refresh_token" .= B.unpack rt
                                               ]


data User a = User { _uToken   :: AccessToken
                   , _uExpires :: UTCTime
                   , _uData    :: a
                   } deriving (Show)

instance FromJSON a => FromJSON (User a) where
    parseJSON (Object o) = User
                       <$> o .: "token"
                       <*> o .: "expires"
                       <*> o .: "data"
    parseJSON _ = mzero

instance ToJSON a => ToJSON (User a) where
    toJSON (User token expires data') =
        object [ "token" .= token
               , "expires" .= expires
               , "data" .= data'
               ]


data GithubUser = GithubUser { _gId      :: Integer
                             , _gLogin   :: Text
                             , _gName    :: Text
                             , _gEmail   :: Text
                             } deriving (Show, Eq)

zeroDay :: UTCTime
zeroDay = UTCTime (ModifiedJulianDay 0) 0

instance FromJSON GithubUser where
    parseJSON (Object o) = GithubUser
                           <$> o .: "id"
                           <*> o .: "login"
                           <*> o .: "name"
                           <*> o .: "email"
    parseJSON _ = mzero

instance ToJSON GithubUser where
    toJSON (GithubUser id' login name email) =
        object [ "id" .= id'
               , "login" .= login
               , "name" .= name
               , "email" .= email
               ]

cookieName :: LT.Text
cookieName = "orioncookie"

updateExpires :: User a -> IO (User a)
updateExpires u = do
    t <- getCurrentTime
    let mins = 10 {-minutes-} * 60 {-seconds-}
        t' = addUTCTime mins t
    return u{_uExpires=t'}

writeUserCookie :: ToJSON a => User a -> ActionM ()
writeUserCookie u = do
    u'  <- liftIO $ updateExpires u
    k   <- liftIO getDefaultKey
    u'' <- liftIO $ encryptIO k $ LB.toStrict $ encode u'
    setHeader "Set-Cookie" $ LT.concat [ cookieName
                                       , "="
                                       , LT.pack $ B.unpack u''
                                       , "; "
                                       ]

type CookieMap = M.Map LT.Text LT.Text


parseCookies :: LT.Text -> CookieMap
parseCookies = foldl mapify M.empty . map tuple . splitCookies
    where splitCookies   = LT.split (==';')
          tuple t        = (LT.takeWhile (/= '=') $ LT.dropWhile (== ' ') t, LT.drop 1 $ LT.dropWhile (/= '=') t)
          mapify m (k,v) = M.insert k v m


readUserCookie :: FromJSON a => ActionM (Maybe (User a))
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
                  mData  = decrypt k (B.pack $ LT.unpack cookie)
              if isNothing mData then return Nothing else
                let datum = LB.fromStrict $ fromJust mData
                in return $ decode datum
