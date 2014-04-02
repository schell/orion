{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.Auth.Types where

import           Control.Applicative
import           Control.Monad
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text (Text)
import           Web.Scotty
import           Network.OAuth.OAuth2
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy as LT


zeroDay :: UTCTime
zeroDay = UTCTime (ModifiedJulianDay 0) 0


instance ToJSON AccessToken where
    toJSON (AccessToken at Nothing) = object [ "access_token" .= B.unpack at ]
    toJSON (AccessToken at (Just rt)) = object [ "access_token" .= B.unpack at
                                               , "refresh_token" .= B.unpack rt
                                               ]


--data User = GithubUser { _uCreds :: UserCreds
--                       , _uData  :: UserData
--                       }
--          | FacebookUser { _uCreds :: UserCreds
--                         , _uData  :: UserData
--                         } deriving (Show)
--
--
--instance FromJSON User where
--    parseJSON v = parseGithubUser v <|> parseFacebookUser v
--
--
--instance ToJSON User where
--    toJSON (GithubUser creds data') = object [ "auth_service" .= ("github" :: String)
--                                             , "creds" .= creds
--                                             , "data" .= data'
--                                             ]
--    toJSON (FacebookUser creds data') = object [ "auth_service" .= ("facebook" :: String)
--                                               , "creds" .= creds
--                                               , "data" .= data'
--                                               ]


--parseGithubUser :: Value -> Parser User
--parseGithubUser (Object o) = do
--    "github" <- o .: "auth_service" :: Parser String
--    GithubUser <$> o .: "creds"
--               <*> o .: "data"
--parseGithubUser _ = mzero
--
--
--parseFacebookUser :: Value -> Parser User
--parseFacebookUser (Object o) = do
--    "facebook" <- o .: "auth_service" :: Parser String
--    FacebookUser <$> o .: "creds"
--               <*> o .: "data"
--parseFacebookUser _ = mzero


--data UserCreds = UserCreds { _ucToken   :: AccessToken
--                           , _ucExpires :: UTCTime
--                           , _ucId      :: Integer
--                           } deriving (Show)
--
--
--instance FromJSON UserCreds where
--    parseJSON (Object o) = UserCreds
--                           <$> o .: "token"
--                           <*> o .: "expires"
--                           <*> o .: "id"
--    parseJSON _ = mzero
--
--instance ToJSON UserCreds where
--    toJSON (UserCreds token expires id') =
--        object [ "token" .= token
--               , "expires" .= expires
--               , "id" .= id'
--               ]


data UserData = UserData { _udId      :: Integer
                         , _udLogin   :: Text
                         , _udName    :: Text
                         , _udEmail   :: Text
                         } deriving (Show, Eq)


instance FromJSON UserData where
    parseJSON o = parseFacebookUserData o <|> parseGithubUserData o


parseGithubUserData :: Value -> Parser UserData
parseGithubUserData (Object o) =
    UserData <$> o .: "id"
             <*> o .: "login"
             <*> o .: "name"
             <*> o .: "email"
parseGithubUserData _ = mzero


parseFacebookUserData :: Value -> Parser UserData
parseFacebookUserData (Object o) =
    UserData <$> (read <$> o .: "id")
             <*> o .: "username"
             <*> o .: "name"
             <*> o .: "email"
parseFacebookUserData _ = mzero


instance ToJSON UserData where
    toJSON (UserData id' login name email) =
        object [ "id" .= id'
               , "login" .= login
               , "name" .= name
               , "email" .= email
               ]


data AuthService = Github | Facebook deriving (Show, Eq)


instance Parsable AuthService where
    parseParam pm =
        case LT.toLower pm of
            "github"   -> Right Github
            "facebook" -> Right Facebook
            _          -> Left "Service is not one of github or facebook."


