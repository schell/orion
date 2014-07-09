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


data AuthService = Github | Bitbucket | Facebook deriving (Show, Eq)


instance Parsable AuthService where
    parseParam pm =
        case LT.toLower pm of
            "github"   -> Right Github
            "facebook" -> Right Facebook
            _          -> Left "Service is not one of github or facebook."


