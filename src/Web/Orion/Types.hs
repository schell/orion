{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.Orion.Types where

import           Web.Scotty.Trans
import           Control.Monad.Reader
import           Data.Configurator.Types hiding (Value)
import           Data.Map
import           Data.ByteString
import           Data.Pool
import           Database.HDBC.Sqlite3
import           Database.HDBC
import           Network.HTTP.Conduit
import           Control.Concurrent.MVar
import qualified Data.Map as M
import           Control.Applicative
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Convertible
import           Network.OAuth.OAuth2
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT

type OrionM = ScottyT LT.Text (ReaderT OrionApp IO)

type ActionOM = ActionT LT.Text (ReaderT OrionApp IO)

data OrionApp = OrionApp { _oCfg        :: Config
                         , _oAuthStates :: AuthStatesVar
                         , _oDBCons     :: DatabaseCons
                         , _oManager    :: Manager
                         , _oKeys       :: KeyStore
                         }

data DatabaseCons = DatabaseCons { dbUsers :: Pool Connection
                                 , dbOther :: M.Map String (Pool Connection)
                                 }

type AuthStatesVar = MVar AuthStates

type AuthStates = (Map ByteString LT.Text)

findAccount :: AuthService -> [OrionAccount] -> Maybe OrionAccount
findAccount _ []     = Nothing
findAccount s (a:as) = if _accService a == s then Just a else findAccount s as

data OrionUser = OrionUser { _ouId       :: Integer
                           , _ouAcl      :: Integer
                           , _ouAccounts :: [OrionAccount]
                           } deriving (Show)

data OrionAccount = OrionAccount { _accId        :: Integer
                                 , _accService   :: AuthService
                                 , _accServiceId :: Integer
                                 , _accLogin     :: String
                                 , _accName      :: String
                                 , _accEmail     :: String
                                 , _accToken     :: Maybe AccessToken
                                 } deriving (Show)

zeroDay :: UTCTime
zeroDay = UTCTime (ModifiedJulianDay 0) 0

instance ToJSON AccessToken where
    toJSON (AccessToken at Nothing) = object [ "access_token" .= B.unpack at ]
    toJSON (AccessToken at (Just rt)) = object [ "access_token" .= B.unpack at
                                               , "refresh_token" .= B.unpack rt
                                               ]

instance Convertible SqlValue AccessToken where
    safeConvert (SqlByteString bs) = case decode $ LB.fromStrict bs of
                                         Nothing -> Left $ ConvertError "SqlByteString _" "SqlValue" "AccessToken" "ByteString JSON was not decoded."
                                         Just t  -> Right t
    safeConvert _                  = Left $ ConvertError "Non-ByteString SqlValue" "SqlValue" "AccessToken" "SqlValue is not a ByteString."

instance Convertible AccessToken SqlValue where
    safeConvert = safeConvert . encode

data UserData = UserData { _udId      :: Integer
                         , _udLogin   :: LT.Text
                         , _udName    :: LT.Text
                         , _udEmail   :: LT.Text
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

data KeyStore = KeyStore { keysWeibo :: OAuth2
                         , keysGithub :: OAuth2
                         , keysGoogle :: OAuth2
                         , keysFacebook :: OAuth2
                         , keysBitbucket :: OAuth
                         }

data OAuth = OAuth { oauthConsumerKey :: String
                   , oauthConsumerSecret :: String
                   }
--type ReaderOrion m = m Text (ReaderT OrionApp IO)

type CookieMap = M.Map LT.Text LT.Text

data UserCookie = UserCookie Integer UTCTime deriving (Show)

instance FromJSON UserCookie where
    parseJSON (Object o) = UserCookie <$> o .: "id"
                                      <*> o .: "expires"
    parseJSON _ = mzero

instance ToJSON UserCookie where
    toJSON (UserCookie uid expires) = object [ "id" .= uid
                                             , "expires" .= expires
                                             ]

