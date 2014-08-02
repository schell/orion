{-# LANGUAGE OverloadedStrings #-}
module Web.Orion.OAuth.Services where

import           Web.Orion.Types
import           Network.OAuth.OAuth2


allServices :: [AuthService]
allServices = [Github, Facebook]

serviceToString :: AuthService -> String
serviceToString Github    = "github"
serviceToString Facebook  = "facebook"
serviceToString Bitbucket = "bitbucket"

stringToService :: String -> Maybe AuthService
stringToService "github"    = Just Github
stringToService "facebook"  = Just Facebook
stringToService "bitbucket" = Just Bitbucket
stringToService _           = Nothing

oauth2serviceKey :: KeyStore -> AuthService -> Maybe OAuth2
oauth2serviceKey k Github   = Just $ keysGithub k
oauth2serviceKey k Facebook = Just $ keysFacebook k
oauth2serviceKey _ _        = Nothing

serviceQuery :: AuthService -> QueryParams
serviceQuery Github = []
serviceQuery Facebook = [("scope", "user_about_me,email")]
serviceQuery Bitbucket = []

loginUrl :: AuthService -> URI
loginUrl Github = "https://api.github.com/user"
loginUrl Facebook = "https://graph.facebook.com/me?fields=id,name,username,email&"
loginUrl Bitbucket = ""



