{-# LANGUAGE OverloadedStrings #-}
module Web.Auth.Service where

import Web.Auth.Types
import Network.OAuth.OAuth2
import Keys


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

oauth2serviceKey :: AuthService -> Maybe OAuth2
oauth2serviceKey Github   = Just githubKey
oauth2serviceKey Facebook = Just facebookKey
oauth2serviceKey _        = Nothing


serviceQuery :: AuthService -> QueryParams
serviceQuery Github = []
serviceQuery Facebook = [("scope", "user_about_me,email")]
serviceQuery Bitbucket = []


serviceUrl :: AuthService -> URI
serviceUrl Github = "https://api.github.com/user"
serviceUrl Facebook = "https://graph.facebook.com/me?fields=id,name,username,email&"
serviceUrl Bitbucket = ""


