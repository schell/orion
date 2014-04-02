{-# LANGUAGE OverloadedStrings #-}
module Web.Auth.Service where

import Web.Auth.Types
import Network.OAuth.OAuth2
import Keys


allServices :: [AuthService]
allServices = [Github, Facebook]


serviceToString :: AuthService -> String
serviceToString Github = "github"
serviceToString Facebook = "facebook"


stringToService :: String -> Maybe AuthService
stringToService "github"   = Just Github
stringToService "facebook" = Just Facebook
stringToService _ = Nothing

serviceKey :: AuthService -> OAuth2
serviceKey Github   = githubKey
serviceKey Facebook = facebookKey


serviceQuery :: AuthService -> QueryParams
serviceQuery Github = []
serviceQuery Facebook = [("scope", "user_about_me,email")]


serviceUrl :: AuthService -> URI
serviceUrl Github = "https://api.github.com/user"
serviceUrl Facebook = "https://graph.facebook.com/me?fields=id,name,username,email&"


