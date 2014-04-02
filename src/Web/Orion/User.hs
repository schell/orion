{-# LANGUAGE RecordWildCards #-}
module Web.Orion.User where

import Web.Database.Types
import Web.Auth.Types


linkedServices :: OrionUser -> [AuthService]
linkedServices OrionUser{..} = map (\OrionAccount{..} -> _accService) _ouAccounts



