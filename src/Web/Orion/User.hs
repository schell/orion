{-# LANGUAGE RecordWildCards #-}
module Web.Orion.User where

import Web.Orion.Types


linkedServices :: OrionUser -> [AuthService]
linkedServices OrionUser{..} = map (\OrionAccount{..} -> _accService) _ouAccounts

