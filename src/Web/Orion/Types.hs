module Web.Orion.Types where

import           Web.Scotty.Trans
import           Control.Monad.Reader
import           Data.Configurator.Types
import           Data.Text.Lazy
import           Data.Map
import           Data.ByteString
import           Control.Concurrent.MVar

type AuthStates = (Map ByteString Text)

type AuthStatesVar = MVar AuthStates

data OrionApp = OrionApp { _oCfg        :: Config
                         , _oAuthStates :: AuthStatesVar
                         }

type OrionM = ScottyT Text (ReaderT OrionApp IO)

type ActionOM = ActionT Text (ReaderT OrionApp IO)

