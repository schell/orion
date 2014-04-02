module Web.Orion.Types where

import           Web.Scotty.Trans
import           Control.Monad.Reader
import           Data.Configurator.Types
import           Data.Text.Lazy
import           Data.Map
import           Data.ByteString
import           Data.Pool
import           Database.HDBC.Sqlite3
import           Control.Concurrent.MVar

type AuthStates = (Map ByteString Text)

type AuthStatesVar = MVar AuthStates

data OrionApp = OrionApp { _oCfg          :: Config
                         , _oAuthStates   :: AuthStatesVar
                         , _oUserConnPool :: Pool Connection
                         }

type OrionM = ScottyT Text (ReaderT OrionApp IO)

type ActionOM = ActionT Text (ReaderT OrionApp IO)

type ReaderOrion m = m Text (ReaderT OrionApp IO)

