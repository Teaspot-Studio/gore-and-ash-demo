module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Monoid
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Sync
import Network.Socket
import Options.Applicative

import Game
import Graphics

-- | CLI options of client
data Options = Options {
  optionHostName :: !HostName    -- ^ Host address of server
, optionService  :: !ServiceName -- ^ Port of server
}

-- | Parser of CLI options
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (
       long "host"
    <> metavar "HOST_NAME"
    <> help "address of remote game server"
    )
  <*> strOption (
       long "port"
    <> metavar "PORT_NUMBER"
    <> help "port of remote game server"
    )

-- | Find server address by host name or IP
resolveServer :: MonadIO m => HostName -> ServiceName -> m SockAddr
resolveServer host serv = do
  addrInfo <- liftIO $ getAddrInfo Nothing (Just host) (Just serv)
  case addrInfo of
    [] -> fail $ "Cannot resolve server address: " <> host
    (a : _) -> return $ addrAddress a

-- | Execute client with given options
client :: Options -> IO ()
client Options{..} = do
  runSpiderHost $ hostApp $ runModule opts clientGame
  where
    opts = defaultSyncOptions netopts & syncOptionsRole .~ SyncSlave
    netopts = (defaultNetworkOptions ()) {
        networkDetailedLogging = False
      }

    clientGame :: AppMonad Spider ()
    clientGame = do
      addr <- resolveServer optionHostName optionService
      e <- getPostBuild
      connectedE <- dontCare =<< (clientConnect $ ffor e $ const $ ClientConnect {
          clientAddrr = addr
        , clientChanns = 3
        , clientIncoming = 0
        , clientOutcoming = 0
        })
      logInfoE $ ffor connectedE $ const "Connected to server!"
      rec
        w <- createMainWindow (drawFrame gameDyn) defaultWindowCfg
        gameDyn <- playGame w
      return ()

main :: IO ()
main = execParser opts >>= client
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Start Gore & Ash client demo"
     <> header "gore-and-ash-demo-client - client app for engine demo" )
