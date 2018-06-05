module Main where

import Control.Lens
import Data.Monoid
import Game.GoreAndAsh
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Network.Backend.TCP
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Logging
import Options.Applicative

import Game

-- | CLI options of server
data Options = Options {
  optionHostName :: !HostName    -- ^ Host of server
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

-- | Execute server with given options
server :: Options -> IO ()
server Options{..} = withNetwork $ do
  mres <- runGM $ runLoggerT . runNetworkT netopts . runSyncT sopts $ serverGame
  case mres of
    Left er -> print $ renderNetworkError er
    Right _ -> pure ()
  where
    sopts = defaultSyncOptions & syncOptionsRole .~ SyncMaster
    tcpOpts = TCPBackendOpts {
        tcpHostName = optionHostName
      , tcpServiceName = optionService
      , tcpParameters = defaultTCPParameters
      , tcpDuplexHints = defaultConnectHints
      }
    netopts = (defaultNetworkOptions tcpOpts) {
        networkOptsDetailedLogging = False
      }

    serverGame :: AppMonad Spider ()
    serverGame = do
      e <- getPostBuild
      logInfoE $ ffor e $ const $ "Started to listen port " <> showl optionService <> " ..."

      connE <- peerConnected
      logInfoE $ ffor connE $ const $ "Peer is connected..."

      discE <- peerDisconnected
      logInfoE $ ffor discE $ const $ "Peer is disconnected..."

      someErrorE <- networkSomeError
      sendErrorE <- networkSendError
      logWarnE $ ffor someErrorE $ \er -> "Network error: " <> showl er
      logWarnE $ ffor sendErrorE $ \er -> "Network send error: " <> showl er

      _ <- playGame
      return ()

main :: IO ()
main = execParser opts >>= server
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Start Gore & Ash server demo"
     <> header "gore-and-ash-demo-server - server app for engine demo" )
