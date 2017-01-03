module Main where

import Control.Lens
import Data.Monoid
import Game.GoreAndAsh
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Logging
import Network.Socket
import Options.Applicative

import Game

-- | CLI options of server
data Options = Options {
  optionPort :: !PortNumber -- ^ Port of server
}

-- | Parser of CLI options
optionsParser :: Parser Options
optionsParser = Options
  <$> option auto (
       long "port"
    <> metavar "PORT_NUMBER"
    <> help "port of remote game server"
    )

-- | Execute server with given options
server :: Options -> IO ()
server Options{..} = do
  runSpiderHost $ hostApp $ runModule opts clientGame
  where
    opts = defaultSyncOptions netopts & syncOptionsRole .~ SyncMaster
    netopts = (defaultNetworkOptions ()) {
        networkDetailedLogging = False
      }

    clientGame :: AppMonad ()
    clientGame = do
      e <- getPostBuild
      loggingSetDebugFlag False
      listenE <- dontCare =<< (serverListen $ ffor e $ const $ ServerListen {
          listenAddress = SockAddrInet optionPort 0
        , listenMaxConns = 100
        , listenChanns = 3
        , listenIncoming = 0
        , listenOutcoming = 0
        })
      logInfoE $ ffor listenE $ const $ "Started to listen port " <> showl optionPort <> " ..."

      connE <- peerConnected
      logInfoE $ ffor connE $ const $ "Peer is connected..."

      discE <- peerDisconnected
      logInfoE $ ffor discE $ const $ "Peer is disconnected..."

      game

main :: IO ()
main = execParser opts >>= server
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Start Gore & Ash server demo"
     <> header "gore-and-ash-demo-server - server app for engine demo" )
