module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Monoid
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Network.Backend.TCP
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Time
import Options.Applicative as OA
import SDL.TTF.FFI (TTFFont)

import qualified SDL.TTF as TTF

import Game
import Graphics

import Paths_gore_and_ash_demo

-- | CLI options of client
data Options = Options {
  optionHostName :: !HostName    -- ^ Host address of server
, optionService  :: !ServiceName -- ^ Port of server
, optionCheating :: !Bool        -- ^ Simulate hacked client
, optionFont     :: !(Maybe FilePath)    -- ^ Path to font file
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
  <*> OA.switch (
       long "cheating"
    <> help "simulate hacked client to test rejecting server behavior"
    )
  <*> optional (strOption $
       long "font"
    <> metavar "TTF_FILE_PATH"
    <> help "font to use to render text"
    )

-- | Load font either from built in or from specified file
loadFont :: MonadIO m => Maybe FilePath -> m TTFFont
loadFont mfile = liftIO $ do
  filename <- case mfile of
    Nothing -> getDataFileName "media/AnonymousPro-Regular.ttf"
    Just n -> return n
  TTF.openFont filename 40

-- | Execute client with given options
client :: Options -> IO ()
client Options{..} = TTF.withInit $ do
  runSpiderHost $ hostApp $ runModule opts clientGame
  where
    opts = defaultSyncOptions netopts & syncOptionsRole .~ SyncSlave
    tcpOpts = TCPBackendOpts {
        tcpHostName = "localhost"
      , tcpServiceName = ""
      , tcpParameters = defaultTCPParameters
      , tcpDuplexHints = defaultConnectHints
      }
    netopts = (defaultNetworkOptions tcpOpts ()) {
        networkOptsDetailedLogging = False
      }

    clientGame :: AppMonad Spider ()
    clientGame = do
      loggingSetDebugFlag True
      e <- getPostBuild
      let EndPointAddress addr = encodeEndPointAddress optionHostName optionService 0
      connectedE <- clientConnect $ ffor e $ const (addr, defaultConnectHints)
      conErrorE <- networkConnectionError
      logInfoE $ ffor connectedE $ const "Connected to server!"
      logErrorE $ ffor conErrorE $ \er -> "Failed to connect: " <> showl er
      _ <- switchAppHost (pure mempty) $ ffor connectedE $ const playPhase
      return ()

    playPhase :: AppMonad Spider ()
    playPhase = do
      font <- loadFont optionFont
      rec
        w <- createMainWindow (const () <$> redrawE) (drawFrame gameDyn font) defaultWindowCfg
        gameDyn <- playGame w optionCheating
        redrawE <- alignWithFps 60 $ updated gameDyn
      return ()

main :: IO ()
main = execParser opts >>= client
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Start Gore & Ash client demo"
     <> header "gore-and-ash-demo-client - client app for engine demo" )
