module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.IORef
import Data.Monoid
import Game.GoreAndAsh
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.Time
import Network.Socket
import Options.Applicative

import Game
import Game.Monad
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
      loggingSetDebugFlag True
      addr <- resolveServer optionHostName optionService
      e <- getPostBuild
      connectedE <- dontCare =<< (clientConnect $ ffor e $ const $ ClientConnect {
          clientAddrr = addr
        , clientChanns = 3
        , clientIncoming = 0
        , clientOutcoming = 0
        })
      logInfoE $ ffor connectedE $ const "Connected to server!"
      _ <- switchAppHost (pure mempty) $ ffor connectedE $ const playPhase
      return ()

    playPhase :: AppMonad Spider ()
    playPhase = do
      rec
        w <- createMainWindow (const () <$> redrawE) (drawFrame gameDyn) defaultWindowCfg
        gameDyn <- playGame w
        redrawE <- alignWithFps 60 $ updated gameDyn
      return ()

-- | Fire event not frequently as given frame per second ratio.
alignWithFps :: AppFrame t => Int -- ^ FPS (frames per second)
  -> Event t a -- ^ Event that frequently changes
  -> AppMonad t (Event t a) -- ^ Event that changes are aligned with FPS
alignWithFps fps ea = do
  fpsE <- tickEvery . realToFrac $ 1 / (fromIntegral fps :: Double)
  ref <- liftIO $ newIORef Nothing
  performEvent_ $ ffor ea $ liftIO . atomicWriteIORef ref . Just
  alignedE <- performEvent $ ffor fpsE $ const $
    liftIO $ atomicModifyIORef' ref $ \v -> (Nothing, v)
  return $ fmapMaybe id alignedE

main :: IO ()
main = execParser opts >>= client
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Start Gore & Ash client demo"
     <> header "gore-and-ash-demo-client - client app for engine demo" )
