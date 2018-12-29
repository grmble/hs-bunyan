{- | Bunyan IO Implementation
-}
module System.Log.Bunyan.IO
  ( module System.Log.Bunyan
  , rootLogger
  , consoleHandler
  , noopHandler
  , loggerNames'
  , priorityMap'
  ) where

import Control.Lens.PicoLens (Lens', view)
import Control.Monad (unless)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Text as T
import Data.Time.Clock.System (SystemTime, getSystemTime)
import Network.BSD (getHostName)
import System.IO (hFlush, stdout)
import System.Log.Bunyan
import System.Process.Current (getPid)
import UnliftIO.STM


loggerNames' :: Lens' Logger (TVar (S.HashSet T.Text))
loggerNames' k lg = fmap (\x -> lg {loggerNames = x}) (k (loggerNames lg))

priorityMap' :: Lens' Logger (TVar (M.HashMap T.Text Priority))
priorityMap' k lg = fmap (\x -> lg {priorityMap = x}) (k (priorityMap lg))

-- | Console handler function - prints to console
consoleHandler :: A.Object -> IO ()
consoleHandler obj = do
  LBSC8.putStrLn $ A.encode obj
  hFlush stdout

-- | Handler that does not log anything
noopHandler :: A.Object -> IO ()
noopHandler _ = pure ()

-- | Create a root logger object.
-- |
-- | There should only be one root logger.
-- | It contains the handling function, all
-- | other loggers should be children of the root
-- | logger.
rootLogger ::
     MonadIO m => T.Text -> Priority -> (A.Object -> IO ()) -> m Logger
rootLogger n p h = do
  hn <- liftIO getHostName
  pid <- liftIO getPid
  priMap <- newTVarIO M.empty
  logSet <- newTVarIO S.empty
  return
    Logger
      { name = n
      , context = M.empty
      , handler = h
      , rootContext =
          M.fromList
            [ ("v", A.Number 0)
            , ("hostname", A.String (T.pack hn))
            , ("pid", A.Number $ fromIntegral pid)
            ]
      , priority = intPriority p
      , priorityMap = priMap
      , loggerNames = logSet
      }

-- yes, it's an orphan instance, but i don't want to get more of the
-- implentations into the (supposedly abstract) Bunyan.Class module
instance HasLogger r => MonadBunyan r (ReaderT r IO) where
  childLogger n ctx = do
    lg <- asks (view logger)
    m <- readTVarIO (priorityMap lg)
    s <- readTVarIO (loggerNames lg)
    unless (S.member n s) $ atomically $ modifyTVar (loggerNames lg) (S.insert n)
    let pri = maybe (priority lg) intPriority $ M.lookup n m
    pure lg {name = n, context = M.union ctx (context lg), priority = pri}
  getLoggingTime = liftIO getSystemTime
  handleRecord obj = do
    lg <-asks (view logger)
    liftIO $ handler lg obj
