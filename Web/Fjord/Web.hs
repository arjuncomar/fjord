module Web.Fjord.Web where

import Data.Global
import Web.Fjord.Types
import Data.Monoid
import Data.IORef
import Control.Monad.RWS
import Control.Applicative

globalPane :: IORef Pane
globalPane = declareIORef "pane" emptyPane

globalConfig :: IORef Config
globalConfig = declareIORef "config" ()

globalLog :: IORef Log
globalLog = declareIORef "log" ()

initWeb :: Config -> Pane -> Log -> IO ()
initWeb c p l = do
  writeIORef globalConfig c
  writeIORef globalPane p
  writeIORef globalLog l

runWeb :: Web () -> IO ()
runWeb wa = void $ do
  pane <- readIORef globalPane
  config <- readIORef globalConfig
  log <- readIORef globalLog

  (pane', log') <- execRWST wa config pane
  
  writeIORef globalPane pane'
  writeIORef globalLog (log <> log')
