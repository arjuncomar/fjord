module Web.Fjord.KeyBinds where

import Web.Fjord.Types
import Web.Fjord.Pane
import Web.Fjord.History
import Web.Fjord.Web
import Web.Fjord.Modifier
import Web.Fjord.Utils

import Graphics.UI.Gtk hiding (on, get, Modifier(..))
import qualified Graphics.UI.Gtk as Gtk (on, get, Modifier(..))
import Graphics.UI.Gtk.WebKit.WebView 

import Control.Lens ((^.), at, view, (&), use, (.=), _Unwrapped)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Applicative

import Data.Maybe
import qualified Data.Map as Map
import Data.List
import Data.Char
import qualified Data.Set as Set


createKeyBind :: [Modifier] -> Maybe Char -> Web () -> KeyBind
createKeyBind mods key action = ((Set.fromList mods, key), action)

addKeyBindings :: Map.Map (Set.Set Modifier, Maybe Char) (Web ()) -> Web ()
addKeyBindings kbm = void $ do
  swView <- use windowPane
  liftIO . Gtk.on swView keyPressEvent . tryEvent $ do
    kv <- eventKeyVal
    mods <- eventModifier
    runAction $ kbm^.at (createKey mods kv)
      where runAction = liftIO . whenJust runWeb
            createKey mods kv = (convert mods, toLower <$> keyToChar kv)

adjust :: Attr ScrolledWindow Adjustment -> BinOp Double -> Web ()
adjust axis f = do
  swView <- use windowPane
  adj <- liftIO $ swView `Gtk.get` axis
  v <- liftIO $ adjustmentGetValue adj
  step <- liftIO $ adjustmentGetStepIncrement adj
  liftIO $ adjustmentSetValue adj (f v step)
  liftIO $ adjustmentValueChanged adj

scroll :: Direction2D -> Web ()
scroll U = adjust scrolledWindowVAdjustment (-)
scroll D = adjust scrolledWindowVAdjustment (+)
scroll L = adjust scrolledWindowHAdjustment (-)
scroll R = adjust scrolledWindowHAdjustment (+)

scrollUp, scrollDown    :: Web () 
scrollLeft, scrollRight :: Web ()
scrollUp    = scroll U
scrollDown  = scroll D
scrollLeft  = scroll L
scrollRight = scroll R

navigate :: Direction1D -> Web ()
navigate d = do
  prevUri <- pop (history.dirLens d)
  case prevUri of
    [uri] -> do
      push currentUri $ history.dirLens' d
      currentUri .= uri
      loadUri uri
    otherwise -> emptyM
  where dirLens Prev = backward
        dirLens Next = forward
        dirLens' Prev = forward
        dirLens' Next = backward

navigateBack, navigateForward :: Web ()
navigateBack = navigate Prev
navigateForward = navigate Next
