module Web.Fjord.KeyBinds where

import Web.Fjord.Types
import Web.Fjord.Pane
import Web.Fjord.Web
import Web.Fjord.Modifier
import Web.Fjord.Utils

import Graphics.UI.Gtk (keyPressEvent, tryEvent, eventKeyVal, eventModifier,
                        keyToChar, Attr, ScrolledWindow, Adjustment,
                        adjustmentGetValue, adjustmentGetStepIncrement,
                        adjustmentSetValue, adjustmentValueChanged,
                        scrolledWindowVAdjustment, scrolledWindowHAdjustment)
import qualified Graphics.UI.Gtk as Gtk (on, get)

import Control.Lens ((^.), at, use, (%=))
import qualified Data.Set as Set (fromList, Set)
import qualified Data.Map as Map
import Control.Monad(void, join)
import Control.Monad.IO.Class(liftIO)
import Data.Char(toLower)
import Control.Applicative((<$>))
import Data.List.Zipper (right, left)
import qualified Data.Text as T

createKeyBind :: [Modifier] -> Maybe Char -> Web () -> KeyBind
createKeyBind ms k act = ((Set.fromList ms, k), act)

addKeyBindings :: Map.Map (Set.Set Modifier, Maybe Char) (Web ()) -> Web ()
addKeyBindings kbm = void $ do
  swView <- use windowPane
  liftIO . Gtk.on swView keyPressEvent . tryEvent $ do
    kv <- eventKeyVal
    ms <- eventModifier
    runAction $ kbm^.at (createKey ms kv)
      where runAction = liftIO . whenJust runWeb
            createKey ms kv = (convert ms, toLower <$> keyToChar kv)

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
  history %= dir d
  join $ loadUri <$> use currentUri
  where dir Prev = right
        dir Next = left

navigateBack, navigateForward :: Web ()
navigateBack = navigate Prev
navigateForward = navigate Next

openUri :: T.Text -> Web ()
openUri = loadUri

openCommandEntry :: Web ()
openCommandEntry = addCommandEntry
