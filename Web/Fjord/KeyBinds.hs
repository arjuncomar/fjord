module Web.Fjord.KeyBinds where

import Web.Fjord.Types

import Graphics.UI.Gtk (Attr, ScrolledWindow, Adjustment,
                        adjustmentGetValue, adjustmentGetStepIncrement,
                        adjustmentSetValue, adjustmentValueChanged,
                        scrolledWindowVAdjustment, scrolledWindowHAdjustment,
                        postGUIAsync, widgetShowNow, widgetGrabFocus)
import qualified Graphics.UI.Gtk as Gtk (get)

import Control.Lens ((^.))
import qualified Data.Set as Set (fromList)

createKeyBind :: [Modifier] -> Maybe Char -> (Pane -> IO ()) -> KeyBind
createKeyBind ms k act = ((Set.fromList ms, k), act)

adjust :: Attr ScrolledWindow Adjustment -> BinOp Double -> Pane -> IO ()
adjust axis f pane = do
  adj <- (pane^.windowPane) `Gtk.get` axis
  v <- adjustmentGetValue adj
  step <- adjustmentGetStepIncrement adj
  adjustmentSetValue adj (f v step)
  adjustmentValueChanged adj

scroll :: Direction2D -> Pane -> IO ()
scroll U = adjust scrolledWindowVAdjustment (-)
scroll D = adjust scrolledWindowVAdjustment (+)
scroll L = adjust scrolledWindowHAdjustment (-)
scroll R = adjust scrolledWindowHAdjustment (+)

scrollUp, scrollDown    :: Pane -> IO () 
scrollLeft, scrollRight :: Pane -> IO ()
scrollUp    = scroll U
scrollDown  = scroll D
scrollLeft  = scroll L
scrollRight = scroll R

showEntry :: Pane -> IO ()
showEntry pane = postGUIAsync $ widgetShowNow (pane^.gtkentry) >> widgetGrabFocus (pane^.gtkentry)
