module Web.Fjord where

import Graphics.UI.Gtk hiding                     (set, on, get, Statusbar, Modifier)

import Web.Fjord.Pane
import Web.Fjord.Types

import Control.Lens
import Control.Monad.State.Strict
import Control.Applicative hiding                 (empty)
import Control.Concurrent.Async                   (async, wait)

import Data.Text hiding                           (empty, toLower)
import Data.Monoid
import Data.List.Zipper

import MVC
import MVC.Updates as MVC

modelLoadUri :: Text -> ListT (State Pane) Pane
modelLoadUri uri = do
  curr <- use currentUri
  unless (curr == uri) $ do
    -- update history if the old uri is not blank
    -- This should only happen when the pane is first created.
    unless (curr == "") $ history %= push uri
    currentUri .= uri
  get

modelDisplay :: FjordInput -> ListT (State Pane) Pane
modelDisplay (Uri uri) = modelLoadUri uri
modelDisplay _         = get

modelRunAction :: FjordInput -> ListT (State Pane) (IO ())
modelRunAction (Action act) = do
  pane <- get
  return (act pane)
modelRunAction _ = return (return ())

modelPane :: FjordInput -> ListT (State Pane) FjordOutput
modelPane input = fmap Display (modelDisplay input)
                <> fmap RunAction (modelRunAction input)

vstatusbar :: Managed (View FjordOutput)
vstatusbar = return $ handles (_Display.statusbar) $ asSink updateStatusbar

displayUri :: Managed (View FjordOutput)
displayUri = liftIO . return . handles _Display $ asSink loadUri

runAction :: Managed (View FjordOutput)
runAction = liftIO . return . handles _RunAction $ asSink id

viewFjord :: Managed (View FjordOutput)
viewFjord = vstatusbar <> displayUri <> runAction

controlFjord :: Pane -> FjordConfig -> Managed (Controller FjordInput)
controlFjord pane config = updates unbounded (attachWV $ pane^.webView)
                         <> updates unbounded (entry pane)
                         <> updates unbounded (addKBs config pane)

modelFjord :: Model Pane FjordInput FjordOutput
modelFjord = asPipe . loop $ modelPane

fjordBrowser :: Pane -> FjordConfig -> Managed (Managed (Controller FjordInput), Managed (View FjordOutput))
fjordBrowser pane config = managed $ \k -> k (controlFjord pane config, viewFjord)

fjord :: FjordConfig -> IO ()
fjord config = do
  _    <- initGUI
  pane <- mkPane
  a    <- async $ runMVC pane modelFjord $ do
    (controller, viewer) <- fjordBrowser pane config
    (,) <$> viewer <*> controller
  widgetShowAll (pane^.window)
  mainGUI
  void $ wait a
