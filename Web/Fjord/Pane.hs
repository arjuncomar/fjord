module Web.Fjord.Pane where

import Data.UUID
import Data.UUID.V4
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp (..))
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame

import Web.Fjord.Types
import Web.Fjord.History
import Web.Fjord.Web

import Control.Lens 
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Applicative

import Data.Text
import qualified Data.Map as Map
import Data.Monoid


createPane :: Web ()
createPane = do
  window <- liftIO Gtk.windowNew
  swView <- liftIO $ Gtk.scrolledWindowNew Nothing Nothing
  wv     <- liftIO webViewNew
  vpane  <- liftIO Gtk.vPanedNew
  sb     <- liftIO Gtk.statusbarNew
  uid    <- liftIO nextRandom

  liftIO $ Gtk.set window [ Gtk.containerChild := vpane ]
  liftIO $ Gtk.set swView [ Gtk.containerChild := wv ]

  liftIO $ Gtk.panedPack1 vpane swView True True
  liftIO $ Gtk.panedPack2 vpane sb False True

  liftIO $ window  `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False 
  liftIO $ wv `Gtk.on` loadCommitted $ \wf -> do
    muri <- webFrameGetUri wf
    maybe (return ()) triggeredLoadUri muri

  liftIO $ Gtk.widgetShowAll window

  -- explicit put instead of modification of existing pane.
  -- we want createPane to dump the current state
  -- and use the new pane instead. This prevents forcing
  -- the fake pane we had to supply to runRWST.
  
  put $ Pane { _uuid = uid
             , _history = History [] []
             , _currentUri = ""
             , _window = window
             , _windowPane = swView
             , _webView = wv }


-- The web frame has triggered a new URI to be loaded
-- update the history and currentURI but don't trigger another load.
triggeredLoadUri :: Text -> IO ()
triggeredLoadUri uri = runWeb $ do
  curr <- use currentUri
  unless (curr == uri) $ do
    unless (curr == "") $ push currentUri $ history.backward
    currentUri .= uri

loadUri :: Text -> Web ()
loadUri uri = do
  pane <- get
  liftIO $ webViewLoadUri (pane ^. webView) uri
