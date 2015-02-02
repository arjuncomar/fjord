module Web.Fjord.Pane where

import Data.UUID
import Data.UUID.V4
import Graphics.UI.Gtk as Gtk hiding (set, on)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebNavigationAction

import Web.Fjord.Types
import Web.Fjord.History
import Web.Fjord.Web
import Web.Fjord.Utils

import Control.Lens 
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Applicative

import Data.Text
import qualified Data.Map as Map
import Data.Monoid

mkPane :: IO Pane
mkPane = do
  window <- Gtk.windowNew
  swView <- Gtk.scrolledWindowNew Nothing Nothing
  wv     <- webViewNew
  vbox   <- Gtk.vBoxNew False 2 
  ovl    <- Gtk.overlayNew
  sb     <- Gtk.statusbarNew
  uid    <- nextRandom

  Gtk.set window [ Gtk.containerChild       := vbox ]
  Gtk.set swView [ Gtk.containerChild       := wv ]
  Gtk.set vbox   [ Gtk.containerBorderWidth := 0
                 , Gtk.widgetMarginTop      := 0
                 , Gtk.widgetMarginBottom   := 0
                 , Gtk.widgetMarginRight    := 0
                 , Gtk.widgetMarginLeft     := 0 
                 , Gtk.boxSpacing           := 0]
  Gtk.set ovl    [ Gtk.containerChild       := sb 
                 , Gtk.containerBorderWidth := 0
                 , Gtk.widgetMarginTop      := 0
                 , Gtk.widgetMarginBottom   := 0
                 , Gtk.widgetMarginRight    := 0
                 , Gtk.widgetMarginLeft     := 0 ]

  Gtk.boxPackStart vbox swView Gtk.PackGrow 0
  Gtk.boxPackEnd   vbox ovl    Gtk.PackNatural 0

  Gtk.set sb     [ Gtk.widgetMarginTop    := 0
                 , Gtk.widgetMarginBottom := 0 
                 , Gtk.widgetMarginRight  := 0
                 , Gtk.widgetMarginLeft   := 0
                 , Gtk.containerBorderWidth := 0 ]
  cid <- Gtk.statusbarGetContextId sb ("url" :: Text)

  sbb <- Gtk.statusbarGetMessageArea sb
  Gtk.set sbb    [ Gtk.widgetMarginTop    := 0
                 , Gtk.widgetMarginBottom := 0 
                 , Gtk.widgetMarginLeft   := 0
                 , Gtk.widgetMarginRight  := 0 
                 , Gtk.containerBorderWidth := 0 ]
  Gtk.widgetModifyBg sbb Gtk.StateNormal (Gtk.Color 65535 65535 65535)
  Gtk.containerForeach sbb $ \lbl -> do
    Gtk.boxReorderChild sbb lbl (-1)
    Gtk.widgetModifyFg lbl Gtk.StateNormal (Gtk.Color 30000 30000 30000)
    Gtk.widgetModifyBg lbl Gtk.StateNormal (Gtk.Color 65535 65535 65535)

  window  `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False 
  wv `Gtk.on` navigationPolicyDecisionRequested $ \wf nr wbna wpd -> do
    uri <- webNavigationActionGetOriginalUri wbna
    reason <- webNavigationActionGetReason wbna
    case reason of
      WebNavigationReasonLinkClicked -> triggeredLoadUri uri
      otherwise -> emptyM
    return False

  Gtk.widgetShowAll window

  return Pane { _uuid = uid
              , _history = History [] []
              , _currentUri = ""
              , _window = window
              , _windowPane = swView
              , _webView = wv
              , _statusbar = Statusbar sb (fromIntegral cid) }

createPane :: Web ()
createPane = do
  pane <- liftIO mkPane
  put pane

-- The web frame has triggered a new URI to be loaded
-- update the history and currentURI but don't trigger another load.
triggeredLoadUri :: Text -> IO ()
triggeredLoadUri = runWeb . updatePane
    
updatePane :: Text -> Web ()
updatePane uri = do
  curr <- use currentUri
  unless (curr == uri) $ do
    unless (curr == "") $ push currentUri $ history.backward
    currentUri .= uri
    join $ replaceMessage <$> use (statusbar.gtkbar) 
                          <*> use (statusbar.contextId)
                          <*> use currentUri

loadUri :: Text -> Web ()
loadUri uri = do
  updatePane uri
  updateWeb
  join $ load <$> use webView <*> return uri

load :: WebView -> Text -> Web ()
load wv uri = liftIO $ webViewLoadUri wv uri

clearStatusbar :: Gtk.Statusbar -> Int -> Web ()
clearStatusbar sb cid = liftIO $ Gtk.statusbarRemoveAll sb cid

pushMessage :: Gtk.Statusbar -> Int -> Text -> Web ()
pushMessage sb cid msg = void . liftIO $ Gtk.statusbarPush sb (fromIntegral cid) msg

replaceMessage :: Gtk.Statusbar -> Int -> Text -> Web ()
replaceMessage sb cid msg = clearStatusbar sb cid >> pushMessage sb cid msg
