module Web.Fjord.Pane where

import Data.UUID.V4
import Graphics.UI.Gtk hiding                     (set, on, get, Statusbar, Modifier)
import qualified Graphics.UI.Gtk                  as Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebNavigationAction

import Web.Fjord.Types
import Web.Fjord.Utils
import Web.Fjord.Modifier

import Control.Lens
import Control.Monad.State.Strict
import Control.Applicative hiding                 (empty)

import Data.Text hiding                           (empty, toLower)
import Data.List.Zipper
import Data.Maybe
import Data.Char

import MVC
import MVC.Updates as MVC
import Control.Foldl                              (lastDef)

mkPane :: IO Pane
mkPane = do
  w      <- Gtk.windowNew
  swView <- Gtk.scrolledWindowNew Nothing Nothing
  vbox   <- Gtk.vBoxNew False 2
  ovl    <- Gtk.overlayNew
  uid    <- nextRandom
  sb     <- createStatusbar
  wv     <- webViewNew
  e      <- mkGtkEntry ovl

  Gtk.set  w     [ Gtk.containerChild       := vbox ]
  Gtk.set swView [ Gtk.containerChild       := wv ]
  Gtk.set vbox   [ Gtk.containerBorderWidth := 0
                 , Gtk.widgetMarginTop      := 0
                 , Gtk.widgetMarginBottom   := 0
                 , Gtk.widgetMarginRight    := 0
                 , Gtk.widgetMarginLeft     := 0
                 , Gtk.boxSpacing           := 0]
  Gtk.set ovl    [ Gtk.containerChild       := (sb^.gtkbar)
                 , Gtk.containerBorderWidth := 0
                 , Gtk.widgetMarginTop      := 0
                 , Gtk.widgetMarginBottom   := 0
                 , Gtk.widgetMarginRight    := 0
                 , Gtk.widgetMarginLeft     := 0 ]

  Gtk.boxPackStart vbox swView Gtk.PackGrow 0
  Gtk.boxPackEnd   vbox ovl    Gtk.PackNatural 0

  _ <- w  `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False 

  Gtk.widgetShowAll w

  return Pane { _uuid         = uid
              , _history      = fromList ["https://www.reddit.com"]
              , _window       = w
              , _windowPane   = swView
              , _webView      = wv
              , _statusbar    = sb
              , _sboverlay    = ovl 
              , _gtkentry     = e }

createStatusbar :: IO Statusbar
createStatusbar = do
  sb     <- Gtk.statusbarNew
  Gtk.set sb     [ Gtk.widgetMarginTop      := 0
                 , Gtk.widgetMarginBottom   := 0
                 , Gtk.widgetMarginRight    := 0
                 , Gtk.widgetMarginLeft     := 0
                 , Gtk.containerBorderWidth := 0 ]
  cid <- Gtk.statusbarGetContextId sb ("url" :: Text)
  sbb <- Gtk.statusbarGetMessageArea sb
  Gtk.set sbb    [ Gtk.widgetMarginTop      := 0
                 , Gtk.widgetMarginBottom   := 0
                 , Gtk.widgetMarginLeft     := 0
                 , Gtk.widgetMarginRight    := 0
                 , Gtk.containerBorderWidth := 0 ]
  return $ Statusbar sb (fromIntegral cid) ""


updateStatusbar :: Statusbar -> IO ()
updateStatusbar sb = void $ do
  Gtk.statusbarRemoveAll (sb^.gtkbar) (sb^.contextId)
  Gtk.statusbarPush (sb^.gtkbar) (fromIntegral $ sb^.contextId) (sb^.message)

loadUri :: Pane -> IO ()
loadUri pane = webViewLoadUri (pane^.webView) (pane^.currentUri)

attachWV :: WebView -> Updatable FjordInput
attachWV wv = fmap Uri $ MVC.on (lastDef "") $ managed $ \k -> do
  (output, input) <- spawn unbounded
  _ <- Gtk.on wv navigationPolicyDecisionRequested $ \_ _ wbna _ -> do
    uri <- webNavigationActionGetOriginalUri wbna
    reason <- webNavigationActionGetReason wbna
    case reason of
      WebNavigationReasonLinkClicked -> void . atomically $ send output uri
      _ -> emptyM
    return False
  k (asInput input)

entry :: Pane -> Updatable FjordInput
entry pane = fmap Uri $ MVC.on (lastDef "") $ managed $ \k -> do
  (output, input) <- spawn unbounded
  void . Gtk.on (pane^.gtkentry) entryActivated . liftIO . postGUISync $ do
    uri <- entryGetText (pane^.gtkentry)
    _   <- atomically (send output uri)
    entrySetText (pane^.gtkentry) ("" :: Text)
    widgetHide (pane^.gtkentry)
    widgetGrabFocus (pane^.windowPane)
  k (asInput input)

addKBs :: FjordConfig -> Pane -> Updatable FjordInput
addKBs config pane = fmap Action $ MVC.on (lastDef (const emptyM)) $ managed $ \k -> do
  (output, input) <- spawn unbounded
  _ <- Gtk.on (pane^.windowPane) keyPressEvent . tryEvent $ do
    kv <- eventKeyVal
    ms <- eventModifier
    _ <- liftIO . atomically . send output . fromMaybe (const emptyM) $ config^.keybinds.at (createKey ms kv) 
    return ()
  k (asInput input)
    where createKey ms kv = (convert ms, toLower <$> keyToChar kv)

mkGtkEntry :: Gtk.Overlay -> IO Gtk.Entry
mkGtkEntry ovl = do
  e <- entryNew
  overlayAdd ovl e
  Gtk.set e [ widgetMarginTop    := 0
                , widgetMarginBottom := 0
                , widgetMarginLeft   := 0
                , widgetMarginRight  := 0 ]
  widgetHide e
  return e
