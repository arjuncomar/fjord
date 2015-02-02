{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Web.Fjord.Types where

import Control.Monad.RWS
import Control.Lens

import Data.UUID
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.WebKit.WebView
import System.Glib.Flags
import Data.Text
import Data.Typeable
import qualified Data.Set as Set

data Direction2D = U | D | L | R deriving (Enum, Eq, Ord, Read, Show, Bounded)
data Direction1D = Prev | Next deriving (Enum, Eq, Ord, Read, Show, Bounded)
type BinOp a = a -> a -> a 

data History = History { _forward :: [Text]
                       , _backward :: [Text] } deriving Show
makeLenses ''History

data Statusbar = Statusbar { _gtkbar :: Gtk.Statusbar
                           , _contextId :: Int }
makeLenses ''Statusbar

data Pane = Pane { _uuid       :: UUID
                 , _history    :: History
                 , _currentUri :: Text
                 , _window     :: Gtk.Window
                 , _windowPane :: Gtk.ScrolledWindow 
                 , _webView    :: WebView
                 , _statusbar  :: Statusbar 
                 , _sboverlay  :: Gtk.Overlay 
                 , _commandEntry :: Maybe Gtk.Entry } deriving (Typeable)
makeLensesWith (lensRules & generateSignatures .~ False) ''Pane
uuid :: Getter Pane UUID

instance Show Pane where
  show p = "Pane { _uuid = " <> (show $ _uuid p) <> "\n"
        <> ", _history = " <> (show $ _history p) <> "\n"
        <> ", _currentUri = " <> (show $ _currentUri p) <> " }"

type Config = ()
type Log = ()
type Web = RWST Config Log Pane IO
type KeyBind = ((Set.Set Modifier, Maybe Char), Web ())

key :: Lens' KeyBind (Maybe Char)
key = _1._2

mods :: Lens' KeyBind (Set.Set Modifier)
mods = _1._1

action :: Lens' KeyBind (Web ())
action = _2

-- pane that should never be forced
-- used so we can have a pane before initGUI is called.
-- This is necessary to use runRWST.
emptyPane :: Pane
emptyPane = undefined

newtype Modifier = Modifier { getModifier :: Gtk.Modifier } deriving (Bounded, Enum, Eq, Show, Flags)
makeWrapped ''Modifier

instance Ord Modifier where
  a <= b = fromEnum a <= fromEnum b
