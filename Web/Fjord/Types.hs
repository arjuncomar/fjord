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
import Data.Text hiding (replace, map)
import Data.Typeable
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Zipper

import Test.QuickCheck

data Direction2D = U | D | L | R deriving (Enum, Eq, Ord, Read, Show, Bounded)
data Direction1D = Prev | Next deriving (Enum, Eq, Ord, Read, Show, Bounded)
type BinOp a = a -> a -> a 

data Statusbar = Statusbar { _gtkbar    :: Gtk.Statusbar
                           , _contextId :: Int 
                           , _message   :: Text }
makeLenses ''Statusbar

data Pane = Pane { _uuid       :: UUID
                 , _history    :: Zipper Text
                 , _window     :: Gtk.Window
                 , _windowPane :: Gtk.ScrolledWindow 
                 , _webView    :: WebView
                 , _statusbar  :: Statusbar 
                 , _sboverlay  :: Gtk.Overlay 
                 , _gtkentry   :: Gtk.Entry} deriving (Typeable)
makeLensesWith (lensRules & generateSignatures .~ False) ''Pane
uuid :: Getter Pane UUID

instance Arbitrary Pane where
  arbitrary = do
    uid <- choose (undefined, undefined)  -- range is ignored by randomR for uuids
    urls <- arbitrary
    let hist = fromList $ map pack urls
    return Pane {
      _uuid = uid
    , _history = hist
    , _window = undefined
    , _windowPane = undefined
    , _sboverlay = undefined
    , _webView = undefined
    , _statusbar = undefined
    , _gtkentry = undefined
    }

currentUri :: Lens' Pane Text
currentUri = history . lens cursor setCursor

setCursor :: Zipper Text -> Text -> Zipper Text
setCursor z u | emptyp z = replace u z 
              | otherwise = insert u z

type Config = ()
type Log = ()
type Web = RWST Config Log Pane IO
type KeyBind = ((Set.Set Modifier, Maybe Char), Pane -> IO ())

key :: Lens' KeyBind (Maybe Char)
key = _1._2

mods :: Lens' KeyBind (Set.Set Modifier)
mods = _1._1

action :: Lens' KeyBind (Pane -> IO ())
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

data FjordInput = Uri Text | Action (Pane -> IO ())
data FjordOutput = Display Pane | RunAction (IO ())

makePrisms ''FjordOutput
makePrisms ''FjordInput

data FjordConfig = FjordConfig {
                     _keybinds :: Map.Map (Set.Set Modifier, Maybe Char) (Pane -> IO ()) 
                   }
makeLenses ''FjordConfig
