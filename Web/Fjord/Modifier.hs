module Web.Fjord.Modifier where

import qualified Graphics.UI.Gtk as Gtk
import Web.Fjord.Types
import Control.Lens
import qualified Data.Set as Set

shift, lock, control, alt, super, hyper, meta :: Modifier
shift = Modifier Gtk.Shift
lock = Modifier Gtk.Lock
control = Modifier Gtk.Control
alt = Modifier Gtk.Alt
super = Modifier Gtk.Super
hyper = Modifier Gtk.Hyper
meta = Modifier Gtk.Meta

button1, button2, button3, button4, button5 :: Modifier
button1 = Modifier Gtk.Button1
button2 = Modifier Gtk.Button2
button3 = Modifier Gtk.Button3
button4 = Modifier Gtk.Button4
button5 = Modifier Gtk.Button5

convert :: [Gtk.Modifier] -> Set.Set Modifier
convert = Set.fromList . map (view _Unwrapped)
