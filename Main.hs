import qualified Graphics.UI.Gtk as Gtk
import Control.Monad.IO.Class
import Web.Fjord.KeyBinds
import Web.Fjord.Pane
import Web.Fjord.Types
import Web.Fjord.Web
import qualified Data.Map as Map
import Control.Monad.RWS
import Web.Fjord.Modifier

defaultConfig = FjordConfig {
  _keybinds = Map.fromList [ createKeyBind [] (Just 'k') scrollUp 
                           , createKeyBind [] (Just 'j') scrollDown
                           , createKeyBind [shift] (Just ':') showEntry]
}
main = fjord defaultConfig

