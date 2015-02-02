import qualified Graphics.UI.Gtk as Gtk
import Data.Text
import Control.Monad.IO.Class
import Web.Fjord.KeyBinds
import Web.Fjord.Pane
import Web.Fjord.Types
import Web.Fjord.Web
import Control.Lens ((^.))
import Data.UUID
import qualified Data.Map as Map
import Control.Monad.RWS
import Web.Fjord.Modifier

main = void $ runRWST fjord () emptyPane

fjord :: Web ()
fjord = liftIO Gtk.initGUI >> createPane >> do
  pane <- get
  liftIO $ initWeb () pane ()
  let keyBindings = Map.fromList [ createKeyBind []      (Just 'k') scrollUp
                                 , createKeyBind []      (Just 'j') scrollDown 
                                 , createKeyBind [shift] (Just 'h') navigateBack
                                 , createKeyBind [shift] (Just 'l') navigateForward 
                                 , createKeyBind [shift] (Just ':') openCommandEntry ]

  addKeyBindings keyBindings
      
  loadUri "https://www.reddit.com"

  liftIO Gtk.mainGUI
