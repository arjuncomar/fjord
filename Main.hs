module Main where

import Web.Fjord
import Web.Fjord.Types
import Web.Fjord.KeyBinds
import Web.Fjord.Modifier
import qualified Data.Map as Map

defaultConfig = FjordConfig {
  _keybinds = Map.fromList [ createKeyBind [] (Just 'k') scrollUp 
                           , createKeyBind [] (Just 'j') scrollDown
                           , createKeyBind [shift] (Just ':') showEntry]
}
main = fjord defaultConfig

