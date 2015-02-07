module Web.Fjord.History where

import Web.Fjord.Types
import Control.Lens
import Data.List.Zipper

goForward :: Pane -> Pane
goForward = history %~ left

goBackward :: Pane -> Pane
goBackward = history %~ right
