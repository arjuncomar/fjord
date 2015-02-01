{-# LANGUAGE Rank2Types #-}
module Web.Fjord.History where

import Web.Fjord.Types
import Control.Lens
import Control.Arrow ((&&&))
import Control.Monad.State

push :: (MonadState s m) => Lens' s a -> Lens' s [a] -> m ()
push l l' = do e <- use l
               es <- use l'
               l' .= e:es

pop :: (MonadState s m) => Traversal' s [a] -> m [a]
pop l = do (h, t) <- use l & liftM (take 1 &&& drop 1)
           l .= t
           return h
