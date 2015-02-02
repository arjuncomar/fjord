module Web.Fjord.Utils where

-- empty action for any given monad
-- this is useful enough that it's worth naming.
emptyM :: Monad m => m ()
emptyM = return ()

-- perform the provided action only when a value is available.
-- run the empty action when it's not.
whenJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
whenJust = maybe emptyM
