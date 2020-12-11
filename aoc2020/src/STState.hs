module STState where

import Board
import Common
import Control.Lens
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
import Data.STRef

newtype STState st s a = STState {runSTState :: STRef s st -> ST s a}
  deriving stock (Generic)

initSTState :: st -> STState st s a -> ST s (a, STRef s st)
initSTState initialState sts = do
  stref <- newSTRef initialState
  a <- runSTState sts stref
  pure (a, stref)

instance Functor (STState st s) where
  fmap f sa = STState $ \stref -> do
    a <- runSTState sa stref
    pure $ f a

instance Applicative (STState st s) where
  fab <*> fa = STState $ \stref -> do
    ab <- runSTState fab stref
    a <- runSTState fa stref
    pure $ ab a

  pure a = STState $ \_ -> pure a

instance Monad (STState st s) where
  ma >>= amb = STState $ \stref -> do
    a <- runSTState ma stref
    (runSTState $ amb a) stref

instance MonadState st (STState st s) where
  get = STState $ \stref -> do
    readSTRef stref
  put v = STState $ \stref -> do
    writeSTRef stref v

liftS :: ST s a -> STState st s a
liftS st = STState $ const st
