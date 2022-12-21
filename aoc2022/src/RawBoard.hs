{-# LANGUAGE LiberalTypeSynonyms #-}
module RawBoard where
import Board

newtype RawBoard a = RawBoard {board :: [[a]]}

-- instance (Monad m) => Board RawBoard e m where
--   getWidth = pure . Width . length . head  . board
--   getHeight = pure . Height . length . board
--   newBoard = pure . RawBoard
--   unsafeGet Position{..} RawBoard{..} = pure $ board !! y.getHeight' !! x.getWidth'
--   unsafeSet Position{..} el RawBoard{..} =

