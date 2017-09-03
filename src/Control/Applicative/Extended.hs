{-# LANGUAGE LambdaCase          #-}

module Control.Applicative.Extended (
  module Control.Applicative,
  module Control.Applicative.Extended
) where

import Control.Applicative

whenM :: Monad m => m Bool -> m () -> m ()
whenM pred f = pred >>= \case
  True -> f
  False -> return ()
