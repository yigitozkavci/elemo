{-# LANGUAGE LambdaCase          #-}

module Control.Applicative.Extra (
  module Control.Applicative,
  module Control.Applicative.Extra
) where

import Control.Applicative

whenM :: Monad m => m Bool -> m () -> m ()
whenM pred f = pred >>= \case
  True -> f
  False -> return ()
