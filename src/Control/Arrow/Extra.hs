module Control.Arrow.Extra (
  module Control.Arrow,
  module Control.Arrow.Extra
) where

import Control.Arrow

-- | Useful for splitting arrows just with infix operators:
--
-- a1 &&& a2 &&&. a3 :: Arrow a => a b (c, d, e)
(&&&.) :: Arrow a => a b (c, d) -> a b e -> a b (c, d, e)
a1 &&&. a2 = (a1 &&& a2) >>^ (\((c, d), e) -> (c, d, e))
infixr 2 &&&.

