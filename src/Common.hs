module Common (maybeRead, raiseUserError, splitStdGenN)
  where

import Control.Monad.IO.Class
import System.Random
import Control.Arrow (second)

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
  [(x, "")] -> Just x
  _         -> Nothing

raiseUserError :: MonadIO m => String -> m a
raiseUserError = liftIO . ioError . userError

-- n-way split of StdDen
splitStdGenN :: Int -> StdGen -> [StdGen]
splitStdGenN k = take k . splits
  where splits = uncurry (:) . second splits . split
