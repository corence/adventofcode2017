
import Data.Function
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

import Control.Monad.ST

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

-- | Use local mutation for efficiency in creating a table of counts,
-- looping through to update it, and freezing the result to return.
--distribute :: Int -> V.Vector Int -> V.Vector Int
distribute index mv = do
  value <- M.read mv index
  M.modify mv (const 0) index
  pure mv

step :: V.Vector Int -> V.Vector Int
step vector = runST $ (V.thaw vector <&> distribute 0) >>= V.freeze

main :: IO ()
main = pure ()
