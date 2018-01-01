
module Day10Rope where

import Lib
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

import Control.Monad.ST

applyLengths :: [Int] -> V.Vector Int -> V.Vector Int
applyLengths _ = id

makeVector :: Int -> V.Vector Int
makeVector n = [0..n] & V.fromList
