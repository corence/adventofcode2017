
module Day10Rope where

import Lib
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.List as List
import Data.Char
import Data.Bits(xor)
import Text.Printf

import Control.Monad.ST

applyLengths :: [Int] -> V.Vector Int -> V.Vector Int
applyLengths lengths = V.modify (applyLengthsM lengths 0 0)

applyLengthsM :: [Int] -> Int -> Int -> M.MVector s Int -> ST s ()
applyLengthsM [] _ _ _ = pure ()
applyLengthsM (len:lengths) index skip rope = do
  rotate index len rope
  applyLengthsM lengths (index + len + skip) (skip + 1) rope

rotate :: Int -> Int -> M.MVector s Int -> ST s ()
rotate index amount rope = mapM_ (uncurry (M.swap rope)) indexPairs
  where indexPairs = zip (reverse indices) indices & filter (uncurry (<))
        indices = wrappedIndices (M.length rope) index amount

wrappedIndices :: Int -> Int -> Int -> [Int]
wrappedIndices wrapLength firstIndex amount = [firstIndex..lastIndex] & map (`mod` wrapLength)
  where lastIndex = firstIndex + amount - 1

  -- get the last part of the rope
  -- get the first part of the rope
  -- make lots of modify calls

makeVector :: Int -> V.Vector Int
makeVector n = [0..(n-1)] & V.fromList

knotHash :: String -> String
knotHash input = denseHash & map toHex & concat
  where saltedLengths = map ord input ++ [17, 31, 73, 47, 23]
        extendedLengths = cycle saltedLengths & take (64 * length saltedLengths)
        denseHash = split 16 (V.toList sparseHash) & map (foldr1 xor)
        sparseHash = makeVector 256 & applyLengths extendedLengths

toHex :: Int -> String
toHex = printf "%02x"

split :: Int -> [a] -> [[a]]
split _ [] = []
split num list = take num list : split num (drop num list)
