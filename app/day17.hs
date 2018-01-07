
import Lib
import qualified Data.List as List

generateBuffers advancement
  = iterate
          (\(index, list) -> insertCircular advancement (length list) (index, list))
          (0, [0])

generateMagic :: Int -> [(Int, MagicBuffer)]
generateMagic advancement
  = iterate
          (\(index, magicBuffer) -> insertMagicCircle advancement (magicalSize magicBuffer) (index, magicBuffer))
          (1, MagicBuffer 1 1 2)

main :: IO ()
main = do
  generateBuffers 3 & take 20 & map show & mapM_ putStrLn
  generateBuffers 3 & (!! 2017) & snd & dropWhile (/= 2017) & (!! 1) & print
  generateBuffers 359 & (!! 2017) & snd & dropWhile (/= 2017) & (!! 1) & print
  generateMagic 3 & take 20 & map snd & mapM_ print
  generateMagic 3 & (!! 5) & snd & print
  generateMagic 359 & take 50000000 & map snd & map magicalValue & rle & mapM_ print
  generateMagic 359 & (!! 49999999) & snd & print

rle :: Eq a => [a] -> [(a, Int)]
rle list = list & List.group & map (\sublist -> (head sublist, length sublist))

insertCircular :: Int -> a -> (Int, [a]) -> (Int, [a])
insertCircular advancement value (index, buffer)
  = (newIndex, newBuffer)
  where newIndex = 1 + ((index + advancement) `mod` length buffer)
        newBuffer = insert value newIndex buffer

insert :: a -> Int -> [a] -> [a]
insert value index list = take index list ++ [value] ++ drop index list

data MagicBuffer = MagicBuffer
                     {
                       magicalIndex :: Int,
                       magicalValue :: Int,
                       magicalSize :: Int
                     } deriving Show

insertMagic :: Int -> Int -> MagicBuffer -> MagicBuffer
insertMagic index value (MagicBuffer mindex mvalue msize)
  = case compare index mindex of
          LT -> MagicBuffer (mindex + 1) mvalue (msize + 1)
          EQ -> MagicBuffer mindex value (msize + 1)
          GT -> MagicBuffer mindex mvalue (msize + 1)

insertMagicCircle :: Int -> Int -> (Int, MagicBuffer) -> (Int, MagicBuffer)
insertMagicCircle advancement value (index, magic)
  = (newIndex, newMagicBuffer)
  where newIndex = 1 + ((index + advancement) `mod` magicalSize magic)
        newMagicBuffer = insertMagic newIndex value magic
