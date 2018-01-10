module Randoms where

import System.Random
import Control.Monad.State

data Pos3D = Pos3D Int Int Int

-- with let and in syntax: (painful to read/write, because we need to manage the random seed value)
makeRandomPos1 :: RandomGen g => g -> (Pos3D, g)
makeRandomPos1 gen0
  = let (x, gen1) = randomR (0, 20) gen0
        (y, gen2) = randomR (0, 20) gen1
        (z, gen3) = randomR (0, 20) gen2
    in (Pos3D x y z, gen2)

-- with "where" syntax: (equivalent to makeRandomPos1, and just as ugly)
makeRandomPos2 :: RandomGen g => g -> (Pos3D, g)
makeRandomPos2 gen0 = (Pos3D x y z, gen2)
    where (x, gen1) = randomR (0, 20) gen0
          (y, gen2) = randomR (0, 20) gen1
          (z, gen3) = randomR (0, 20) gen2

-- Here we don't bother passing around the "g" parameters to keep track of the seed
-- Instead, we let them be considered as "IO" data, external to this program
-- It works, but sometimes we do need a pure function, so it's not always helpful
makeRandomPos3 :: IO Pos3D
makeRandomPos3
  = do
    x <- randomRIO (0, 20)
    y <- randomRIO (0, 20)
    z <- randomRIO (0, 20)
    pure $ Pos3D x y z

-- The same as makeRandomPos3, but written in applicative style
-- This is much nicer imo -- much less visual noise
makeRandomPos4 :: IO Pos3D
makeRandomPos4
  = Pos3D <$> randomRIO (0, 20) <*> randomRIO (0, 20) <*> randomRIO (0, 20)

-- Next, we apply the State monad instead of the IO monad. Note this function
-- has the same type signature as makeRandomPos1 -- it should compile down to
-- exactly the same code, i think.
-- The State monad manages the random seed state for us completely.
makeRandomPos5 :: RandomGen g => g -> (Pos3D, g)
makeRandomPos5
  = runState $ do
    x <- state $ randomR (0, 20)
    y <- state $ randomR (0, 20)
    z <- state $ randomR (0, 20)
    pure $ Pos3D x y z

-- Same as makeRandomPos5, but now we're using applicative style
-- I like this!
makeRandomPos6 :: RandomGen g => g -> (Pos3D, g)
makeRandomPos6
  =   runState $ Pos3D
                 <$> state (randomR (0, 20))
                 <*> state (randomR (0, 20))
                 <*> state (randomR (0, 20))

-- Same as above, but factoring out the repetition
makeRandomPos7 :: RandomGen g => g -> (Pos3D, g)
makeRandomPos7 = runState (Pos3D <$> number <*> number <*> number)
  where number = state (randomR (0, 20))
