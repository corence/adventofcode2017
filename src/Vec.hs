{-
module Vec where
import qualified Data.Vector.Unboxed.Mutable as VUM

data Vec s a = Vec Int (VUM.MVector s a)

length :: Unbox a => Vec s a -> Int
length (Vec len _) = len

null :: Unbox a => Vec s a -> Bool
null vec = length vec == 0

add :: Unbox a => Vec s a -> a -> m (Vec (PrimState m) a)
add vec@(Vec len content) value = do
  if len >= VUM.length content
    else Vec (len + 1) (VUM
  modifySTRef
  Vec (len + 1) (VUM.write content len value)

slice :: Unbox a => Int -> Int -> Vec s a -> Vec s a
init :: Unbox a => Vec s a -> Vec s a
tail :: Unbox a => Vec s a -> Vec s a
take :: Unbox a => Int -> Vec s a -> Vec s a
drop :: Unbox a => Int -> Vec s a -> Vec s a
splitAt :: Unbox a => Int -> Vec s a -> (Vec s a, Vec s a)
unsafeSlice :: Unbox a => Int -> Int -> Vec s a -> Vec s a
unsafeInit :: Unbox a => Vec s a -> Vec s a
unsafeTail :: Unbox a => Vec s a -> Vec s a
unsafeTake :: Unbox a => Int -> Vec s a -> Vec s a
unsafeDrop :: Unbox a => Int -> Vec s a -> Vec s a
overlaps :: Unbox a => Vec s a -> Vec s a -> Bool
new :: (PrimMonad m, Unbox a) => Int -> m (Vec (PrimState m) a)
unsafeNew :: (PrimMonad m, Unbox a) => Int -> m (Vec (PrimState m) a)
replicate :: (PrimMonad m, Unbox a) => Int -> a -> m (Vec (PrimState m) a)
replicateM :: (PrimMonad m, Unbox a) => Int -> m a -> m (Vec (PrimState m) a)
clone :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> m (Vec (PrimState m) a)
grow :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Int -> m (Vec (PrimState m) a)
unsafeGrow :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Int -> m (Vec (PrimState m) a)
clear :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> m ()
zip :: (Unbox a, Unbox b) => Vec s a -> Vec s b -> Vec s (a, b)
zip3 :: (Unbox a, Unbox b, Unbox c) => Vec s a -> Vec s b -> Vec s c -> Vec s (a, b, c)
zip4 :: (Unbox a, Unbox b, Unbox c, Unbox d) => Vec s a -> Vec s b -> Vec s c -> Vec s d -> Vec s (a, b, c, d)
zip5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) => Vec s a -> Vec s b -> Vec s c -> Vec s d -> Vec s e -> Vec s (a, b, c, d, e)
zip6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) => Vec s a -> Vec s b -> Vec s c -> Vec s d -> Vec s e -> Vec s f -> Vec s (a, b, c, d, e, f)
unzip :: (Unbox a, Unbox b) => Vec s (a, b) -> (Vec s a, Vec s b)
unzip3 :: (Unbox a, Unbox b, Unbox c) => Vec s (a, b, c) -> (Vec s a, Vec s b, Vec s c)
unzip4 :: (Unbox a, Unbox b, Unbox c, Unbox d) => Vec s (a, b, c, d) -> (Vec s a, Vec s b, Vec s c, Vec s d)
unzip5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) => Vec s (a, b, c, d, e) -> (Vec s a, Vec s b, Vec s c, Vec s d, Vec s e)
unzip6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) => Vec s (a, b, c, d, e, f) -> (Vec s a, Vec s b, Vec s c, Vec s d, Vec s e, Vec s f)
read :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Int -> m a
write :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Int -> a -> m ()
modify :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> (a -> a) -> Int -> m ()
swap :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Int -> Int -> m ()
unsafeRead :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Int -> m a
unsafeWrite :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Int -> a -> m ()
unsafeModify :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> (a -> a) -> Int -> m ()
unsafeSwap :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Int -> Int -> m ()
nextPermutation :: (PrimMonad m, Ord e, Unbox e) => Vec (PrimState m) e -> m Bool
set :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> a -> m ()
copy :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Vec (PrimState m) a -> m ()
move :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Vec (PrimState m) a -> m ()
unsafeCopy :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Vec (PrimState m) a -> m ()
unsafeMove :: (PrimMonad m, Unbox a) => Vec (PrimState m) a -> Vec (PrimState m) a -> m ()
-}

