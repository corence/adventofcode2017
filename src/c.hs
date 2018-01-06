
module C where

import Data.IORef
import Control.Applicative
import Control.Monad
import Data.Function

fac :: Int -> IO Int
fac n = do {
  a <- auto 1;
  i <- auto n;
  while (i >. (0 :: Int)) $ do {
      a *= i;
      i -= (1 :: Int);
  };
  rvalue a;
}

data V = V (IORef Int)
data E = E Int

class R a => L a where
  lvalue :: a -> IORef Int

instance L V where
  lvalue (V ioref) = ioref

class R a where
  rvalue :: a -> IO Int

instance R V where
  rvalue (V ioref) = readIORef ioref

instance R E where
  rvalue (E a) = pure a

instance R Int where
  rvalue = pure

(*.) :: R r => r -> r -> IO E
(*.) lhs rhs = E <$> liftA2 (*) (rvalue lhs) (rvalue rhs)

(-.) :: R r => r -> r -> IO E
(-.) lhs rhs = E <$> liftA2 (-) (rvalue lhs) (rvalue rhs)

(*=) :: (L l, R r) => l -> r -> IO ()
(*=) variable expression = do
  value <- rvalue expression
  modifyIORef (lvalue variable) (* value)

(=.) :: (L l, R r) => l -> r -> IO ()
(=.) variable expression = do
  value <- rvalue expression
  writeIORef (lvalue variable) value

(=..) :: (L l, R r) => (Int -> Int -> Int) -> l -> r -> IO ()
(=..) func variable expression = do
  value <- rvalue expression
  modifyIORef (lvalue variable) (`func` value)

(-=) :: (L l, R r) => l -> r -> IO ()
(-=) = (=..) (-)

(+=) :: (L l, R r) => l -> r -> IO ()
(+=) = (=..) (+)

(>.) :: (R lhs, R rhs) => lhs -> rhs -> IO E
(>.) lhs rhs = do
  l <- rvalue lhs
  r <- rvalue rhs
  if l > r
    then pure $ E 1
    else pure $ E 0

auto :: Int -> IO V
auto value = V <$> newIORef value

while :: IO E -> IO () -> IO ()
while = undefined
