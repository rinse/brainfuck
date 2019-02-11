module Brainfuck
    ( Brainfuck
    , inc
    , dec
    , fwd
    , bwd
    , putB
    , getB
    , while
    , runBrainfuck
    ) where

import Control.Monad.State
import Data.Char (chr, ord)
import Data.Zipper

type Memory = Zipper Int
initialMemory :: Memory
initialMemory = toZipper 0 []
cur :: Memory -> Int
cur = extractZipper

newtype Brainfuck a = Bf { unBf :: StateT Memory IO a }

instance Functor Brainfuck where
  fmap f (Bf s) = Bf $ fmap f s

instance Applicative Brainfuck where
  pure = Bf . return
  (Bf f)  <*> (Bf s) = Bf $ f <*> s

instance Monad Brainfuck where
  return = pure
  (Bf s) >>= f = Bf $ s >>= unBf . f

inc, dec, fwd, bwd, putB, getB :: Brainfuck ()
inc = Bf $ get >>= put . inc' where
  inc' (Zipper l n r) = Zipper l (n+1) r
dec = Bf $ get >>= put . dec' where
  dec' (Zipper l n r) = Zipper l (n-1) r
fwd = Bf $ get >>= put . right
bwd = Bf $ get >>= put . left
putB = Bf $ void $ get >>= lift . putB' where
  putB' z = z <$ putChar (chr $ cur z)
getB = Bf $ do
  (Zipper l _ r) <- get
  c <- lift getChar
  put $ Zipper l (ord c) r

while :: Brainfuck () -> Brainfuck ()
while = Bf . while' . unBf where
  while' :: StateT Memory IO () -> StateT Memory IO ()
  while' bf = do
    m <- get
    when (cur m /= 0) $ do
      bf
      while' bf

runBrainfuck :: Brainfuck a -> IO (a, Memory)
runBrainfuck = flip runStateT initialMemory . unBf

