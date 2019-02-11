module Lib
    ( someFunc
    ) where

import Control.Monad.State
import Data.Char (chr, ord)
import Data.Zipper

type Memory = Zipper Int
initialMemory :: Memory
initialMemory = toZipper 0 []
cur :: Memory -> Int
cur = extractZipper

type Brainfuck = StateT Memory IO ()

inc, dec, fwd, bwd, putB, getB :: Brainfuck
inc = get >>= put . inc' where
  inc' (Zipper l n r) = Zipper l (n+1) r
dec = get >>= put . dec' where
  dec' (Zipper l n r) = Zipper l (n-1) r
fwd = get >>= put . right
bwd = get >>= put . left
putB = void $ get >>= lift . putB' where
  putB' z = return z <* putChar (chr $ cur z)
getB = do
  (Zipper l _ r) <- get
  c <- lift getChar
  put $ Zipper l (ord c) r

while :: Brainfuck -> Brainfuck
while bf = do
  m <- get
  when (cur m /= 0) $ do
    bf
    while bf


helloWorld :: Brainfuck
helloWorld = do
  replicateM_ 9 inc
  while $ do
    fwd >> replicateM_ 8 inc
    fwd >> replicateM_ 11 inc
    fwd >> replicateM_ 5 inc
    replicateM_ 3 bwd
    dec
  fwd >> putB
  fwd >> replicateM_ 2 inc >> putB
  replicateM_ 7 inc >> replicateM_ 2 putB
  replicateM_ 3 inc >> putB
  fwd >> dec >> putB
  replicateM_ 12 dec >> putB
  bwd >> replicateM_ 8 inc >> putB
  replicateM_ 8 dec >> putB
  replicateM_ 3 inc >> putB
  replicateM_ 6 dec >> putB
  replicateM_ 8 dec >> putB
  fwd >> inc >> putB

someFunc :: IO ()
someFunc = do
  void $ flip runStateT initialMemory helloWorld

