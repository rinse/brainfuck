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

runBrainfuck :: Brainfuck -> IO ((), Memory)
runBrainfuck = flip runStateT initialMemory

