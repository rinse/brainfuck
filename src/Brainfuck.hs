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
    , readBrainfuck
    ) where

import Control.Monad.State
import Data.Char (chr, ord)
import Data.Zipper

type Memory = Zipper Int
initialMemory :: Memory
initialMemory = toZipper 0 []
cur :: Memory -> Int
cur = extractZipper
modifyCur :: (a -> a) -> Zipper a -> Zipper a
modifyCur f (Zipper l c r) = Zipper l (f c) r
putCur :: a -> Zipper a -> Zipper a
putCur a = modifyCur (const a)

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
inc = Bf $ modify' $ modifyCur (+1)
dec = Bf $ modify' $ modifyCur (subtract 1)
fwd = Bf $ modify' right
bwd = Bf $ modify' left
putB = Bf $ void $ get >>= lift . putB' where
  putB' z = z <$ putChar (chr $ cur z)
getB = Bf $ do
  c <- lift getChar
  modify $ putCur (ord c)

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

readBrainfuck :: String -> Brainfuck ()
readBrainfuck = sequence_ . fst . rbf where
  rbf :: String -> ([Brainfuck ()], String)
  rbf [] = ([], [])
  rbf ('+':s) = inc <++> rbf s
  rbf ('-':s) = dec <++> rbf s
  rbf ('>':s) = fwd <++> rbf s
  rbf ('<':s) = bwd <++> rbf s
  rbf ('.':s) = putB <++> rbf s
  rbf (',':s) = getB <++> rbf s
  rbf ('[':s) = let (bs, s') = rbf s
                 in while (sequence_ bs) <++> rbf s'
  rbf (']':s) = ([], s)
  rbf (_:s) = rbf s
  b <++> (bs, s) = (b:bs, s)

