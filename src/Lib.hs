module Lib
    ( someFunc
    ) where

import Control.Monad
import Brainfuck

helloWorld :: Brainfuck ()
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

helloWorld' :: String
helloWorld' = "+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+."

someFunc :: IO ()
someFunc = do
  void $ runBrainfuck helloWorld
  void $ runBrainfuck . readBrainfuck $ helloWorld'

