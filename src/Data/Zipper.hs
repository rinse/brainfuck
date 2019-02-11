{-# LANGUAGE DeriveFunctor #-}
module Data.Zipper
    (Zipper (..)
    , left
    , right
    , toZipper
    , extractZipper
    ) where

data Zipper a = Zipper [a] a [a] deriving
    ( Show
    , Functor
    )

left, right :: Zipper a -> Zipper a
left (Zipper ls c (r:rs)) = Zipper (c:ls) r rs
right (Zipper (l:ls) c rs) = Zipper ls l (c:rs)

toZipper :: a -> [a] -> Zipper a
toZipper x xs = Zipper (repeat x) x (xs ++ repeat x)

extractZipper :: Zipper a -> a
extractZipper (Zipper _ c _) = c

