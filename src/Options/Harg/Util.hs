{-# LANGUAGE RankNTypes #-}
module Options.Harg.Util where

import           Data.Functor.Product (Product (..))

import qualified Data.Barbie          as B

bpairwise
  :: forall a f g h. B.ProductB a
  => (forall x. f x -> g x -> h x)
  -> a f
  -> a g
  -> a h
bpairwise f xs ys
  = B.bmap (\(Pair x y) -> f x y) (B.bprod xs ys)
