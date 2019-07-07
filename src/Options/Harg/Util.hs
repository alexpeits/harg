{-# LANGUAGE RankNTypes #-}
module Options.Harg.Util where

import           Data.Functor.Compose (Compose (..))
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

compose
  :: forall f g a.
      ( Functor f
      , B.FunctorB a
      )
  => (forall x. x -> g x)
  -> a f
  -> a (Compose f g)
compose to
  = B.bmap (Compose . fmap to)
