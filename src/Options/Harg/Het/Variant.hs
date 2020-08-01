{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Options.Harg.Het.Variant
  ( VariantF (..),
    fromVariantF,
    InjectPosF (..),
    pattern In1,
    pattern In2,
    pattern In3,
    pattern In4,
    pattern In5,
  )
where

import qualified Data.Barbie as B
import Data.Kind (Type)
import Options.Harg.Het.Nat

-- | A Variant is similar to nested 'Either's. For example, @Variant '[Int,
-- Bool, Char]@ is isomorphic to @Either Int (Either Bool Char)@. 'VariantF'
-- is a variant for higher-kinded types, which means that the type-level list
-- holds types of kind @(Type -> Type) -> Type@, and the second parameter is
-- the type constructor @f :: Type -> Type@. To pattern match on a variant,
-- @HereF@ and @ThereF@ can be used:
--
-- @
--   getFromVariant :: Variant '[Int, Bool, String] -> Bool
--   getFromVariant (ThereF (HereF b)) = b
-- @
data VariantF (xs :: [(Type -> Type) -> Type]) (f :: Type -> Type) where
  HereF :: x f -> VariantF (x ': xs) f
  ThereF :: VariantF xs f -> VariantF (y ': xs) f

instance
  ( B.FunctorB x,
    B.FunctorB (VariantF xs)
  ) =>
  B.FunctorB (VariantF (x ': xs))
  where
  bmap nat (HereF x) = HereF $ B.bmap nat x
  bmap nat (ThereF xs) = ThereF $ B.bmap nat xs

instance B.FunctorB (VariantF '[]) where
  bmap _ _ = error "Impossible: empty variant"

instance
  ( B.TraversableB x,
    B.TraversableB (VariantF xs)
  ) =>
  B.TraversableB (VariantF (x ': xs))
  where
  btraverse nat (HereF x) = HereF <$> B.btraverse nat x
  btraverse nat (ThereF xs) = ThereF <$> B.btraverse nat xs

instance B.TraversableB (VariantF '[]) where
  btraverse _ _ = error "Impossible: empty variant"

-- * Helpers for pattern-matching on variants

pattern In1 :: x1 f -> VariantF (x1 ': xs) f
pattern In1 x = HereF x

pattern In2 :: x2 f -> VariantF (x1 ': x2 ': xs) f
pattern In2 x = ThereF (In1 x)

pattern In3 :: x3 f -> VariantF (x1 ': x2 ': x3 ': xs) f
pattern In3 x = ThereF (In2 x)

pattern In4 :: x4 f -> VariantF (x1 ': x2 ': x3 ': x4 ': xs) f
pattern In4 x = ThereF (In3 x)

pattern In5 :: x5 f -> VariantF (x1 ': x2 ': x3 ': x4 ': x5 ': xs) f
pattern In5 x = ThereF (In4 x)

-- https://github.com/i-am-tom/learn-me-a-haskell/blob/master/src/OneOf/Fold.hs

-- | Create the signature needed for 'FromVariantF' to work. This constructs a
-- function that takes as arguments functions that can act upon each item in
-- the list that the 'VariantF' holds. For example, @VariantF [a, b, c]
-- f@ will result to the signature:
--
-- @
--   VariantF [a, b, c] f -> (a f -> r) -> (b f -> r) -> (c f -> r) -> r
-- @
type family FoldSignatureF (xs :: [(Type -> Type) -> Type]) r f where
  FoldSignatureF (x ': xs) r f = (x f -> r) -> FoldSignatureF xs r f
  FoldSignatureF '[] r f = r

class FromVariantF xs result f where
  fromVariantF :: VariantF xs f -> FoldSignatureF xs result f

instance FromVariantF '[x] result f where
  fromVariantF (HereF x) f = f x
  fromVariantF (ThereF _) _ = error "Impossible: empty variant"

instance
  ( tail ~ (x' ': xs),
    FromVariantF tail result f,
    IgnoreF tail result f
  ) =>
  FromVariantF (x ': x' ': xs) result f
  where
  fromVariantF (ThereF x) _ = fromVariantF @_ @result x
  fromVariantF (HereF x) f = ignoreF @tail (f x)

class IgnoreF (args :: [(Type -> Type) -> Type]) result f where
  ignoreF :: result -> FoldSignatureF args result f

instance IgnoreF '[] result f where
  ignoreF result = result

instance IgnoreF xs result f => IgnoreF (x ': xs) result f where
  ignoreF result _ = ignoreF @xs @_ @f result

-- | Given a type-level natural that designates a position of injection into
-- a 'VariantF', return a function that performs this injection. For example,
-- @S Z@ which corresponds to 1 or the second position in the type-level list
-- the variant holds, can give the injection @b f -> VariantF [a, b, c] f@.
-- The injection can as well be constructed without providing the position, but
-- it helps in case @x@ is not unique in @xs@.
class
  InjectPosF
    (n :: Nat)
    (x :: (Type -> Type) -> Type)
    (xs :: [(Type -> Type) -> Type])
    | n xs -> x
  where
  injectPosF :: SNat n -> (x f -> VariantF xs f)

instance InjectPosF Z x (x ': xs) where
  injectPosF SZ = HereF

instance InjectPosF n x xs => InjectPosF (S n) x (y ': xs) where
  injectPosF (SS n) = ThereF . injectPosF n
