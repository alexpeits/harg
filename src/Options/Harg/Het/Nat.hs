module Options.Harg.Het.Nat where

-- | Type-level Peano natural number.
data Nat
  = Z
  | S Nat

-- | Singleton type for 'Nat'.
data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
