module Options.Harg.Het.Nat where

data Nat
  = Z
  | S Nat

data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
