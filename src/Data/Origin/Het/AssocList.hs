module Data.Origin.Het.AssocList where

import           Data.Kind                   (Type, Constraint)
import           Data.Functor.Identity       (Identity)
import           GHC.TypeLits                (ErrorMessage(..), TypeError, Symbol, KnownSymbol, symbolVal)

import qualified Data.Barbie                 as B

import           Data.Origin.Het.All
import           Data.Origin.Options.Types

data AssocListF
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type])
    (f :: Type -> Type) where
  ANil  :: AssocListF '[] '[] f
  ACons :: x f -> AssocListF ts xs f -> AssocListF (t ': ts) (x ': xs) f

type family l :** r = (res :: (Type -> Type) -> Type) where
  (tl :-> vl) :** (tr :-> vr) = AssocListF '[tl, tr] '[vl, vr]
  (tl :-> vl) :** AssocListF ts vs = AssocListF (tl ': ts) (vl ': vs)
  l :** r
    = TypeError
    (    'Text "Invalid type for tagged options. Construct like this:"
    :$$: 'Text "type MyConfig"
    :$$: 'Text "  =   \"one\" :-> ConfigForOne"
    :$$: 'Text "  :** \"two\" :-> ConfigForTwo"
    )

infixr 4 :**

pattern (:+) :: x f -> AssocListF ts xs f -> AssocListF (t ': ts) (x ': xs) f
pattern x :+ xs = ACons x xs

infixr 4 :+

data (t :: Symbol) :-> (v :: (Type -> Type) -> Type) :: (Type -> Type) -> Type

infixr 5 :->
