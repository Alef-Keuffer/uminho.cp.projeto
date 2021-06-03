module Main where

main :: IO ()

main = undefined

{-- | Note como o mapa em bifunctores funciona em Haskell

>>> fmap (+1) (Right 1)
Ambiguous type variable ‘a0’ arising from a use of ‘evalPrint’
prevents the constraint ‘(Show a0)’ from being solved.
Probable fix: use a type annotation to specify what ‘a0’ should be.
These potential instances exist:
  instance [safe] (Show a, Show b) => Show (a :-> b)
    -- Defined in ‘Test.QuickCheck.Function’
  instance [safe] (Show a, Show b) => Show (Fun a b)
    -- Defined in ‘Test.QuickCheck.Function’
  instance Show ASCIIString -- Defined in ‘Test.QuickCheck.Modifiers’
  ...plus 249 others
  (use -fprint-potential-instances to see them all)

>>> fmap (+1) (Left 1)
Left 1

Isso não dá erro porque existem instâncias de Either para Functor
(em haskell, functores com 1 argumento) mas sabemos que Either é um bifunctor.
Logo, esse "mapa" é um pouco degenerado, ele aplica só para um lado do functor
(a escolha do lado é arbitrária e feita na criação da instância), em Haskell escolheram o direito

Em bifunctores estamos usando as funções menos genéricas + e ×

>>> ((+1) -|- (+1)) (Left 1)
Variable not in scope:
  (-|-) :: (a0 -> a0) -> (a1 -> a1) -> Either a2 b0 -> t

>>> ((+1) -|- (+1)) (Right 1)
Variable not in scope:
  (-|-) :: (a1 -> a1) -> (a2 -> a2) -> Either a0 b0 -> t

>>> ((+1) >< (+1)) (1,1)
Variable not in scope:
  (><) :: (a0 -> a0) -> (a1 -> a1) -> (a2, b0) -> t

-- Mas poderiamos usar um bimap (mapa para functor com dois argumentos)
-- Inclusive o professor fez isso em Cp:
class BiFunctor f where
      bmap :: (a -> b) -> (c -> d) -> (f a c -> f b d)

instance BiFunctor Either where
      bmap f g = f -|- g

instance BiFunctor (,) where
      bmap f g = f >< g

-- Logo,
>>> bmap (+1) (+1) (Right 1)
Variable not in scope:
  bmap :: (a1 -> a1) -> (a2 -> a2) -> Either a0 b0 -> t
>>>bmap (+1) (+1) (Left 1)
Variable not in scope:
  bmap :: (a0 -> a0) -> (a1 -> a1) -> Either a2 b0 -> t
>>>bmap (+1) (+1) (1,1)
Variable not in scope:
  bmap :: (a0 -> a0) -> (a1 -> a1) -> (a2, b0) -> t

Idealmente, gostaria de um map arbitrário, mas em Haskell
normalmente temos que criar novas instâncias para cada functor.
-}
