{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NPlusKPatterns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UnicodeSyntax             #-}
{-# OPTIONS_HADDOCK show-extensions    #-}


module Solucoes where

import           Control.Applicative hiding ((<|>))
import           Cp
import           Data.List           hiding (find)
import           Given               hiding (OutExpAr, ad_gen, avgLTree,
                                      avg_aux, calcLine, clean, deCasteljau,
                                      g_eval_exp, gopt, hyloAlgForm, inic, loop,
                                      outExpAr, prj, recExpAr, sd_gen)
import           LTree
import           List                hiding (fac)
import           Nat


-- ------------------------------- Problema 1 ------------------------------- --
outExpAr ∷ ExpAr a → OutExpAr a
outExpAr = to ∷ ExpAr a → OutExpAr a

recExpAr ∷ (a → e) → b ∐ c ∐ d × a × a ∐ g × a → b ∐ c ∐ d × e × e ∐ g × e
recExpAr f = baseExpAr id id id f f id f

g_eval_exp ∷ Floating c ⇒ c → b ∐ c ∐ BinOp × c × c ∐ UnOp × c → c
g_eval_exp a = const a ∐ id ∐ Cp.ap . (outBinOp × id) ∐ Cp.ap . (outUnOp × id)

-- | Nós optimizamos 4 casos e para os outros usamos 'outExpAr'
clean ∷ (Eq a, Num a) ⇒ ExpAr a → OutExpAr a
clean q = case q of
  (Un E      (N 0)        ) → tag 1
  (Un Negate (N 0)        ) → tag 0
  (Bin Product (N 0) _    ) → tag 0
  (Bin Product _     (N 0)) → tag 0
  a                         → outExpAr a
  where tag = i2 . i1

gopt ∷ Floating a ⇒ a → () ∐ a ∐ BinOp × a × a ∐ UnOp × a → a
gopt = undefined

sd_gen ∷ Floating a ⇒ () ∐ a ∐ Bin (Dup (ExpAr a)) ∐ Un (Dup (ExpAr a)) → Dup (ExpAr a)
sd_gen = f ∐ g ∐ h ∐ k where
  f    = const (X, N 1)
  g a  = (N a, N 0)
  h    = bin_aux (Bin Sum) (Bin Product)
  k    = un_aux (Un Negate) (Bin Product) (Un E)

ad_gen ∷ Floating a ⇒ a → () ∐ a ∐ (BinOp, Dup (Dup a)) ∐ (UnOp, Dup a) → Dup a
ad_gen x = f ∐ g ∐ h ∐ k where
  f = const (x, 1)
  g a = (a, 0)
  h = bin_aux (+) (*)
  k = un_aux negate (*) expd
-- ------------------------------- Problema 2 ------------------------------- --
loop ∷ Integral c ⇒ (c, c, c) → (c, c, c)
loop = g where g (a, b, c) = (div (a * b) c, b + 4, c + 1)

inic ∷ (Num a, Num b, Num c) ⇒ (a, b, c)
inic = (1, 2, 2)

prj ∷ (a, b, c) → a
prj = p where p (a, _, _) = a

cat ∷ (Integral c1, Integral c2) ⇒ c1 → c2
cat = prj . for loop inic
-- ------------------------------- Problema 3 ------------------------------- --
calcLine ∷ NPoint → (NPoint → OverTime NPoint)
calcLine = cataList h where
    h = either f g
    f _ _ = nil
    g _ []         =  nil
    g (d,f) (x:xs) =  concat . sequenceA [singl . linear1d d x, f xs]


deCasteljau :: [NPoint] -> OverTime NPoint
deCasteljau = hyloAlgForm alg  coalg where
   coalg = (id ⊕ id ⊕ split init tail) . outSL
   alg = const nil ∐ a
   a = const ∐ b
   b (e,d) pt = calcLine (e pt) (d pt) pt

outSL :: [a] -> () ∐ a ∐ [a]
outSL []  = i1 ()
outSL [a] = i2 (i1 a)
outSL l   = i2 (i2 l)

hyloAlgForm :: (() ∐ b ∐ c × c -> c) -> (a -> d ∐ b ∐ a × a) -> a -> c
hyloAlgForm = h where
    h a b = cataCastel a . anaCastel b

newtype Castel' a = Castel' (() ∐ a ∐ Castel a × Castel a)
data Castel a = Empty | Single a | InitTail (Castel a × Castel a) deriving Show

inCastel :: b ∐ a ∐ Castel a × Castel a -> Castel a
inCastel = const Empty ∐ Single ∐ InitTail

outCastel :: Castel a -> () ∐ a ∐ Castel a × Castel a
outCastel Empty            = i1 ()
outCastel (Single a)       = i2 (i1 a)
outCastel (InitTail (e,d)) = i2 (i2 (e,d))

fC :: (a -> d) -> b1 ∐ b2 ∐ a × a -> b1 ∐ b2 ∐ d × d
fC f = id ⊕ id ⊕ f × f

cataCastel :: (() ∐ b ∐ d × d -> d) -> Castel b -> d
cataCastel f = f . fC (cataCastel f) . outCastel
anaCastel :: (a1 -> b ∐ a2 ∐ a1 × a1) -> a1 -> Castel a2
anaCastel g = inCastel . fC (anaCastel g) . g
-- ------------------------------- Problema 4 ------------------------------- --
outL :: [a] -> a ∐ a × [a]
outL [a]   = i1 a
outL (a:x) = i2 (a,x)

recL :: (c -> d) -> (b1 ∐ b2 × c) -> b1 ∐ b2 × d
recL  f   = id ⊕ id × f

cataL :: (b ∐ b × d -> d) -> [b] -> d
cataL g   = g . recL (cataL g) . outL

avg_aux ∷ Fractional b ⇒ [b] → (b, b)
avg_aux = cataL (either b q) where
   b a = (a,1)
   q (h,(a,l)) = ((h + (a*l)) / (l+1) ,l+1)

avgLTree ∷ Fractional b ⇒ LTree b → b
avgLTree = p1 . cataLTree gene where
   gene = either g q where
      g a = (a,1)
      q((a1,l1),(a2,l2)) = (((a1*l1)+(a2*l2))/(l1+l2),l1+l2)


-- -------------------------------- Símbolos -------------------------------- --
infixr 6 ×
type a × b = (a, b)
-- | bimap de tuplos ('(,)')
(×) ∷ (a → b) → (c → d) → (a, c) → (b, d)
(×) = (><)

infixr 4 ⊕
-- | bimap de 'Either'
(⊕) ∷ (a → b) → (c → d) → a ∐ c → b ∐ d
(⊕) = (-|-)

infixr 4 ∐
type (∐) = Either
(∐) ∷ (a → c) → (b → c) → a ∐ b → c
(∐) = either

-- | ≡ @'BinOp' '×' ('ExpAr' d '×' 'ExpAr' d)@
type BinExp d = BinOp × ExpAr d × ExpAr d

type UnExp d = UnOp × ExpAr d

{- | Isso é uma redefinição do que o Professor definiu.
É igual excepto os símbolos mais fáceis de ler. -}
type OutExpAr a = () ∐ a ∐ BinExp a ∐ UnExp a

type ℚ = Rational
toℚ ∷ Real a ⇒ a → ℚ
toℚ = toRational
fromℚ ∷ Fractional a ⇒ ℚ → a
fromℚ = fromRational

-- -------------------------- Auxiliares Problema 1 ------------------------- --

{- | Para criar uma interpretação de um tipo A como um tipo B. Assim, por exemplo,
posso definir que a expressão 'X' do tipo @'ExpAr' a@ pode ser interpretada como
@'Left' '()'@ do tipo @'OutExpAr' a@. -}
class Interpretation a b where
    to ∷ a → b

{- | Vamos criar uma interpretação de @'ExpAr' a@ como @'OutExpAr' a@.
Ou seja, essa interpretação é 'outExpAr'. -}
instance Interpretation (ExpAr a) (OutExpAr a) where
  to X            = Left ()
  to (N a       ) = Right $ Left a
  to (Bin op l r) = Right $ Right $ Left (op, (l, r))
  to (Un op a   ) = Right $ Right $ Right (op, a)

{-| Interpretamos cada símbolo 'BinOp' como uma função @(c,c) → c@
Por exemplo, o símbolo 'Sum' é interpretado como a função 'add' -}
instance (Num c) ⇒ Interpretation BinOp ((c, c) → c) where
  to Sum     = add
  to Product = mul

-- | Na nossa linguagem mais usual
outBinOp ∷ Num c => BinOp → (c × c) → c
outBinOp = to ∷ (Num c) ⇒ BinOp → (c × c → c)

-- | Interpretamos cada símbolo 'UnOp' como uma função @c→c@ onde @c@ é da classe 'Floating'.
instance (Floating c) ⇒ Interpretation UnOp (c → c) where
  to Negate = negate
  to E      = Prelude.exp

-- | Na nossa linguagem mais usual
outUnOp ∷ Floating c => UnOp → (c → c)
outUnOp = to ∷ (Floating c) ⇒ UnOp → (c → c)

type Dup d = d × d -- Baseado na função 'dup' definida em [[Cp.hs]]
type Bin d = BinOp × Dup d
type Un d = UnOp × d

-- | Só extrai o código que se repetia nas funções 'sd_gen' e 'ad_gen' do Tiago
bin_aux ∷ (t → t → t) → (t → t → t) → (BinOp, Dup (Dup t)) → Dup t
bin_aux f g (op, ((e1, d1), (e2, d2))) = case op of
  Sum     → (e1 `f` e2, d1 `f` d2)
  Product → (e1 `g` e2, f (g e1 d2) (g d1 e2))

-- | Extração do código que se repetia. Veja 'bin_aux'.
un_aux ∷ (a → b) → (b → a → b) → (a → b) → (UnOp, Dup a) → Dup b
un_aux f g h (op, (e, d)) = case op of
  Negate → (f e, f d)
  E      → (h e, g (h e) d)

-- --------------------------------- Extras --------------------------------- --
out ∷ [a] × [b] → Either () ((a × b) × ([a] × [b]))
out = \case
  ([],_)           → Left ()
  (_,[])           → Left ()
  (a : as, b : bs) → Right ((a,b) , (as,bs))

calcLineAlt1 ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLineAlt1 as bs e = hyloList (either nil g1) out (as,bs) where
    g1 = cons . (flip (uncurry linear1d) e × id)

calcLineAlt2 ∷ NPoint → (NPoint → OverTime NPoint)
calcLineAlt2 xs ys e = cataList (either nil h) (fmap (uncurry linear1d) $ uncurry zip (xs, ys)) where
    h (f, fs) = f e : fs
