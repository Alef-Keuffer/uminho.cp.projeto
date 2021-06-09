{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NPlusKPatterns             #-}

-- =============================================================================
-- ADDED BY ALEF
{-# LANGUAGE MultiParamTypeClasses      #-} -- Para criar a classe 'Interpretation'
{-# LANGUAGE NoMonomorphismRestriction  #-} -- Temporário. Assume a assinatura mais geral.
{-# LANGUAGE RankNTypes                 #-} -- Para quantificar na tipagem. Usado em 'to' de 'Interpretation'
{-# LANGUAGE GADTs                      #-} -- Usado em 'to' de 'Interpretation' ('TypeFamilies' também funciona, investigar isso)
{-# LANGUAGE TypeOperators              #-} -- Para '∐' e '×' nos tipos
{-# LANGUAGE UnicodeSyntax              #-} -- Para que 'forall' = '∀', → = →, ∷ = ∷, etc.
{-# OPTIONS_HADDOCK show-extensions     #-} -- Estou gerado documentação Haddock, é necessário só para isso.
-- =============================================================================

{-# LANGUAGE DeriveFunctor #-}
module Solucoes where

import           Control.Applicative     hiding ( (<|>) )
import           Control.Monad
import           Cp
import           Data.List               hiding ( find )
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           LTree
import           List                    hiding ( fac )
import           Nat
import           System.Process
import           Test.QuickCheck         hiding ( (><)
                                                , choose
                                                , collect
                                                )
import qualified Test.QuickCheck               as QuickCheck

-- * Problemas

-- ** Problema 1

-- =============================================================================
-- *** Código Fornecido
data ExpAr a
  = X
  | N a
  | Bin BinOp (ExpAr a) (ExpAr a)
  | Un UnOp (ExpAr a)
  deriving (Eq, Show)

data BinOp
  = Sum
  | Product
  deriving (Eq, Show)

data UnOp
  = Negate
  | E
  deriving (Eq, Show)
-- -----------------------------------------------------------------------------
inExpAr ∷ b ∐ a ∐ BinExp a ∐ UnExp a → ExpAr a
-- | @'inExpAr'@ ≡ @'const' 'X' '∐' 'N' '∐' bin '∐' ('Un' '＾') where bin (op, (a, b)) = 'Bin' op a b@
inExpAr = either (const X) num_ops
 where
  num_ops = either N ops
  ops     = either bin (uncurry Un)
  bin (op, (a, b)) = Bin op a b

-- | @'baseExpAr'@ ≡ @f g h j k l z = f '⊕' g '⊕' h '×' j '×' k '⊕' l '×' z@
baseExpAr ∷
  (a → b) →
  (c → d) →
  (e → f) →
  (g → h) →
  (i → j) →
  (k → l) →
  (m → n) →
  a ∐ c ∐ e × g × i ∐ k × m →
  b ∐ d ∐ f × h × j ∐ l × n
baseExpAr f g h j k l z = f -|- (g -|- (h >< (j >< k) -|- l >< z))

cataExpAr ∷ (() ∐ c ∐ BinOp × e × e ∐ UnOp × e → e) → ExpAr c → e
cataExpAr g = g . recExpAr (cataExpAr g) . outExpAr

anaExpAr ∷ (a → b ∐ c ∐ BinOp × a × a ∐ UnOp × a) → a → ExpAr c
anaExpAr g = inExpAr . recExpAr (anaExpAr g) . g

hyloExpAr
  ∷ (() ∐ c ∐ BinOp × d × d ∐ UnOp × d → d)
  → (a → b ∐ c ∐ BinOp × a × a ∐ UnOp × a)
  → a
  → d
hyloExpAr h g = cataExpAr h . anaExpAr g
-- -----------------------------------------------------------------------------
expd ∷ Floating a ⇒ a → a
expd = Prelude.exp
-- -----------------------------------------------------------------------------
eval_exp ∷ Floating a ⇒ a → (ExpAr a) → a
eval_exp a = cataExpAr (g_eval_exp a)

optmize_eval ∷ (Floating a, Eq a) ⇒ a → (ExpAr a) → a
optmize_eval a = hyloExpAr (gopt a) clean

sd ∷ Floating a ⇒ ExpAr a → ExpAr a
sd = p2 . cataExpAr sd_gen

ad ∷ Floating a ⇒ a → ExpAr a → a
ad v = p2 . cataExpAr (ad_gen v)
-- =============================================================================
-- *** Solução

infixr 6 ×
type a × b = (a, b)
-- | bimap de tuplos ('(,)')
(×) ∷ (a → b) → (c → d) → (a, c) → (b, d)
(×) = (><)
-- -----------------------------------------------------------------------------
infixr 4 ⊕
-- | bimap de 'Either'
(⊕) ∷ (a → b) → (c → d) → a ∐ c → b ∐ d
(⊕) = (-|-)
-- -----------------------------------------------------------------------------
infixr 4 ∐
type (∐) = Either
(∐) ∷ (a → c) → (b → c) → a ∐ b → c
(∐) = either
-- -----------------------------------------------------------------------------
-- | ≡ @'BinOp' '×' ('ExpAr' d '×' 'ExpAr' d)@
type BinExp d = BinOp × ExpAr d × ExpAr d

type UnExp d = UnOp × ExpAr d

{- | Isso é uma redefinição do que o Professor definiu.
É igual excepto os símbolos mais fáceis de ler. -}
type OutExpAr a = () ∐ a ∐ BinExp a ∐ UnExp a
-- -----------------------------------------------------------------------------
{- | Para criar uma interpretação de um tipo A como um tipo B. Assim, por exemplo,
posso definir que a expressão 'X' do tipo @'ExpAr' a@ pode ser interpretada como
@'Left' '()'@ do tipo @'OutExpAr' a@. -}
class Interpretation a b where
    to ∷ a → b
    -- O '~' em @to ∷ ∀ b1 a1. (b1 ~ b, a1 ~ a) ⇒ a → b@ é igualdade de tipos
    -- Essa definição era necessária antes porque estava usando a extensão TypeFamilies (agora uso GADTs)
    -- e, como dito em [Equality Constraints](https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/equality-constraints.html)
    -- "In the presence of type families, whether two types are equal cannot generally be decided locally.
    -- Hence, the contexts of function signatures may include equality constraints"
    -- Vou remover esse comentário, porque quero confirmar que minha modificação não quebrou nada.
-- -----------------------------------------------------------------------------
{- | Vamos criar uma interpretação de @'ExpAr' a@ como @'OutExpAr' a@.
Ou seja, essa interpretação é 'outExpAr'. -}
instance Interpretation (ExpAr a) (OutExpAr a) where
  to X            = Left ()
  to (N a       ) = Right $ Left a
  to (Bin op l r) = Right $ Right $ Left (op, (l, r))
  to (Un op a   ) = Right $ Right $ Right (op, a)

{- | Não há muito o que dizer. Essa função é um sinônimo. -}
outExpAr ∷ ExpAr a → OutExpAr a
outExpAr = to ∷ ExpAr a → OutExpAr a
-- -----------------------------------------------------------------------------
recExpAr ∷ (a → e) → b ∐ c ∐ d × a × a ∐ g × a → b ∐ c ∐ d × e × e ∐ g × e
recExpAr f = baseExpAr id id id f f id f
-- -----------------------------------------------------------------------------

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

g_eval_exp ∷ Floating c ⇒ c → b ∐ c ∐ BinOp × c × c ∐ UnOp × c → c
g_eval_exp a = const a ∐ id ∐ Cp.ap . (outBinOp × id) ∐ Cp.ap . (outUnOp × id)
-- -----------------------------------------------------------------------------
-- | Nós optimizamos 4 casos e para os outros usamos 'outExpAr'
clean ∷ (Eq a, Num a) ⇒ ExpAr a → OutExpAr a
clean q = case q of
  (Un E      (N 0)        ) → tag 1
  (Un Negate (N 0)        ) → tag 0
  (Bin Product (N 0) _    ) → tag 0
  (Bin Product _     (N 0)) → tag 0
  a                         → outExpAr a
  where tag = i2 . i1
-- -----------------------------------------------------------------------------
gopt ∷ Floating a ⇒ a → () ∐ a ∐ BinOp × a × a ∐ UnOp × a → a
gopt = undefined
-- -----------------------------------------------------------------------------
type Dup d = d × d -- Baseado na função 'dup' definida em [[Cp.hs]]
type Bin d = BinOp × Dup d
type Un d = UnOp × d

-- | Só extrai o código que se repetia nas funções 'sd_gen' e 'ad_gen' do Tiago
bin_aux ∷ (t → t → t) → (t → t → t) → (BinOp, Dup (Dup t)) → Dup t
bin_aux f g (op, ((e1, d1), (e2, d2))) = case op of
  Sum     → (e1 `f` e2, d1 `f` d2)
  Product → (e1 `g` e2, f (g e1 d2) (g d1 e2))

-- | Extração do código que se repetia. Veja 'bin_aux'.
un_aux
  ∷ (t1 → t2) → (t2 → t1 → t2) → (t1 → t2) → (UnOp, Dup t1) → Dup t2
un_aux f g h (op, (e, d)) = case op of
  Negate → (f e, f d)
  E      → (h e, g (h e) d)

sd_gen
  ∷ Floating a
  ⇒ () ∐ a ∐ Bin (Dup (ExpAr a)) ∐ Un (Dup (ExpAr a))
  → Dup (ExpAr a)
sd_gen = f ∐ g ∐ h ∐ k where
  f = const (X, N 1)
  g a = (N a, N 0)
  h = bin_aux (Bin Sum) (Bin Product)
  k = un_aux (Un Negate) (Bin Product) (Un E)
-- -----------------------------------------------------------------------------
ad_gen
  ∷ Floating a ⇒ a → () ∐ a ∐ (BinOp, Dup (Dup a)) ∐ (UnOp, Dup a) → Dup a
ad_gen x = f ∐ g ∐ h ∐ k where
  f = const (x, 1)
  g a = (a, 0)
  h = bin_aux (+) (*)
  k = un_aux negate (*) expd
-- =============================================================================
-- *** Propriedades
{- clipboard para testar várias propriedades
do {quickCheck prop_in_out_idExpAr; quickCheck prop_out_in_idExpAr; quickCheck prop_sum_idr; quickCheck prop_sum_idl; quickCheck prop_product_idr; quickCheck prop_product_idl ; quickCheck prop_e_id; quickCheck prop_negate_id; quickCheck prop_double_negate; quickCheck prop_optimize_respects_semantics; quickCheck prop_const_rule; quickCheck prop_var_rule}
-}

prop_in_out_idExpAr ∷ (Eq a) ⇒ ExpAr a → Bool
prop_in_out_idExpAr = inExpAr . outExpAr .==. id

prop_out_in_idExpAr ∷ (Eq a) ⇒ OutExpAr a → Bool
prop_out_in_idExpAr = outExpAr . inExpAr .==. id

prop_sum_idr ∷ (Floating a, Real a) ⇒ a → ExpAr a → Bool
prop_sum_idr a exp = eval_exp a exp .=?=. sum_idr
  where sum_idr = eval_exp a (Bin Sum exp (N 0))

prop_sum_idl ∷ (Floating a, Real a) ⇒ a → ExpAr a → Bool
prop_sum_idl a exp = eval_exp a exp .=?=. sum_idl
  where sum_idl = eval_exp a (Bin Sum (N 0) exp)

prop_product_idr ∷ (Floating a, Real a) ⇒ a → ExpAr a → Bool
prop_product_idr a exp = eval_exp a exp .=?=. prod_idr
  where prod_idr = eval_exp a (Bin Product exp (N 1))

prop_product_idl ∷ (Floating a, Real a) ⇒ a → ExpAr a → Bool
prop_product_idl a exp = eval_exp a exp .=?=. prod_idl
  where prod_idl = eval_exp a (Bin Product (N 1) exp)

prop_e_id ∷ (Floating a, Real a) ⇒ a → Bool
prop_e_id a = eval_exp a (Un E (N 1)) == expd 1

prop_negate_id ∷ (Floating a, Real a) ⇒ a → Bool
prop_negate_id a = eval_exp a (Un Negate (N 0)) == 0

prop_double_negate ∷ (Floating a, Real a) ⇒ a → ExpAr a → Bool
prop_double_negate a exp =
  eval_exp a exp .=?=. eval_exp a (Un Negate (Un Negate exp))

prop_optimize_respects_semantics
  ∷ (Floating a, Real a) ⇒ a → ExpAr a → Bool
prop_optimize_respects_semantics a exp =
  eval_exp a exp .=?=. optmize_eval a exp

prop_const_rule ∷ (Real a, Floating a) ⇒ a → Bool
prop_const_rule a = sd (N a) == N 0

prop_var_rule ∷ Bool
prop_var_rule = sd X == N 1

prop_sum_rule ∷ (Real a, Floating a) ⇒ ExpAr a → ExpAr a → Bool
prop_sum_rule exp1 exp2 = sd (Bin Sum exp1 exp2) == sum_rule
  where sum_rule = Bin Sum (sd exp1) (sd exp2)

prop_product_rule ∷ (Real a, Floating a) ⇒ ExpAr a → ExpAr a → Bool
prop_product_rule exp1 exp2 = sd (Bin Product exp1 exp2) == prod_rule
 where
  prod_rule = Bin Sum (Bin Product exp1 (sd exp2)) (Bin Product (sd exp1) exp2)

prop_e_rule ∷ (Real a, Floating a) ⇒ ExpAr a → Bool
prop_e_rule exp = sd (Un E exp) == Bin Product (Un E exp) (sd exp)

prop_negate_rule ∷ (Real a, Floating a) ⇒ ExpAr a → Bool
prop_negate_rule exp = sd (Un Negate exp) == Un Negate (sd exp)

prop_congruent ∷ (Floating a, Real a) ⇒ a → ExpAr a → Bool
prop_congruent a exp = ad a exp .=?=. eval_exp a (sd exp)
-- =============================================================================
-- ** Problema 2

-- =============================================================================
-- *** Código Fornecido
fib' ∷ (Integral c, Num b) ⇒ c → b
fib' = p1 . for loop init
 where
  loop (fib, f) = (f, fib + f)
  init = (1, 1)

f' ∷ (Integral c, Num b) ⇒ b → b → b → c → b
f' a b c = p1 . for loop init
 where
  loop (f, k) = (f + k, k + 2 * a)
  init = (c, a + b)

catdef ∷ Integer → Integer
catdef n = div (fac ((2 * n))) ((fac ((n + 1)) * (fac n)))

oracle ∷ Num a ⇒ [a]
oracle =
  [ 1
  , 1
  , 2
  , 5
  , 14
  , 42
  , 132
  , 429
  , 1430
  , 4862
  , 16796
  , 58786
  , 208012
  , 742900
  , 2674440
  , 9694845
  , 35357670
  , 129644790
  , 477638700
  , 1767263190
  , 6564120420
  , 24466267020
  , 91482563640
  , 343059613650
  , 1289904147324
  , 4861946401452
  ]
-- =============================================================================
-- *** Propriedades
prop_cat = (>= 0) .==>. (catdef .==. cat)
-- =============================================================================
-- *** Solução
loop ∷ Integral c ⇒ (c, c, c) → (c, c, c)
loop = g where g (a, b, c) = (div (a * b) c, b + 4, c + 1)

inic ∷ (Num a, Num b, Num c) ⇒ (a, b, c)
inic = (1, 2, 2)

prj ∷ (a, b, c) → a
prj = p where p (a, _, _) = a

cat ∷ (Integral c1, Integral c2) ⇒ c1 → c2
cat = prj . for loop inic
-- =============================================================================
-- ** Problema 3

-- =============================================================================
-- *** Código Fornecido
linear1d ∷ Rational → Rational → OverTime Rational
linear1d a b = formula a b
 where
  formula ∷ Rational → Rational → Float → Rational
  formula x y t = ((1.0 ∷ Rational) - (toRational t)) * x + (toRational t) * y

type NPoint = [Rational]
type OverTime a = Float → a
-- =============================================================================
-- *** Propriedades
prop_calcLine_def ∷ NPoint → NPoint → Float → Bool
prop_calcLine_def p q d = calcLine p q d == zipWithM linear1d p q d

prop_bezier_sym ∷ [[Rational]] → Gen Bool
prop_bezier_sym l = all (< delta) . calc_difs . bezs <$> elements ps
 where
  calc_difs =
    (\(x, y) → zipWith (\w v → if w >= v then w - v else v - w) x y)
  bezs t =
    ( deCasteljau l           t
    , deCasteljau (reverse l) (fromRational (1 - (toRational t)))
    )
  delta = 1e-2
-- =============================================================================
-- *** Solução


type ℚ = Rational
toℚ ∷ Real a ⇒ a → ℚ
toℚ = toRational
fromℚ ∷ Fractional a ⇒ ℚ → a
fromℚ = fromRational

calcLine ∷ NPoint → (NPoint → OverTime NPoint)
calcLine = cataList h where
    h = either f g
    f _ _ = nil
    g _ [] =  nil
    g (d,f) (x:xs) =  \z → concat $ (sequenceA [singl . linear1d d x, f xs]) z

calcLineAlt ∷ NPoint → (NPoint → OverTime NPoint)
calcLineAlt xs ys e = cataList (either nil h) ((fmap (uncurry linear1d) $ (uncurry zip) (xs,ys))) where
    h (f, fs) = f e : fs


seq' l a = cataList (either nil (\(f,fs) → f a : fs)) l

seq2 = \l → \a → cataList (either nil (\(f,fs) → f a : fs)) l

calcLine6 = sequenceA ° (z ° zip) where z = fmap (uncurry linear1d)
calcLine2 = (sequenceA . z) ° zip where z = fmap (uncurry linear1d)

out ∷ [a] × [b] → Either () ((a × b) × ([a] × [b]))
out = \case
  ([],_)           → Left ()
  (_,[])           → Left ()
  ((a:as), (b:bs)) → Right ((a,b) , (as,bs))

zip' ∷ [a] × [b] → [a × b]
zip' = anaList out

zipWith' ∷ ((a × b) → c) → ([a] × [b]) → [c]
zipWith' f = fmap f . zip'

sequenceA' ∷ [t → a] → t → [a]
sequenceA' = flip aux where
  aux a = cataList (either nil h)  where
    h (f, fs) = f a : fs

zipWithM' ∷ ((a × b) → t → c) → ([a] × [b]) → t → [c]
zipWithM' f = sequenceA' . zipWith' f

calcLine' ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine' = curry aux where
  aux = zipWithM' (uncurry linear1d)


-- Tests

(°) ∷ (Functor f, Functor g) ⇒ (a → b) → f (g a) → f (g b)
(°) = fmap . fmap
(//) = (<<) (.)
(<<) = flip (.)


zipWithFree :: ((a, b1) → b2, ([a], [b1])) → [b2]
zipWithFree =  uncurry $ fmap << (uncurry zip <<)

--
calcLine3 e = cataList k where
  k = z . (baseList (uncurry linear1d) id)
  z = either nil h
  h (f, fs) = f e : fs

--

deCasteljau :: [NPoint] -> OverTime NPoint
deCasteljau = hyloAlgForm alg  coalg where
   coalg = (id ⊕ id ⊕ split init tail) . outSL
   alg = const nil ∐ a
   a = const ∐ b
   b (e,d) pt = (calcLine (e pt) (d pt)) pt

outSL :: [a] -> () ∐ a ∐ [a]
outSL [] = i1 ()
outSL [a] = i2 (i1 a)
outSL l = i2 (i2 l)

hyloAlgForm ::
  (() ∐ b ∐ c × c -> c) ->
  (a -> d ∐ b ∐ a × a) -> a -> c
hyloAlgForm = h where
    h a b = cataCastel a . anaCastel b

newtype Castel' a = Castel' (() ∐ a ∐ Castel a × Castel a)
data Castel a = Empty | Single a | InitTail (Castel a × Castel a) deriving Show

inCastel :: b ∐ a ∐ Castel a × Castel a -> Castel a
inCastel = const Empty ∐ Single ∐ InitTail

outCastel :: Castel a -> () ∐ a ∐ Castel a × Castel a
outCastel Empty = i1 ()
outCastel (Single a) = i2 (i1 a)
outCastel (InitTail (e,d)) = i2 (i2 (e,d))

fC :: (a -> d) -> b1 ∐ b2 ∐ a × a -> b1 ∐ b2 ∐ d × d
fC f = id ⊕ id ⊕ f × f

cataCastel :: (() ∐ b ∐ d × d -> d) -> Castel b -> d
cataCastel f = f . fC (cataCastel f) . outCastel
anaCastel :: (a1 -> b ∐ a2 ∐ a1 × a1) -> a1 -> Castel a2
anaCastel g = inCastel . fC (anaCastel g) . g
-- =============================================================================
-- ** Problema 4

-- =============================================================================
-- *** Propriedades
prop_avg ∷ (Ord b, Fractional b) ⇒ [b] → Property
prop_avg = nonempty .==>. diff .<=. const 0.000001
 where
  diff l = avg l - (avgLTree . genLTree) l
  genLTree = anaLTree lsplit
  nonempty = (> [])
-- =============================================================================
-- *** Código fornecido
avg ∷ Fractional b ⇒ [b] → b
avg = p1 . avg_aux
-- =============================================================================
-- *** Solução

outL :: [a] -> a ∐ a × [a]
outL [a] = i1 a
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
-- =============================================================================
-- * Programação dinâmica por recursividade múltipla
e' ∷ (Fractional c1, Integral c2) ⇒ c1 → c2 → c1
e' x = prj . for loop init
 where
  init = (1, x, 2)
  loop (e, h, s) = (h + e, x / s * h, 1 + s)
  prj (e, h, s) = e
-- =============================================================================
-- * Código Extra para Problema 3

-- =============================================================================
-- ** 2D
bezier2d ∷ [NPoint] → OverTime (Float, Float)
bezier2d [] = const (0, 0)
bezier2d l  = \z →
  (fromRational >< fromRational) . (\[x, y] → (x, y)) $ ((deCasteljau l) z)
-- =============================================================================
-- ** Modelo
data World = World
  { points ∷ [NPoint]
  , time   ∷ Float
  }

initW ∷ World
initW = World [] 0

tick ∷ Float → World → World
tick dt world = world { time = (time world) + dt }

actions ∷ Event → World → World
actions (EventKey (MouseButton LeftButton) Down _ p) world =
  world { points = (points world) ++ [(\(x, y) → map toRational [x, y]) p] }
actions (EventKey (SpecialKey KeyDelete) Down _ _) world =
  world { points = cond (== []) id init (points world) }
actions _ world = world

scaleTime ∷ World → Float
scaleTime w = (1 + cos (time w)) / 2

bezier2dAtTime ∷ World → (Float, Float)
bezier2dAtTime w = (bezier2dAt w) (scaleTime w)

bezier2dAt ∷ World → OverTime (Float, Float)
bezier2dAt w = bezier2d (points w)

thicCirc ∷ Picture
thicCirc = ThickCircle 4 10

ps ∷ [Float]
ps = map fromRational ps'
 where
  ps' ∷ [Rational]
  ps' = [0, 0.01 .. 1] -- interval
-- =============================================================================
-- ** Gloss
picture ∷ World → Picture
picture world = Pictures
  [ animateBezier (scaleTime world) (points world)
  , Color white . Line . map (bezier2dAt world) $ ps
  , Color blue
  . Pictures
  $ [ Translate (fromRational x) (fromRational y) thicCirc
    | [x, y] <- points world
    ]
  , Color green $ Translate cx cy thicCirc
  ]
  where (cx, cy) = bezier2dAtTime world
-- =============================================================================
-- ** Animação
animateBezier ∷ Float → [NPoint] → Picture
animateBezier _ []  = Blank
animateBezier _ [_] = Blank
animateBezier t l   = Pictures
  [ animateBezier t (init l)
  , animateBezier t (tail l)
  , Color red . Line $ [a, b]
  , Color orange $ Translate ax ay thicCirc
  , Color orange $ Translate bx by thicCirc
  ]
 where
  a@(ax, ay) = bezier2d (init l) t
  b@(bx, by) = bezier2d (tail l) t
-- =============================================================================
-- ** Propriedades e main
runBezier ∷ IO ()
runBezier =
  play (InWindow "Bézier" (600, 600) (0, 0)) black 50 initW picture actions tick

runBezierSym ∷ IO ()
runBezierSym =
  quickCheckWith (stdArgs { maxSize = 20, maxSuccess = 200 }) prop_bezier_sym
-- =============================================================================
-- *** Compilação e execução dentro do interpretador
main ∷ IO ()
main = runBezier

run = do
  system "ghc cp2021t"
  system "./cp2021t"

-- * QuickCheck

instance Arbitrary UnOp where
  arbitrary = elements [Negate, E]

instance Arbitrary BinOp where
  arbitrary = elements [Sum, Product]

instance (Arbitrary a) ⇒ Arbitrary (ExpAr a) where
  arbitrary = do
    binop <- arbitrary
    unop  <- arbitrary
    exp1  <- arbitrary
    exp2  <- arbitrary
    a     <- arbitrary

    frequency
      . map (id >< pure)
      $ [(20, X), (15, N a), (35, Bin binop exp1 exp2), (30, Un unop exp1)]
-- =============================================================================
-- * Outras funções auxiliares
infixr 5 .=?=.
(.=?=.) ∷ Real a ⇒ a → a → Bool
(.=?=.) x y = (toRational x) == (toRational y)

infixr 0 .==>.
(.==>.) ∷ (Testable prop) ⇒ (a → Bool) → (a → prop) → a → Property
p .==>. f = \a → p a ==> f a

infixr 0 .<==>.
(.<==>.) ∷ (a → Bool) → (a → Bool) → a → Property
p .<==>. f = \a → (p a ==> property (f a)) .&&. (f a ==> property (p a))

infixr 4 .==.
(.==.) ∷ Eq b ⇒ (a → b) → (a → b) → (a → Bool)
f .==. g = \a → f a == g a

infixr 4 .<=.
(.<=.) ∷ Ord b ⇒ (a → b) → (a → b) → (a → Bool)
f .<=. g = \a → f a <= g a

infixr 4 .&&&.
(.&&&.) ∷ (a → Bool) → (a → Bool) → (a → Bool)
f .&&&. g = \a → ((f a) && (g a))