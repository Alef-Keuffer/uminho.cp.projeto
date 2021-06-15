\documentclass{article}
\usepackage[a4paper,left=3cm,right=2cm,top=2.5cm,bottom=2.5cm]{geometry}
%include polycode.fmt
\usepackage{ucs}
\usepackage[utf8x]{inputenc}
\usepackage{bbm}
\usepackage[greek,english]{babel}
\usepackage{autofe}

%%\defaultfontfeatures{ Scale = MatchUppercase }
%%\setmainfont{TeX Gyre Pagella}[Ligatures = {Common, TeX}, Scale = 1.0]
%%\setmathfont{TeX Gyre Pagella Math}
%%\renewcommand{\texfamily}{\familydefault\selectfont}
%%\renewcommand{\tex}[1]{\text{#1}}

%format ℚ = "\BbbQ"
%format sl = "_{" [] "}"
%format (anaList (g)) = "\ana{"g"}" sl
%format (cataList (g)) = "\cata{" g "}" sl
%format (hyloList (f) (g)) = "\hylo{" f "," g "}" sl
%format out = "out_{A^{*}\times B^{*}}"

%format (either (f) (g)) = "\either{" f "}{" g "}"
%format ⊕ = "+"
%format (baseList (f) (g)) = "B_{[]}(" f "," g ")"
%format (const (e)) = "\const{" e "}"
%format nil =  "\const{" [] "}"
%format (split (f) (g)) = "{⟨" f "," g "⟩}"
%format uncurry (f) = "\uncurry{" f "}"
%format curry (f) = "\curry{" f "}"
%format cons = "\curry{" : "}"
\usepackage{Solucoes}

\begin{document}

%if False
\begin{code}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NPlusKPatterns             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# OPTIONS_HADDOCK show-extensions     #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
module SolucoesLit where

import           Cp
import           List                    hiding ( fac )
import Given (linear1d)

\end{code}
%endif

%if False
\begin{code}
type ℚ = Rational

infixr 6 ×
type a × b = (a, b)
(×) ∷ (a → b) → (c → d) → (a, c) → (b, d)
(×) = (><)

infixr 4 ∐
type (∐) = Either
(∐) ∷ (a → c) → (b → c) → a ∐ b → c
(∐) = either

infixr 4 ⊕
(⊕) ∷ (a → b) → (c → d) → a ∐ c → b ∐ d
(⊕) = (-|-)

infixr 9 °
(°) ∷ (Functor f, Functor g) ⇒ (a → b) → f (g a) → f (g b)
(°) = fmap . fmap
\end{code}
%endif

%{
%format (fmap (f)) = "T_{[]}" f
\begin{code}
out  ∷ [a] × [b] → () ∐ ((a × b) × ([a] × [b]))
out  = \case
  ([],_)            → Left ()
  (_,[])            → Left ()
  ((a:as), (b:bs))  → Right ((a,b) , (as,bs))

type OutTuple a b = () ∐ ((a × b) × ([a] × [b]))

class Interpretation a b where
    to ∷ a → b


instance Interpretation (a→b) (OutTuple a b) where
  to = undefined

zip' ∷ [a] × [b] → [a × b]
zip' = anaList out

zipWith' ∷ ((a × b) → c) → ([a] × [b]) → [c]
zipWith' f = (fmap f) . zip'

sequenceA' ∷ [a → b] → a → [b]
sequenceA' l e = cataList (either nil h) l where h (f, fs) = f e : fs

zipWithM'' ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'' f = sequenceA' . zipWith' f

zipWithM'1 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'1 f l e = sequenceA' (zipWith' f l) e

zipWithM'2 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'2 f l e = sequenceA' (anaList ((id ⊕ f × id) . out) l) e

zipWithM'3 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'3 f l e =  cataList (either nil h) $ (fmap f . (anaList out)) l where
    h (g,gs) = g e : gs

zipWithM'4 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'4 f l e = alg $ coalg l where
    alg = cataList (either nil h)
    coalg = anaList (baseList f id . out)
    h (g,gs) = g e : gs

zipWithM'5 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'5 = (flip ° id) aux where
    aux f e = alg . coalg where
        alg        = cataList (either nil h)
        coalg      = anaList (baseList f id . out)
        h (g, gs)  = g e : gs

zipWithM'6 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'6 = (flip ° id) aux where
    aux f e = alg . coalg where
        alg        = cataList (either nil h) . fmap f
        coalg      = anaList out
        h (g, gs)  = g e : gs

zipWithM'7 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'7 = (flip ° id) aux where
    aux f e = alg . coalg where
        alg        = cataList (either nil h . baseList f id)
        coalg      = anaList out
        h (g, gs)  = g e : gs

zipWithM'8 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'8 = (flip ° id) aux where
    aux f e = alg . coalg where
        alg        = cataList (either nil h . (id ⊕ f × id))
        coalg      = anaList out
        h (g, gs)  = g e : gs

zipWithM'9 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'9 = (flip ° id) aux where
    aux f e = alg . coalg where
        alg        = cataList (either nil (h . (f × id)))
        coalg      = anaList out
        h (g, gs)  = g e : gs

zipWithM'10 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'10 = (flip ° id) aux where
    aux f e = alg . coalg where
        alg        = cataList (either nil (h . (f × id)))
        coalg      = anaList out
        h          = cons . (ap × id) . ((split id (const e)) × id)

zipWithM'11 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'11 = (flip ° id) aux where
    aux f e = alg . coalg where
        alg        = cataList (either nil h)
        coalg      = anaList out
        h          = cons . (ap × id) . ((split id (const e)) × id) . (f × id)

zipWithM'12 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'12 = (flip ° id) aux where
    aux f e = alg . coalg where
        alg        = cataList (either nil h)
        coalg      = anaList out
        h          = cons . (ap × id) . ((split f (const e)) × id)

zipWithM'13 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'13 = (flip ° id) aux where
    aux f e = alg . coalg where
        alg        = cataList (either nil h)
        coalg      = anaList out
        h          = cons . (ap . (split f (const e)) × id)

zipWithM'14 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'14 = (flip ° id) aux where
    aux f e = alg . coalg where
        alg        = cataList (either nil h)
        coalg      = anaList out
        h          = cons . (k × id)
        k = ap . split id (const e) . f

zipWithM'15 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'15 = (flip ° id) aux where
    aux f e = alg . coalg where
        alg        = cataList (either nil h)
        coalg      = anaList out
        h          = cons . (flip f e × id)

zipWithM'16 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'16 = (flip ° id) aux where
    aux f e = (cataList (either nil h)) . (anaList out) where
        h   = cons . (flip f e × id)


zipWithM'17 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'17 = (flip ° id) aux where
    aux f e = hyloList (either nil h) out where
        h = cons . (flip f e × id)

zipWithM''17 = (flip ° id) aux where
    aux f e = cataList g where
        g = either nil h
        h = cons . (flip f e × id)

zipWithM'18 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'18 = (flip ° id) aux where
    aux f e = hyloList (either nil h) out where
        h = cons . (flip f e × id)

zipWithM'19 ∷ ((a × b) → c → d) → c → ([a] × [b]) → [d]
zipWithM'19 f e = hyloList (either nil h) out where
    h = cons . (flip f e × id)


zipWithM'''19 :: ((a1 × b) -> a2 -> a3) -> a2 -> ([a1] × [b]) -> [a3]
zipWithM'''19 = rotate2 out . (hyloList . either nil . (cons .)) ° rotate2 id . (×) ° flip

hy ∷ Functor f ⇒ (f b → b) → (a → f a) → a → b
hy g h = fix (\f → g . fmap f . h)


hyl g h = g . fmap (hyl g h) . h


fix :: (a -> a) -> a
fix f = let x = f x in x

rotate1 a b = b a
rotate2 x y y0 = y y0 x

--outTuple :: [a] -> [b] -> () ∐ ((a × b) × ([a] × [b]))
outTuple = out ° (,)

loc a b = (a,b)


rot :: (t -> p -> a -> b -> d) -> t -> a -> b -> p -> d
rot a b c d e = a b e c d

hh = rot zipWithM''19


zipWithM''19 ∷ (a → b → p → d) → p → [a] → [b] → [d]
zipWithM''19 f e = hylo ° (,) where
    hylo = hyloList (either nil h) out
    h = cons . (flip (uncurry f) e × id)


rotate3 f x y z = f y z x

zz1 a b = rotate3 zipWithM'19 (a,b)
--zipWithM' ∷ (a → b → ((→) m c)) → [a] → [b] → ((→) m [c])

zipWithM' ∷ (a → b → p → d) → [a] → [b] → p → [d]
zipWithM' f xs ys e = zipWithM'19 (uncurry f) e (xs,ys)

lla :: ((a × b, [c]) -> [c]) -> ([a],[b]) -> [c]
lla f =  hyloList (either nil f) out

outNat 0 = i1 ()
outNat (n+1) = i2 n


kk :: (a1 -> b -> t -> a2) -> [a1] -> [b] -> t -> [a2]
kk g xs ys p = hyloList (either nil g2) out (xs, ys) where
    g2 = cons . (flip (uncurry g) p >< id)

kk1 :: (a1 -> b -> t -> a2) -> t -> ((a1, b), [a2]) -> [a2]
kk1 g p x = cons (((\ y -> uncurry g y p) >< id) x)

zipWithM''' :: (a1 -> b -> a2 -> d) -> [a1] -> [b] -> a2 -> [d]
zipWithM''' = (. (,)) °° flip . zipWithM'19 . uncurry


infixl 8 °°
(°°) :: (((a1 -> b) -> a1 -> c1) -> c2) -> (a2 -> b -> c1) -> a2 -> c2
f °° g = f . (.) . g

{- zipWithM'19 (uncurry f) e (xs,ys)
= hyloList (either nil h) out where
    h = cons . (flip f e × id)
-}

calcLine' ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine' = zipWithM' linear1d

zipWithM'19PL ∷ ((a × b) → c → d) → c → ([a] × [b]) → [d]
zipWithM'19PL = flip flip out . ((hyloList . either nil . (cons .)) .) . flip flip id . ((×) .) . flip

zipWithM'PL ∷ (a → b → p → d) → [a] → [b] → p → [d]
zipWithM'PL = (. (,)) . (.) . flip . flip flip out . ((hyloList . either nil . (cons .)) .) . flip flip id . ((×) .) . flip . uncurry

calcLine'PL = (flip (flip hyloList out . either nil . (cons .) . (>< id) . flip (uncurry linear1d)) .) . (,)

calcLine'UNPL ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine'UNPL as bs e = hyloList (either nil g1) out (as,bs) where
    g1 = cons . (flip (uncurry linear1d) e × id)


data T t where
    S :: T ((a -> b -> c) -> (a -> b) -> (a -> c))
    K :: T (a -> b -> a)
    I :: T (a -> a)
    C :: T ((a -> b -> c) -> (b -> a -> c))
    B :: T ((b -> c) -> (a -> b) -> (a -> c))
    (:$) :: T (a -> b) -> T a -> T b

class Functor2 f g where
   fmap2 :: f a -> g a

newtype LA f g a = LA (f (g a))


data F a = F a | F1 a
data G a = G a

instance Functor2 F G where
    fmap2 (F a) = G a

--flip flip (,) . ((:::) .) . flip flip out . ((hyloList . either nil . (cons .)) .) . flip flip id . ((><) .) . flip . uncurry
calcLine'2 xs ys e = zipWithM'19 (uncurry linear1d) e (xs,ys)

calcLine'1 ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine'1 = curry (zipWithM'18 (uncurry linear1d))

calcLine'' :: (Float, [ℚ] × [ℚ]) -> [ℚ]
calcLine'' = uncurry (zipWithM'19 (uncurry linear1d))

data NonEmpty a = a :| [a] deriving Show

\end{code}
%}

\end{document}
