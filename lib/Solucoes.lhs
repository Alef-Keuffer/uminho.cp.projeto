\documentclass{article}
\usepackage[a4paper,left=3cm,right=2cm,top=2.5cm,bottom=2.5cm]{geometry}
%include polycode.fmt
\usepackage{stmaryrd}
\usepackage[math-style=ISO]{unicode-math}

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
module SolucoesLit where

import           Cp
import           List                    hiding ( fac )
import Solucoes (linear1d)
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

zipWithM'18 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'18 = (flip ° id) aux where
    aux f e = hyloList (either nil h) out where
        h = cons . (flip f e × id)

zipWithM'19 ∷ ((a × b) → c → d) → c → ([a] × [b]) → [d]
zipWithM'19 f e = hyloList (either nil h) out where
    h = cons . (flip f e × id)

--zipWithM' ∷ (a → b → ((→) m c)) → [a] → [b] → ((→) m [c])

zipWithM' ∷ (a → b → p → d) → [a] → [b] → p → [d]
zipWithM' f xs ys e = zipWithM'19 (uncurry f) e (xs,ys)

calcLine' ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine' = zipWithM' linear1d

calcLine'1 ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine'1 = curry (zipWithM'18 (uncurry linear1d))

calcLine'' :: (Float, [ℚ] × [ℚ]) -> [ℚ]
calcLine'' = uncurry (zipWithM'19 (uncurry linear1d))

data NonEmpty a = a :| [a] deriving Show

\end{code}
%}

\end{document}
