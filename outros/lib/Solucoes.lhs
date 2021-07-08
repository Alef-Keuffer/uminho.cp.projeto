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
%format (baseList (f) (g)) = "B_{[]}(" f "," g ")"

%format NEL = "{_{*{\backslash}{\{"[]"\}}}}"
%format NL = "_{"NList"}"
%format outL = "out" NL
%format recL = "rec" NL
%format (cataL (g)) = "\cata{"g"}" NL

%format i1
%format i2
%format (either (f) (g)) = "\either{" f "}{" g "}"
%format ⊕ = "+"

%format p1
%format p2

%format (const (e)) = "\const{" e "}"
%format nil =  "\const{" [] "}"

%format (split (f) (g)) = "{⟨" f "," g "⟩}"

%format (uncurry (f)) = "{\uncurry{" f "}}"
%format curry (f) = "\curry{" f "}"
%format cons = "\curry{" : "}"

%format outUnOp = "out_{" UnOp "}"
%format outBinOp = "out_{" BinOp "}"

%format out = "out_{A^{*}\times B^{*}}"

%format (cataCastel (g)) = "\cata{" g "}_{Castel}"
%format (anaCastel (g)) = "\ana{" g "}_{Castel}"
%format inCastel = "in_{" Castel "}"
%format outCastel = "out_{" Castel "}"
%format fC  = "T_{Castel}"
\usepackage{Solucoes}


\begin{document}
%if False
\begin{code}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UnicodeSyntax             #-}


module Solucoes where
import Control.Applicative ( Applicative(liftA2) )
import Cp
    ( (-|-), (><), add, ap, cons, i1, i2, mul, nil, p1, split )
import Given
    ( baseExpAr,
      expd,
      linear1d,
      BinOp(..),
      ExpAr(..),
      NPoint,
      OverTime,
      UnOp(..) )
import LTree ( cataLTree, LTree )
import List ( anaList, baseList, cataList, hyloList )
import Nat ( for )
\end{code}
%endif


\section{Resolução Alternativa}
\subsection{Questão 1}
Para criar uma interpretação de um tipo |A| como um tipo |B|. Assim, por exemplo,
posso definir que uma expressão $x$ do tipo |ExpAr a| pode ser interpretada como
|Left ()| do tipo |OutExpAr a|.
\begin{code}
class Interpretation a b where
    to ∷ a → b
\end{code}


A fim de diminuir o número de parenteses e facilitar a legibilidade defini as funções:

bimap de tuplos (|(,)|):
\begin{code}
infixr 6 ×
type a × b = (a, b)
(×) ∷ (a → b) → (c → d) → (a, c) → (b, d)
(×) = (><)
\end{code}


bimap de |Either|:
\begin{code}
infixr 4 ⊕
(⊕) ∷ (a → b) → (c → d) → a ∐ c → b ∐ d
(⊕) = (-|-)
\end{code}


\begin{code}
infixr 4 ∐
type (∐) = Either
(∐) ∷ (a → c) → (b → c) → a ∐ b → c
(∐) = either
\end{code}


Novamente, para simplificar a tipagem:
\begin{code}
type BinExp d = BinOp × ExpAr d × ExpAr d
\end{code}
Note que por conta de precedência |BinExp d ≡ BinOp × (ExpAr d × ExpAr d)|.

\begin{code}
type UnExp d = UnOp × ExpAr d
\end{code}


Isso é uma redefinição do que o Professor definiu.
É igual excepto os símbolos mais fáceis de ler.
\begin{code}
type OutExpAr a = () ∐ a ∐ BinExp a ∐ UnExp a
\end{code}


Vamos criar uma interpretação de |ExpAr a| como |OutExpAr a|.
Ou seja, essa interpretação é |outExpAr|.
\begin{code}
instance Interpretation (ExpAr a) (OutExpAr a) where
  to X             = i1 ()
  to (N a       )  = i2 $ i1 a
  to (Bin op l r)  = i2 $ i2 $ i1 (op, (l, r))
  to (Un op a   )  = i2 $ i2 $ i2 (op, a)
\end{code}


Como dito, temos
\begin{code}
outExpAr ∷ ExpAr a → OutExpAr a
outExpAr = to ∷ ExpAr a → OutExpAr a
\end{code}


Agora vou interpretar os símbolos que representam as operações como
as funções que representam essas operações.


Interpretamos cada símbolo |BinOp| como uma função |(c,c) → c|.
Por exemplo, o símbolo |Sum| é interpretado como a função |add|.
\begin{code}
instance (Num c) ⇒ Interpretation BinOp ((c, c) → c) where
  to Sum      = add
  to Product  = mul
\end{code}


Na nossa linguagem mais usual:
\begin{code}
outBinOp ∷ Num c ⇒ BinOp → (c × c) → c
outBinOp = to ∷ (Num c) ⇒ BinOp → (c × c → c)
\end{code}


Interpretamos cada símbolo |UnOp| como uma função |c→c| onde |c|
é da classe |Floating|.
\begin{code}
instance (Floating c) ⇒ Interpretation UnOp (c → c) where
  to Negate  = negate
  to E       = Prelude.exp
\end{code}


Na nossa linguagem mais usual
\begin{code}
outUnOp ∷ Floating c ⇒ UnOp → (c → c)
outUnOp = to ∷ (Floating c) ⇒ UnOp → (c → c)
\end{code}


\begin{code}
recExpAr ∷ (a → e) → b ∐ c ∐ d × a × a ∐ g × a → b ∐ c ∐ d × e × e ∐ g × e
recExpAr f = baseExpAr id id id f f id f

g_eval_exp ∷ Floating c ⇒ c → b ∐ c ∐ BinOp × c × c ∐ UnOp × c → c
g_eval_exp a = const a ∐ id ∐ ap . (outBinOp × id) ∐ ap . (outUnOp × id)
\end{code}


Nós optimizamos 4 casos e para os outros usamos |outExpAr|:
\begin{code}
clean ∷ (Eq a, Num a) ⇒ ExpAr a → OutExpAr a
clean = \case
  (Un   E        (N 0)       )  → tag 1
  (Un   Negate   (N 0)       )  → tag 0
  (Bin  Product  (N 0)  _    )  → tag 0
  (Bin  Product  _      (N 0))  → tag 0
  a                             → outExpAr a
  where tag = i2 . i1
\end{code}


\begin{code}
gopt ∷ Floating a ⇒ a → () ∐ a ∐ BinOp × a × a ∐ UnOp × a → a
gopt = g_eval_exp
\end{code}


Baseado na função |dup| definida em \texttt{Cp.hs}:
\begin{code}
type Dup d = d × d
\end{code}

\begin{code}
type Bin d  = BinOp × Dup d
type Un d   = UnOp × d
\end{code}

%{
%format e1
%format e2
%format d1
%format d2
%format un1  = "\circleddash"
%format un2 = "\boxminus"
%format ^^ = "\;"
%format '(⊛)' = "⊛"
%subst dummy = "{}"
%format bin_aux (f) (g) h = "{bin}_{aux}" ^^ f ^^ g ^^ h
%format un_aux f (g) h i = "{un}_{aux}" ^^ f ^^ g ^^ h ^^ i


A fim de criar código sucinto extrai o que se
repetia nas funções |sd_gen| e |ad_gen| do Tiago.
\begin{code}
bin_aux ∷ (t → t → t) → (t → t → t) → (BinOp, Dup (Dup t)) → Dup t
bin_aux (♢) (□) (op, ((e1, d1), (e2, d2))) = case op of
  Sum      → (e1 ♢ e2, d1 ♢ d2)
  Product  → (e1 □ e2, (e1 □ d2) ♢ (d1 □ e2))

un_aux ∷ (a → b) → (b → a → b) → (a → b) → (UnOp, Dup a) → Dup b
un_aux un1 (⊛) un2 (op, (e, d)) = case op of
  Negate  → (un1 e, un1 d)
  E       → (un2 e, (un2 e) ⊛ d)
\end{code}
%}

%format bin_aux = "{bin}_{aux}"
%format un_aux = "{un}_{aux}"


Agora podemos escrever:
\begin{code}
sd_gen ∷ Floating a ⇒ () ∐ a ∐ Bin (Dup (ExpAr a)) ∐ Un (Dup (ExpAr a)) → Dup (ExpAr a)
sd_gen = f ∐ g ∐ h ∐ k where
  f    = const ((X, N 1))
  g a  = (N a, N 0)
  h    = bin_aux (Bin Sum) (Bin Product)
  k    = un_aux (Un Negate) (Bin Product) (Un E)

ad_gen ∷ Floating a ⇒ a → () ∐ a ∐ (BinOp, Dup (Dup a)) ∐ (UnOp, Dup a) → Dup a
ad_gen x = f ∐ g ∐ h ∐ k where
  f    = const ((x, 1))
  g a  = (a, 0)
  h    = bin_aux (+) (*)
  k    = un_aux negate (*) expd
\end{code}

\subsection{Problema 2}
\begin{code}
loop ∷ Integral c ⇒ (c, c, c) → (c, c, c)
loop = g where g (a, b, c) = (div (a * b) c, b + 4, c + 1)

inic ∷ (Num a, Num b, Num c) ⇒ (a, b, c)
inic = (1, 2, 2)

prj ∷ (a, b, c) → a
prj = p where p (a, _, _) = a

cat ∷ (Integral c1, Integral c2) ⇒ c1 → c2
cat = prj . for loop inic
\end{code}

\subsection{Problema 3}
%{
%format sequenceA'1 = sequenceA'
%format calcLine'1 = calcLine
%format (fmap (f)) = "T_{[]}" f
%format (fmap2 (f)) = "T" f
%format zipWithM'2 = zipWithM'
%format zipWithM'3 = zipWithM'
%format zipWithM''4 = zipWithM'
%format zipWithM''5 = zipWithM'
%format zipWithM''6 = zipWithM'
%format zipWithM''7 = zipWithM'
%format zipWithM''8 = zipWithM'
%format zipWithM''9 = zipWithM'
%format zipWithM''10 = zipWithM'
%format zipWithM''11 = zipWithM'
%format zipWithM''12 = zipWithM'
%format zipWithM''13 = zipWithM'
%format zipWithM''14 = zipWithM'
%format zipWithM''15 = zipWithM'
É interessante ver que podemos ver |calcLine| como um hilomorfismo.

A ideia que levou a isso parte da definição alternativa
|calcLine = zipWithM linear1d|.


Sabemos que:
\begin{spec}
zipWithM ∷ (Applicative m) ⇒ (a → b → m c) → [a] → [b] → m [c]
zipWithM f xs ys = sequenceA (zipWith f xs ys)
\end{spec}


Percebi que podia escrever uma função (|curry zip|):
\begin{code}
zip' ∷ [a] × [b] → [a × b]
zip' = anaList out
\end{code}


Desde que transforme os pares de lista de uma forma que respeite
o funcionamento de |zipWith| que será descrito em seguida dessa definição:
\begin{code}
out ∷ [a] × [b] → Either () ((a × b) × ([a] × [b]))
out = \case
  ([],_)            → i1 ()
  (_,[])            → i1 ()
  (a : as, b : bs)  → i2 ((a,b) , (as,bs))
\end{code}


|zipWith| pega uma função (de aridade 2), por exemplo, $f$, e
duas listas (digamos $a$ e $b$) e devolve uma lista (digamos $c$)
onde $c[i] = f(a[i],b[i])$ para todo $0≤i≤|min(length a,length b)|$.


Ora, então posso pegar uma função curried e pegar uma par de listas.
Transformo o par de listas numa lista de pares com |out| e aplico a
função argumento em cada um dos pares. Logo tenho a seguinte definição:
\begin{code}
zipWith' ∷ ((a × b) → c) → ([a] × [b]) → [c]
zipWith' f = (fmap f) . zip'
\end{code}


A próxima etapa é baseada nas seguintes definições
\begin{spec}
sequenceA ∷ Applicative f ⇒ t (f a) → f (t a)
sequenceA = traverse id

traverse ∷ Applicative f ⇒ (a → f b) → t a → f (t b)
traverse f = sequenceA . fmap f
\end{spec}


Ora, vamos ver como |traverse| é definido para listas
\begin{spec}
instance Traversable [] where
    traverse f = foldr cons_f (pure [])
      where cons_f x ys = liftA2 (:) (f x) ys
\end{spec}


Vou criar um |sequenceA'| (será uma versão menos genérica de |sequenceA| uma vez que estamos
sendo específicos no trabalho com listas).
\begin{spec}
sequenceA = traverse id = foldr cons_f (pure []) where
  cons_f x ys = liftA2 (:) x ys
\end{spec}


Já fizemos o catamorfismo para |foldr| nas aulas:
\begin{code}
foldrC :: (a -> b -> b) -> b -> [a] -> b
foldrC f u = cataList (either (const u) (uncurry f))
\end{code}


Então temos:
\begin{code}
sequenceA'1 ∷ Applicative f ⇒ [f a] → f [a]
sequenceA'1 = cataList (either b (uncurry g)) where
  b = (const (pure []))
  g x ys = liftA2 (:) x ys
\end{code}


Sabemos que, em |Applicative ((→) r)|, |pure = 'const'| e
|liftA2 q f g x = q (f x) (g x)|. Logo:
\begin{code}
sequenceA' ∷ [a → b] → a → [b]
sequenceA' = cataList (either b (uncurry g)) where
  b = (const (const []))
  g x ys = (\z → x z : ys z)
\end{code}


%if False
\begin{code}
infixr 9 °
(°) ∷ (Functor f, Functor g) ⇒ (a → b) → f (g a) → f (g b)
(°) = fmap . fmap

(°°) ∷ (Functor f1, Functor g, Functor f2) ⇒(a → b) → f1 (g (f2 a)) → f1 (g (f2 b))
(°°) = (°) . fmap

infixr 8 <<
(<<) ∷ (a1 → b → c) → (a2 → b) → a1 → a2 → c
g << f = (. f) . g
\end{code}
%endif


Lembre que |zipWithM'| como vimos recebia duas listas. No nosso caso
essas duas listas (digamos |xs| e |ys|) estão em um só argumento |t=(xs,ys)|
Agora, estamos em condições de escrever:
\def\commentbegin{\quad\{\ }
\def\commentend{\}}
\begin{code}
zipWithM'2 f t  = sequenceA' (zipWith' f t)

{- |(.) f g = \x -> f (g x)| -}

zipWithM'3 f    = sequenceA' . zipWith' f

{- Def-|zipWith'| -}

zipWithM''4 f   = sequenceA' . (fmap f . zip')

{- Assoc-comp -}

zipWithM''5 f   = (sequenceA' . fmap f) . zip'

{- Def-|sequenceA'| -}

zipWithM''6 f   = (cataList (either (const (const [])) (uncurry g)) . fmap f) . zip' where g x ys = (\z → x z : ys z)

{- Absorção-cata -}

zipWithM''7 f   = cataList (either (const (const [])) (uncurry g) . (baseList f id)) . zip' where g x ys = (\z → x z : ys z)

{- Def-baseList -}

zipWithM''8 f   = cataList (either (const (const [])) (uncurry g) . (id ⊕ f × id)) . zip' where g x ys = (\z → x z : ys z)

{- Absorção-|⊕|; Natural-const -}

zipWithM''9 f   = cataList (either (const (const [])) (uncurry g . (f × id))) . zip' where g x ys  = (\z → x z : ys z)

{- |(.) f g = \x -> f (g x)| -}

zipWithM''10 f  = cataList (either (const (const [])) (\(a,b) → (uncurry g) ((f × id) (a,b)))) . zip' where g x ys = (\z → x z : ys z)

{- Def-|×| -}

zipWithM''11 f  = cataList (either (const (const [])) (\(a,b) → g (f a, b))) . zip' where g (x, ys) = (\z → x z : ys z)

{- Deixe que |h= (\(a,b) → g (f a, b))|; Notação-|\| -}

zipWithM''12 f  = cataList (either (const (const [])) h) . zip' where h (a,b) = (\z → (f a) z: b z)

{- Def-|zip'|; Notação-|\| -}

zipWithM''13 f  = cataList (either (const (const [])) h) . (anaList out) where h (a,b) z = (f a) z : b z

{- catamorfismo após anamorfismo é um hilomorfismo -}

zipWithM''14 f  = hyloList (either (const (const [])) h) out where h (a,b) z = (f a) z : b z

zipWithM''15 f  = hyloList (either (const (const [])) h) out where h (a,b) = cons . split (f a) b
\end{code}


Portanto, lembrando que |calcLine = zipWithM linear1d|
e tendo em mente que |calcLine ∷ [ℚ] → [ℚ] → Float → [ℚ]|,
mas |zipWithM''15 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]|
\begin{code}
calcLine'1 = curry (zipWithM''15 (uncurry linear1d))

{- Def-|zipWithM'15| -}

calcLine = curry (hyloList (either (const (const [])) h) out) where h (a,b) = cons . split (uncurry linear1d a) b
\end{code}

%if False
\begin{code}
calcLine'1 ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine ∷ [ℚ] → [ℚ] → Float → [ℚ]

zipWithM'2 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'3 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]

zipWithM''4 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM''5 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM''6 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM''7 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM''8 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM''9 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM''10 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM''11 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM''12 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM''13 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM''14 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM''15 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]


zipWithM'4 =  sequenceA' ° zipWith'
zipWithM'5 =  sequenceA' ° zipWith'
zipWithM'6 =  sequenceA' ° (fmap << zip')
zipWithM'7 =  (sequenceA' ° fmap) << zip'
zipWithM'8 =  ((fmap) sequenceA' . fmap) << zip'
zipWithM'4 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'5 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'6 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'7 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
zipWithM'8 ∷ ((a × b) → c → d) → ([a] × [b]) → c → [d]
\end{code}
%endif
%}

\begin{code}
deCasteljau ∷ [NPoint] → OverTime NPoint
deCasteljau = hyloAlgForm alg  coalg where
   coalg = (id ⊕ id ⊕ split init tail) . outSL
   alg = const nil ∐ a
   a = const ∐ b
   b (e,d) pt = calcLine (e pt) (d pt) pt

outSL ∷ [a] → () ∐ a ∐ [a]
outSL = \case
  []   → i1 ()
  [a]  → i2 (i1 a)
  l    → i2 (i2 l)

hyloAlgForm ∷ (() ∐ b ∐ c × c → c) → (a → d ∐ b ∐ a × a) → a → c
hyloAlgForm = h where
    h a b = cataCastel a . anaCastel b

newtype Castel' a = Castel' (() ∐ a ∐ Castel a × Castel a)
data Castel a = Empty | Single a | InitTail (Castel a × Castel a) deriving Show

inCastel ∷ b ∐ a ∐ Castel a × Castel a → Castel a
inCastel = const Empty ∐ Single ∐ InitTail

outCastel ∷ Castel a → () ∐ a ∐ Castel a × Castel a
outCastel = \case
  Empty           → i1 ()
  Single a        → i2 (i1 a)
  InitTail (e,d)  → i2 (i2 (e,d))

fC ∷ (a → d) → b1 ∐ b2 ∐ a × a → b1 ∐ b2 ∐ d × d
fC f = id ⊕ id ⊕ f × f

cataCastel ∷ (() ∐ b ∐ d × d → d) → Castel b → d
cataCastel f = f . fC (cataCastel f) . outCastel
anaCastel ∷ (a1 → b ∐ a2 ∐ a1 × a1) → a1 → Castel a2
anaCastel g = inCastel . fC (anaCastel g) . g
\end{code}

\subsection{Problema 4}
\begin{code}
outL ∷ [a] → a ∐ a × [a]
outL = \case
  [a]   → i1 a
  (a:x) → i2 (a,x)
\end{code}

\begin{code}
recL ∷ (c → d) → (b1 ∐ b2 × c) → b1 ∐ b2 × d
recL  f   = id ⊕ id × f

cataL ∷ (b ∐ b × d → d) → [b] → d
cataL g   = g . recL (cataL g) . outL
\end{code}

%{
%format a1
%format a2
%format l1
%format l2
\begin{code}
avg_aux ∷ Fractional b ⇒ [b] → (b × b)
avg_aux = cataL (either b q) where
   b a = (a,1)
   q (h,(a,l)) = ((h + (a*l)) / (l+1) ,l+1)

avgLTree ∷ Fractional b ⇒ LTree b → b
avgLTree = p1 . cataLTree gene where
   gene = either g q where
      g a = (a,1)
      q((a1,l1),(a2,l2)) = (((a1*l1)+(a2*l2))/(l1+l2),l1+l2)
\end{code}
%}


%if False
\begin{code}
type ℚ = Rational
toℚ ∷ Real a ⇒ a → ℚ
toℚ = toRational
fromℚ ∷ Fractional a ⇒ ℚ → a
fromℚ = fromRational
\end{code}
%endif


\end{document}