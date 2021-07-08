\documentclass[a4paper]{article}

%================= local x=====================================================%
\def\getGif#1{\includegraphics[width=0.3\textwidth]{cp2021t_media/#1.png}}
\let\uk=\emph
\def\aspas#1{``#1"}
%================= lhs2tex=====================================================%
%include polycode.fmt
\usepackage[a4paper,left=3cm,right=2cm,top=2.5cm,bottom=2.5cm]{geometry}
\usepackage{palatino}
\usepackage[colorlinks=true,linkcolor=blue,citecolor=blue]{hyperref}
\usepackage{graphicx}
\usepackage{cp2021t}
\usepackage{subcaption}
\usepackage{adjustbox}
\usepackage{color}
\definecolor{red}{RGB}{255,  0,  0}
\definecolor{blue}{RGB}{0,0,255}
\def\red{\color{red}}
\def\blue{\color{blue}}

\usepackage{stmaryrd}
\usepackage{tikz}
\usepackage[math-style=ISO]{unicode-math}
%format (div (x)(y)) = x "\div " y
%format succ = "\succ "
%format ==> = "\Longrightarrow "
%format map = "\map "
%format length = "\length "
%format fst = "\p1"
%format p1  = "\p1"
%format snd = "\p2"
%format p2  = "\p2"
%format Left = "i_1"
%format Right = "i_2"
%format i1 = "i_1"
%format i2 = "i_2"
%format >< = "\times"
%format >|<  = "\bowtie "
%format |-> = "\mapsto"
%format . = "\comp "
%format .=?=. = "\mathbin{\stackrel{\mathrm{?}}{=}}"
%format (kcomp (f)(g)) = f "\kcomp " g
%format -|- = "+"
%format conc = "\mathsf{conc}"
%format summation = "{\sum}"
%format (either (a) (b)) = "\alt{" a "}{" b "}"
%format (frac (a) (b)) = "\frac{" a "}{" b "}"
%format (uncurry f) = "\uncurry{" f "}"
%format (const f) = "\underline{" f "}"
%format TLTree = "\mathsf{TLTree}"
%format (lcbr (x)(y)) = "\begin{lcbr}" x "\\" y "\end{lcbr}"
%format (split (x) (y)) = "\conj{" x "}{" y "}"
%format (for (f) (i)) = "\for{" f "}\ {" i "}"
%format B_tree = "\mathsf{B}\mbox{-}\mathsf{tree} "
\def\ana#1{\mathopen{[\!(}#1\mathclose{)\!]}}
%format <$> = "\mathbin{\mathopen{\langle}\$\mathclose{\rangle}}"
%format (cataA (f) (g)) = "\cata{" f "~" g "}_A"
%format (anaA (f) (g)) = "\ana{" f "~" g "}_A"
%format (cataB (f) (g)) = "\cata{" f "~" g "}_B"
%format (cata (f)) = "\cata{" f "}"
%format (anaB (f) (g)) = "\ana{" f "~" g "}_B"
%format Either a b = a "+" b
%format fmap = "\mathsf{fmap}"
%format NA   = "\textsc{na}"
%format NB   = "\textsc{nb}"
%format inT = "\mathsf{in}"
%format outT = "\mathsf{out}"
%format Null = "1"
%format (Prod (a) (b)) = a >< b
%format fF = "\fun F "
%format e1 = "e_1 "
%format e2 = "e_2 "
%format Dist = "\fun{Dist}"
%format IO = "\fun{IO}"
%format BTree = "\fun{BTree} "
%format LTree = "\mathsf{LTree}"
%format inNat = "\mathsf{in}"
%format (cataNat (g)) = "\cata{" g "}"
%format Nat0 = "\N_0"
%format Rational = "\Q "
%format toRational = " {to_\Q} "
%format fromRational = " from_{\Q} "
%format muB = "\mu "
%format (frac (n)(m)) = "\frac{" n "}{" m "}"
%format (fac (n)) = "{" n "!}"
%format (underbrace (t) (p)) = "\underbrace{" t "}_{" p "}"
%format matrix = "matrix"
%%format (bin (n) (k)) = "\Big(\vcenter{\xymatrix@R=1pt{" n "\\" k "}}\Big)"
%format `ominus` = "\mathbin{\ominus}"
%format % = "\mathbin{/}"
%format <-> = "{\,\leftrightarrow\,}"
%format <|> = "{\,\updownarrow\,}"
%format `minusNat`= "\mathbin{-}"
%format ==> = "\Rightarrow"
%format .==>. = "\Rightarrow"
%format .<==>. = "\Leftrightarrow"
%format .==. = "\equiv"
%format .<=. = "\leq"
%format .&&&. = "\wedge"
%format cdots = "\cdots "
%format pi = "\pi "
%format (curry (f)) = "\overline{" f "}"

%format sLTree = "_{" LTree "}"
%format outLTree = "out" sLTree
%format inLTree = "in" sLTree
%format (cataLTree (x)) = "\cata{" x "\,}" sLTree
%format (anaLTree (x)) = "\mathopen{[\!(}" x "\mathclose{)\!]}"

%format delta = "\Delta "
%format (ana (f)) = "\ana{" f "}"

%format NList = "\mathsf{NList}"

%format Castel = "\mathsf{Castel}"

%format sl = "_{" [] "}"
%format (anaList (g)) = "\ana{"g"}" sl
%format (cataList (g)) = "\cata{" g "}" sl
%format (hyloList (f) (g)) = "{\llbracket}{" f "," g "{\rrbracket}" sl
%format (baseList (f) (g)) = "B_{[]}(" f "," g ")"

%format sCastel = "_{"Castel"}"
%format (cataC (g)) = "\cata{" g "}" sCastel
%format (anaC (g)) = "\ana{" g "}" sCastel
%format inC = "in" sCastel
%format outC = "out" sCastel
%format fC = "rec" sCastel

%format NL = "_{"NList"}"
%format inL = "in" NL
%format outL = "out" NL
%format recL = "rec" NL
%format (cataL (g)) = "\cata{"g"}" NL
%---------------------------------------------------------------------------

\title{
       	Cálculo de Programas
\\
       	Trabalho Prático
\\
       	MiEI+LCC --- 2020/21
}

\author{
       	\dium
\\
       	Universidade do Minho
}


\date\mydate

\makeindex
\newcommand{\rn}[1]{\textcolor{red}{#1}}
\begin{document}

\maketitle

\begin{center}\large
\begin{tabular}{ll}
\textbf{Grupo} nr. & 17
\\\hline
a91683 & Alef Pinto Keuffer
\\
a93546 & Fernando Maria Bicalho
\\
a88062 & Pedro Paulo Costa Pereira
\\
a91693 & Tiago André Oliveira Leite
\end{tabular}
\end{center}

\section{Preâmbulo}

\CP\ tem como objectivo principal ensinar
a progra\-mação de computadores como uma disciplina científica. Para isso
parte-se de um repertório de \emph{combinadores} que formam uma álgebra da
programação (conjunto de leis universais e seus corolários) e usam-se esses
combinadores para construir programas \emph{composicionalmente}, isto é,
agregando programas já existentes.

Na sequência pedagógica dos planos de estudo dos dois cursos que têm
esta disciplina, opta-se pela aplicação deste método à programação
em \Haskell\ (sem prejuízo da sua aplicação a outras linguagens
funcionais). Assim, o presente trabalho prático coloca os
alunos perante problemas concretos que deverão ser implementados em
\Haskell.  Há ainda um outro objectivo: o de ensinar a documentar
programas, a validá-los e a produzir textos técnico-científicos de
qualidade.

\section{Documentação} Para cumprir de forma integrada os objectivos
enunciados acima vamos recorrer a uma técnica de programa\-ção dita
``\litp{literária}'' \cite{Kn92}, cujo princípio base é o seguinte:
%
\begin{quote}\em Um programa e a sua documentação devem coincidir.
\end{quote}
%
Por outras palavras, o código fonte e a documentação de um
programa deverão estar no mesmo ficheiro.

O ficheiro \texttt{cp2021t.pdf} que está a ler é já um exemplo de
\litp{programação literária}: foi gerado a partir do texto fonte
\texttt{cp2021t.lhs}\footnote{O suffixo `lhs' quer dizer
\emph{\lhaskell{literate Haskell}}.} que encontrará no
\MaterialPedagogico\ desta disciplina descompactando o ficheiro
\texttt{cp2021t.zip} e executando:
\begin{Verbatim}[fontsize=\small]
    $ lhs2TeX cp2021t.lhs > cp2021t.tex
    $ pdflatex cp2021t
\end{Verbatim}
em que \href{https://hackage.haskell.org/package/lhs2tex}{\texttt\LhsToTeX} é
um pre-processador que faz ``pretty printing''
de código Haskell em \Latex\ e que deve desde já instalar executando
\begin{Verbatim}[fontsize=\small]
    $ cabal install lhs2tex --lib
\end{Verbatim}
Por outro lado, o mesmo ficheiro \texttt{cp2021t.lhs} é executável e contém
o ``kit'' básico, escrito em \Haskell, para realizar o trabalho. Basta executar
\begin{Verbatim}[fontsize=\small]
    $ ghci cp2021t.lhs
\end{Verbatim}

%if False
\begin{code}
{-# OPTIONS_GHC -XNPlusKPatterns #-}

--- Extensões adicionadas por Alef para resoluções alternativas.
--- Resouções principais não dependem delas.
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UnicodeSyntax             #-}
---

{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, FlexibleInstances #-}
module Main where
import Cp
import List hiding (fac)
import Nat
import LTree
import Data.List hiding (find)
import Test.QuickCheck hiding ((><),choose,collect)
import qualified Test.QuickCheck as QuickCheck
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad ( zipWithM )
import Control.Applicative hiding ((<|>))
import System.Process
\end{code}
%endif

\noindent Abra o ficheiro \texttt{cp2021t.lhs} no seu editor de texto preferido
e verifique que assim é: todo o texto que se encontra dentro do ambiente
\begin{quote}\small\tt
\verb!\begin{code}!
\\ ... \\
\verb!\end{code}!
\end{quote}
é seleccionado pelo \GHCi\ para ser executado.

\section{Como realizar o trabalho}
Este trabalho teórico-prático deve ser realizado por grupos de 3 (ou 4) alunos.
Os detalhes da avaliação (datas para submissão do relatório e sua defesa
oral) são os que forem publicados na \cp{página da disciplina} na \emph{internet}.

Recomenda-se uma abordagem participativa dos membros do grupo
de trabalho por forma a poderem responder às questões que serão colocadas
na \emph{defesa oral} do relatório.

Em que consiste, então, o \emph{relatório} a que se refere o parágrafo anterior?
É a edição do texto que está a ser lido, preenchendo o anexo \ref{sec:resolucao}
com as respostas. O relatório deverá conter ainda a identificação dos membros
do grupo de trabalho, no local respectivo da folha de rosto.

Para gerar o PDF integral do relatório deve-se ainda correr os comando seguintes,
que actualizam a bibliografia (com \Bibtex) e o índice remissivo (com \Makeindex),
\begin{Verbatim}[fontsize=\small]
    $ bibtex cp2021t.aux
    $ makeindex cp2021t.idx
\end{Verbatim}
e recompilar o texto como acima se indicou. Dever-se-á ainda instalar o utilitário
\QuickCheck,
que ajuda a validar programas em \Haskell\ e a biblioteca \gloss{Gloss} para
geração de gráficos 2D:
\begin{Verbatim}[fontsize=\small]
    $ cabal install QuickCheck gloss --lib
\end{Verbatim}
Para testar uma propriedade \QuickCheck~|prop|, basta invocá-la com o comando:
\begin{verbatim}
    > quickCheck prop
    +++ OK, passed 100 tests.
\end{verbatim}
Pode-se ainda controlar o número de casos de teste e sua complexidade,
como o seguinte exemplo mostra:
\begin{verbatim}
    > quickCheckWith stdArgs { maxSuccess = 200, maxSize = 10 } prop
    +++ OK, passed 200 tests.
\end{verbatim}
Qualquer programador tem, na vida real, de ler e analisar (muito!) código
escrito por outros. No anexo \ref{sec:codigo} disponibiliza-se algum
código \Haskell\ relativo aos problemas que se seguem. Esse anexo deverá
ser consultado e analisado à medida que isso for necessário.

\subsection{Stack}

O \stack{Stack} é um programa útil para criar, gerir e manter projetos em \Haskell.
Um projeto criado com o Stack possui uma estrutura de pastas muito específica:

\begin{itemize}
\item Os módulos auxiliares encontram-se na pasta \emph{src}.
\item O módulos principal encontra-se na pasta \emph{app}.
\item A lista de depêndencias externas encontra-se no ficheiro \emph{package.yaml}.
\end{itemize}

Pode aceder ao \GHCi\ utilizando o comando:
\begin{verbatim}
stack ghci
\end{verbatim}

Garanta que se encontra na pasta mais externa \textbf{do projeto}.
A primeira vez que correr este comando as depêndencias externas serão instaladas automaticamente.

Para gerar o PDF, garanta que se encontra na diretoria \emph{app}.

\Problema

Os \emph{tipos de dados algébricos} estudados ao longo desta disciplina oferecem
uma grande capacidade expressiva ao programador. Graças à sua flexibilidade,
torna-se trivial implementar \DSL s
e até mesmo \href{http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf}{linguagens de programação}.

Paralelamente, um tópico bastante estudado no âmbito de \DL\
é a derivação automática de expressões matemáticas, por exemplo, de derivadas.
Duas técnicas que podem ser utilizadas para o cálculo de derivadas são:

\begin{itemize}
\item \emph{Symbolic differentiation}
\item \emph{Automatic differentiation}
\end{itemize}

\emph{Symbolic differentiation} consiste na aplicação sucessiva de transformações
(leia-se: funções) que sejam congruentes com as regras de derivação. O resultado
final será a expressão da derivada.

O leitor atento poderá notar um problema desta técnica: a expressão
inicial pode crescer de forma descontrolada, levando a um cálculo pouco eficiente.
\emph{Automatic differentiation} tenta resolver este problema,
calculando \textbf{o valor} da derivada da expressão em todos os passos.
Para tal, é necessário calcular o valor da expressão \textbf{e} o valor da sua derivada.

Vamos de seguida definir uma linguagem de expressões matemáticas simples e
implementar as duas técnicas de derivação automática.
Para isso, seja dado o seguinte tipo de dados,

\begin{code}
data ExpAr a = X
           | N a
           | Bin BinOp (ExpAr a) (ExpAr a)
           | Un UnOp (ExpAr a)
           deriving (Eq, Show)
\end{code}

\noindent
onde |BinOp| e |UnOp| representam operações binárias e unárias, respectivamente:

\begin{code}
data BinOp = Sum
           | Product
           deriving (Eq, Show)

data UnOp = Negate
          | E
          deriving (Eq, Show)
\end{code}

\noindent
O construtor |E| simboliza o exponencial de base $e$.

Assim, cada expressão pode ser uma variável, um número, uma operação binária
aplicada às devidas expressões, ou uma operação unária aplicada a uma expressão.
Por exemplo,
\begin{spec}
Bin Sum X (N 10)
\end{spec}
designa |x+10| na notação matemática habitual.

\begin{enumerate}
\item A definição das funções |inExpAr| e |baseExpAr| para este tipo é a seguinte:
\begin{code}
inExpAr = either (const X) num_ops where
  num_ops = either N ops
  ops     = either bin (uncurry Un)
  bin(op, (a, b)) = Bin op a b

baseExpAr f g h j k l z = f -|- (g -|- (h >< (j >< k) -|- l >< z))
\end{code}

  Defina as funções |outExpAr| e |recExpAr|,
  e teste as propriedades que se seguem.
  \begin{propriedade}
    |inExpAr| e |outExpAr| são testemunhas de um isomorfismo,
    isto é,
    |inExpAr . outExpAr = id| e |outExpAr . idExpAr = id|:
\begin{code}
prop_in_out_idExpAr :: (Eq a) => ExpAr a -> Bool
prop_in_out_idExpAr = inExpAr . outExpAr .==. id

prop_out_in_idExpAr :: (Eq a) => OutExpAr a -> Bool
prop_out_in_idExpAr = outExpAr . inExpAr .==. id
\end{code}
    \end{propriedade}

  \item Dada uma expressão aritmética e um escalar para substituir o |X|,
	a função

\begin{quote}
      |eval_exp :: Floating a => a -> (ExpAr a) -> a|
\end{quote}

\noindent calcula o resultado da expressão. Na página \pageref{pg:P1}
    esta função está expressa como um catamorfismo. Defina o respectivo gene
    e, de seguida, teste as propriedades:
    \begin{propriedade}
       A função |eval_exp| respeita os elementos neutros das operações.
\begin{code}
prop_sum_idr :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_sum_idr a exp = eval_exp a exp .=?=. sum_idr where
  sum_idr = eval_exp a (Bin Sum exp (N 0))

prop_sum_idl :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_sum_idl a exp = eval_exp a exp .=?=. sum_idl where
  sum_idl = eval_exp a (Bin Sum (N 0) exp)

prop_product_idr :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_product_idr a exp = eval_exp a exp .=?=. prod_idr where
  prod_idr = eval_exp a (Bin Product exp (N 1))

prop_product_idl :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_product_idl a exp = eval_exp a exp .=?=. prod_idl where
  prod_idl = eval_exp a (Bin Product (N 1) exp)

prop_e_id :: (Floating a, Real a) => a -> Bool
prop_e_id a = eval_exp a (Un E (N 1)) == expd 1

prop_negate_id :: (Floating a, Real a) => a -> Bool
prop_negate_id a = eval_exp a (Un Negate (N 0)) == 0
\end{code}
    \end{propriedade}
    \begin{propriedade}
      Negar duas vezes uma expressão tem o mesmo valor que não fazer nada.
\begin{code}
prop_double_negate :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_double_negate a exp = eval_exp a exp .=?=. eval_exp a (Un Negate (Un Negate exp))
\end{code}
    \end{propriedade}

  \item É possível otimizar o cálculo do valor de uma expressão aritmética tirando proveito
  dos elementos absorventes de cada operação. Implemente os genes da função
\begin{spec}
      optmize_eval :: (Floating a, Eq a) => a -> (ExpAr a) -> a
\end{spec}
  que se encontra na página \pageref{pg:P1} expressa como um hilomorfismo\footnote{Qual é a vantagem de implementar a função |optimize_eval| utilizando um hilomorfismo em vez de utilizar um catamorfismo com um gene "inteligente"?}
  e teste as propriedades:

    \begin{propriedade}
      A função |optimize_eval| respeita a semântica da função |eval|.
\begin{code}
prop_optimize_respects_semantics :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_optimize_respects_semantics a exp = eval_exp a exp .=?=. optmize_eval a exp
\end{code}
    \end{propriedade}


\item Para calcular a derivada de uma expressão, é necessário aplicar transformações
à expressão original que respeitem as regras das derivadas:\footnote{%
	Apesar da adição e multiplicação gozarem da propriedade comutativa,
	há que ter em atenção a ordem das operações por causa dos testes.}

\begin{itemize}
  \item Regra da soma:
\begin{eqnarray*}
	\frac{d}{dx}(f(x)+g(x))=\frac{d}{dx}(f(x))+\frac{d}{dx}(g(x))
\end{eqnarray*}
  \item Regra do produto:
\begin{eqnarray*}
	\frac{d}{dx}(f(x)g(x))=f(x)\cdot \frac{d}{dx}(g(x))+\frac{d}{dx}(f(x))\cdot g(x)
\end{eqnarray*}
\end{itemize}

  Defina o gene do catamorfismo que ocorre na função
    \begin{quote}
      |sd :: Floating a => ExpAr a -> ExpAr a|
    \end{quote}
  que, dada uma expressão aritmética, calcula a sua derivada.
  Testes a fazer, de seguida:
    \begin{propriedade}
       A função |sd| respeita as regras de derivação.
\begin{code}
prop_const_rule :: (Real a, Floating a) => a -> Bool
prop_const_rule a = sd (N a) == N 0

prop_var_rule :: Bool
prop_var_rule = sd X == N 1

prop_sum_rule :: (Real a, Floating a) => ExpAr a -> ExpAr a -> Bool
prop_sum_rule exp1 exp2 = sd (Bin Sum exp1 exp2) == sum_rule where
  sum_rule = Bin Sum (sd exp1) (sd exp2)

prop_product_rule :: (Real a, Floating a) => ExpAr a -> ExpAr a -> Bool
prop_product_rule exp1 exp2 = sd (Bin Product exp1 exp2) == prod_rule where
  prod_rule =Bin Sum (Bin Product exp1 (sd exp2)) (Bin Product (sd exp1) exp2)

prop_e_rule :: (Real a, Floating a) => ExpAr a -> Bool
prop_e_rule exp = sd (Un E exp) == Bin Product (Un E exp) (sd exp)

prop_negate_rule :: (Real a, Floating a) => ExpAr a -> Bool
prop_negate_rule exp = sd (Un Negate exp) == Un Negate (sd exp)
\end{code}
    \end{propriedade}

\item Como foi visto, \emph{Symbolic differentiation} não é a técnica
mais eficaz para o cálculo do valor da derivada de uma expressão.
\emph{Automatic differentiation} resolve este problema cálculando o valor
da derivada em vez de manipular a expressão original.

  Defina o gene do catamorfismo que ocorre na função
    \begin{spec}
    ad :: Floating a => a -> ExpAr a -> a
    \end{spec}
  que, dada uma expressão aritmética e um ponto,
  calcula o valor da sua derivada nesse ponto,
  sem transformar manipular a expressão original.
  Testes a fazer, de seguida:

    \begin{propriedade}
       Calcular o valor da derivada num ponto |r| via |ad| é equivalente a calcular a derivada da expressão e avalia-la no ponto |r|.
\begin{code}
prop_congruent :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_congruent a exp = ad a exp .=?=. eval_exp a (sd exp)
\end{code}
    \end{propriedade}
\end{enumerate}

\Problema

Nesta disciplina estudou-se como fazer \pd{programação dinâmica} por cálculo,
recorrendo à lei de recursividade mútua.\footnote{Lei (\ref{eq:fokkinga})
em \cite{Ol18}, página \pageref{eq:fokkinga}.}

Para o caso de funções sobre os números naturais (|Nat0|, com functor |fF
X = 1 + X|) é fácil derivar-se da lei que foi estudada uma
	\emph{regra de algibeira}
	\label{pg:regra}
que se pode ensinar a programadores que não tenham estudado
\cp{Cálculo de Programas}. Apresenta-se de seguida essa regra, tomando como exemplo o
cálculo do ciclo-\textsf{for} que implementa a função de Fibonacci, recordar
o sistema
\begin{spec}
fib 0 = 1
fib(n+1) = f n

f 0 = 1
f (n+1) = fib n + f n
\end{spec}
Obter-se-á de imediato
\begin{code}
fib' = p1 . for loop init where
   loop(fib,f)=(f,fib+f)
   init = (1,1)
\end{code}
usando as regras seguintes:
\begin{itemize}
\item	O corpo do ciclo |loop| terá tantos argumentos quanto o número de funções mutuamente recursivas.
\item	Para as variáveis escolhem-se os próprios nomes das funções, pela ordem
que se achar conveniente.\footnote{Podem obviamente usar-se outros símbolos, mas numa primeira leitura
dá jeito usarem-se tais nomes.}
\item	Para os resultados vão-se buscar as expressões respectivas, retirando a variável |n|.
\item	Em |init| coleccionam-se os resultados dos casos de base das funções, pela mesma ordem.
\end{itemize}
Mais um exemplo, envolvendo polinómios do segundo grau $ax^2 + b x + c$ em |Nat0|.
Seguindo o método estudado nas aulas\footnote{Secção 3.17 de \cite{Ol18} e tópico
\href{https://www4.di.uminho.pt/~jno/media/cp/}{Recursividade mútua} nos vídeos das aulas teóricas.},
de $f\ x = a x^2 + b x + c$ derivam-se duas funções mutuamente recursivas:
\begin{spec}
f 0 = c
f (n+1) = f n + k n

k 0 = a + b
k(n+1) = k n + 2 a
\end{spec}
Seguindo a regra acima, calcula-se de imediato a seguinte implementação, em Haskell:
\begin{code}
f' a b c = p1 . for loop init where
  loop(f,k) = (f+k,k+2*a)
  init = (c,a+b)
\end{code}
O que se pede então, nesta pergunta?
Dada a fórmula que dá o |n|-ésimo \catalan{número de Catalan},
\begin{eqnarray}
	C_n = \frac{(2n)!}{(n+1)! (n!) }
	\label{eq:cat}
\end{eqnarray}
derivar uma implementação de $C_n$ que não calcule factoriais nenhuns.
Isto é, derivar um ciclo-\textsf{for}
\begin{spec}
cat = cdots . for loop init where cdots
\end{spec}
que implemente esta função.

\begin{propriedade}
A função proposta coincidem com a definição dada:
\begin{code}
prop_cat = (>=0) .==>. (catdef  .==. cat)
\end{code}
\end{propriedade}
%
\textbf{Sugestão}: Começar por estudar muito bem o processo de cálculo dado
no anexo \ref{sec:recmul} para o problema (semelhante) da função exponencial.


\Problema

As \bezier{curvas de Bézier}, designação dada em honra ao engenheiro
\href{https://en.wikipedia.org/wiki/Pierre_B%C3%A9zier}{Pierre Bézier},
são curvas ubíquas na área de computação gráfica, animação e modelação.
Uma curva de Bézier é uma curva paramétrica, definida por um conjunto
$\{P_0,...,P_N\}$ de pontos de controlo, onde $N$ é a ordem da curva.

\begin{figure}[h!]
  \centering
  \includegraphics[width=0.8\textwidth]{cp2021t_media/Bezier_curves.png}
  \caption{Exemplos de curvas de Bézier retirados da \bezier{ Wikipedia}.}
\end{figure}

O algoritmo de \emph{De Casteljau} é um método recursivo capaz de calcular
curvas de Bézier num ponto. Apesar de ser mais lento do que outras abordagens,
este algoritmo é numericamente mais estável, trocando velocidade por correção.

De forma sucinta, o valor de uma curva de Bézier de um só ponto $\{P_0\}$
(ordem $0$) é o próprio ponto $P_0$. O valor de uma curva de Bézier de ordem
$N$ é calculado através da interpolação linear da curva de Bézier dos primeiros
$N-1$ pontos e da curva de Bézier dos últimos $N-1$ pontos.

A interpolação linear entre 2 números, no intervalo $[0, 1]$, é dada pela
seguinte função:

\begin{code}
linear1d :: Rational -> Rational -> OverTime Rational
linear1d a b = formula a b where
  formula :: Rational -> Rational -> Float -> Rational
  formula x y t = ((1.0 :: Rational) - (toRational t)) * x + (toRational t) * y
\end{code}
%
A interpolação linear entre 2 pontos de dimensão $N$ é calculada através
da interpolação linear de cada dimensão.

O tipo de dados |NPoint| representa um ponto com $N$ dimensões.
\begin{code}
type NPoint = [Rational]
\end{code}
Por exemplo, um ponto de 2 dimensões e um ponto de 3 dimensões podem ser
representados, respetivamente, por:
\begin{spec}
p2d = [1.2, 3.4]
p3d = [0.2, 10.3, 2.4]
\end{spec}
%
O tipo de dados |OverTime a| representa um termo do tipo |a| num dado instante
(dado por um |Float|).
\begin{code}
type OverTime a = Float -> a
\end{code}
%
O anexo \ref{sec:codigo} tem definida a função
    \begin{spec}
    calcLine :: NPoint -> (NPoint -> OverTime NPoint)
    \end{spec}
que calcula a interpolação linear entre 2 pontos, e a função
    \begin{spec}
    deCasteljau :: [NPoint] -> OverTime NPoint
    \end{spec}
que implementa o algoritmo respectivo.

\begin{enumerate}

\item Implemente |calcLine| como um catamorfismo de listas,
testando a sua definição com a propriedade:
    \begin{propriedade} Definição alternativa.
\begin{code}
prop_calcLine_def :: NPoint -> NPoint -> Float -> Bool
prop_calcLine_def p q d = calcLine p q d ==  zipWithM linear1d p q d
\end{code}
    \end{propriedade}

\item Implemente a função |deCasteljau| como um hilomorfismo, testando agora a propriedade:
    \begin{propriedade}
      Curvas de Bézier são simétricas.
\begin{code}
prop_bezier_sym :: [[Rational]] -> Gen Bool
prop_bezier_sym l = all (< delta) . calc_difs . bezs <$> elements ps  where
  calc_difs = (\(x, y) -> zipWith (\w v -> if w >= v then w - v else v - w) x y)
  bezs t    = (deCasteljau l t, deCasteljau (reverse l) (fromRational (1 - (toRational t))))
  delta = 1e-2
\end{code}
    \end{propriedade}

  \item Corra a função |runBezier| e aprecie o seu trabalho\footnote{%
        A representação em Gloss é uma adaptação de um
        \href{https://github.com/hrldcpr/Bezier.hs}{projeto}
        de Harold Cooper.} clicando na janela que é aberta (que contém, a verde, um ponto
        inicila) com o botão esquerdo do rato para adicionar mais pontos.
        A tecla |Delete| apaga o ponto mais recente.

\end{enumerate}

\Problema

Seja dada a fórmula que calcula a média de uma lista não vazia $x$,
\begin{equation}
avg\ x = \frac 1 k\sum_{i=1}^{k} x_i
\end{equation}
onde $k=length\ x$. Isto é, para sabermos a média de uma lista precisamos de dois catamorfismos: o que faz o somatório e o que calcula o comprimento a lista.
Contudo, é facil de ver que
\begin{quote}
	$avg\ [a]=a$
\\
	$avg (a:x) = \frac 1 {k+1}(a+\sum_{i=1}^{k} x_i) = \frac{a+k(avg\ x)}{k+1}$ para $k=length\ x$
\end{quote}
Logo $avg$ está em recursividade mútua com $length$ e o par de funções pode ser expresso por um único catamorfismo, significando que a lista apenas é percorrida uma vez.

\begin{enumerate}

\item	Recorra à lei de recursividade mútua para derivar a função
|avg_aux = cata (either b q)| tal que
|avg_aux = split avg length| em listas não vazias.

\item	Generalize o raciocínio anterior para o cálculo da média de todos os elementos de uma \LTree\ recorrendo a uma única travessia da árvore (i.e.\ catamorfismo).

\end{enumerate}
Verifique as suas funções testando a propriedade seguinte:
\begin{propriedade}
A média de uma lista não vazia e de uma \LTree\ com os mesmos elementos coincide,
a menos de um erro de 0.1 milésimas:
\begin{code}
prop_avg = nonempty .==>. diff .<=. const 0.000001 where
   diff l = avg l - (avgLTree . genLTree) l
   genLTree = anaLTree lsplit
   nonempty = (>[])
\end{code}
\end{propriedade}

\Problema	(\textbf{NB}: Esta questão é \textbf{opcional} e funciona como \textbf{valorização} apenas para os alunos que desejarem fazê-la.)

\vskip 1em \noindent
Existem muitas linguagens funcionais para além do \Haskell, que é a linguagem usada neste trabalho prático. Uma delas é o \Fsharp\ da Microsoft. Na directoria \verb!fsharp! encontram-se os módulos \Cp, \Nat\ e \LTree\ codificados em \Fsharp. O que se pede é a biblioteca \BTree\ escrita na mesma linguagem.

Modo de execução: o código que tiverem produzido nesta pergunta deve ser colocado entre o \verb!\begin{verbatim}! e o \verb!\end{verbatim}! da correspondente parte do anexo \ref{sec:resolucao}. Para além disso, os grupos podem demonstrar o código na oral.

\newpage

\part*{Anexos}

\appendix

\section{Como exprimir cálculos e diagramas em LaTeX/lhs2tex}
Como primeiro exemplo, estudar o texto fonte deste trabalho para obter o
efeito:\footnote{Exemplos tirados de \cite{Ol18}.}
\begin{eqnarray*}
\start
	|id = split f g|
%
\just\equiv{ universal property }
%
        |lcbr(
		p1 . id = f
	)(
		p2 . id = g
	)|
%
\just\equiv{ identity }
%
        |lcbr(
		p1 = f
	)(
		p2 = g
	)|
\qed
\end{eqnarray*}

Os diagramas podem ser produzidos recorrendo à \emph{package} \LaTeX\
\href{https://ctan.org/pkg/xymatrix}{xymatrix}, por exemplo:
\begin{eqnarray*}
\xymatrix@@C=2cm{
    |Nat0|
           \ar[d]_-{|cataNat g|}
&
    |1 + Nat0|
           \ar[d]^{|id + (cataNat g)|}
           \ar[l]_-{|inNat|}
\\
     |B|
&
     |1 + B|
           \ar[l]^-{|g|}
}
\end{eqnarray*}



\section{Programação dinâmica por recursividade múltipla}\label{sec:recmul}
Neste anexo dão-se os detalhes da resolução do Exercício \ref{ex:exp} dos apontamentos da
disciplina\footnote{Cf.\ \cite{Ol18}, página \pageref{ex:exp}.},
onde se pretende implementar um ciclo que implemente
o cálculo da aproximação até |i=n| da função exponencial $exp\ x = e^x$,
via série de Taylor:
\begin{eqnarray}
	exp\ x
& = &
	\sum_{i=0}^{\infty} \frac {x^i} {i!}
\end{eqnarray}
Seja $e\ x\ n = \sum_{i=0}^{n} \frac {x^i} {i!}$ a função que dá essa aproximação.
É fácil de ver que |e x 0 = 1| e que $|e x (n+1)| = |e x n| + \frac {x^{n+1}} {(n+1)!}$.
Se definirmos $|h x n| = \frac {x^{n+1}} {(n+1)!}$ teremos |e x| e |h x| em recursividade
mútua. Se repetirmos o processo para |h x n| etc obteremos no total três funções nessa mesma
situação:
\begin{spec}
e x 0 = 1
e x (n+1) = h x n + e x n

h x 0 = x
h x (n+1) = x/(s n) * h x n

s 0 = 2
s (n+1) = 1 + s n
\end{spec}
Segundo a \emph{regra de algibeira} descrita na página \ref{pg:regra} deste enunciado,
ter-se-á, de imediato:
\begin{code}
e' x = prj . for loop init where
     init = (1,x,2)
     loop(e,h,s)=(h+e,x/s*h,1+s)
     prj(e,h,s) = e
\end{code}

\section{Código fornecido}\label{sec:codigo}

\subsection*{Problema 1}

\begin{code}
expd :: Floating a => a -> a
expd = Prelude.exp

type OutExpAr a = Either () (Either a (Either (BinOp, (ExpAr a, ExpAr a)) (UnOp, ExpAr a)))
\end{code}

\subsection*{Problema 2}
Definição da série de Catalan usando factoriais (\ref{eq:cat}):
\begin{code}
catdef n = div (fac((2*n))) ((fac((n+1))*(fac n)))
\end{code}
Oráculo para inspecção dos primeiros 26 números de Catalan\footnote{Fonte:
\catalan{Wikipedia}.}:
\begin{code}
oracle = [
    1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845,
    35357670, 129644790, 477638700, 1767263190, 6564120420, 24466267020,
    91482563640, 343059613650, 1289904147324, 4861946401452
    ]
\end{code}

\subsection*{Problema 3}
Algoritmo:
\begin{spec}
deCasteljau :: [NPoint] -> OverTime NPoint
deCasteljau [] = nil
deCasteljau [p] = const p
deCasteljau l = \pt -> (calcLine (p pt) (q pt)) pt where
  p = deCasteljau (init l)
  q = deCasteljau (tail l)
\end{spec}
Função auxiliar:
\begin{spec}
calcLine :: NPoint -> (NPoint -> OverTime NPoint)
calcLine [] = const nil
calcLine(p:x) = curry g p (calcLine x) where
   g :: (Rational, NPoint -> OverTime NPoint) -> (NPoint -> OverTime NPoint)
   g (d,f) l = case l of
       []     -> nil
       (x:xs) -> \z -> concat $ (sequenceA [singl . linear1d d x, f xs]) z
\end{spec}
2D:
\begin{code}
bezier2d :: [NPoint] -> OverTime (Float, Float)
bezier2d [] = const (0, 0)
bezier2d l = \z -> (fromRational >< fromRational) . (\[x, y] -> (x, y)) $ ((deCasteljau l) z)
\end{code}
Modelo:
\begin{code}
data World = World { points :: [NPoint]
                   , time :: Float
                   }
initW :: World
initW = World [] 0

tick :: Float -> World -> World
tick dt world = world { time=(time world) + dt }

actions :: Event -> World -> World
actions (EventKey (MouseButton LeftButton) Down _ p) world =
  world {points=(points world) ++ [(\(x, y) -> map toRational [x, y]) p]}
actions (EventKey (SpecialKey KeyDelete) Down _ _) world =
    world {points = cond (== []) id init (points world)}
actions _ world = world

scaleTime :: World -> Float
scaleTime w = (1 + cos (time w)) / 2

bezier2dAtTime :: World -> (Float, Float)
bezier2dAtTime w = (bezier2dAt w) (scaleTime w)

bezier2dAt :: World -> OverTime (Float, Float)
bezier2dAt w = bezier2d (points w)

thicCirc :: Picture
thicCirc = ThickCircle 4 10

ps :: [Float]
ps = map fromRational ps' where
  ps' :: [Rational]
  ps' = [0, 0.01..1] -- interval
\end{code}
Gloss:
\begin{code}
picture :: World -> Picture
picture world = Pictures
  [ animateBezier (scaleTime world) (points world)
  , Color white . Line . map (bezier2dAt world) $ ps
  , Color blue . Pictures $ [Translate (fromRational x) (fromRational y) thicCirc | [x, y] <- points world]
  , Color green $ Translate cx cy thicCirc
  ] where
  (cx, cy) = bezier2dAtTime world
\end{code}
Animação:
\begin{code}
animateBezier :: Float -> [NPoint] -> Picture
animateBezier _ [] = Blank
animateBezier _ [_] = Blank
animateBezier t l = Pictures
  [ animateBezier t (init l)
  , animateBezier t (tail l)
  , Color red . Line $ [a, b]
  , Color orange $ Translate ax ay thicCirc
  , Color orange $ Translate bx by thicCirc
  ] where
  a@(ax, ay) = bezier2d (init l) t
  b@(bx, by) = bezier2d (tail l) t
\end{code}
Propriedades e \emph{main}:
\begin{code}
runBezier :: IO ()
runBezier = play (InWindow "Bézier" (600, 600) (0,  0))
  black 50 initW picture actions tick

runBezierSym :: IO ()
runBezierSym = quickCheckWith (stdArgs {maxSize = 20, maxSuccess = 200} ) prop_bezier_sym
\end{code}

Compilação e execução dentro do interpretador:\footnote{Pode ser útil em testes
envolvendo \gloss{Gloss}. Nesse caso, o teste em causa deve fazer parte de uma função
|main|.}
\begin{code}
main = runBezier

run = do { system "ghc cp2021t" ; system "./cp2021t" }
\end{code}

\subsection*{QuickCheck}
Código para geração de testes:
\begin{code}
instance Arbitrary UnOp where
  arbitrary = elements [Negate, E]

instance Arbitrary BinOp where
  arbitrary = elements [Sum, Product]

instance (Arbitrary a) => Arbitrary (ExpAr a) where
  arbitrary = do
    binop <- arbitrary
    unop  <- arbitrary
    exp1  <- arbitrary
    exp2  <- arbitrary
    a     <- arbitrary

    frequency . map (id >< pure) $ [(20, X), (15, N a), (35, Bin binop exp1 exp2), (30, Un unop exp1)]


infixr 5 .=?=.
(.=?=.) :: Real a => a -> a -> Bool
(.=?=.) x y = (toRational x) == (toRational y)


\end{code}

\subsection*{Outras funções auxiliares}
%----------------- Outras definições auxiliares -------------------------------------------%
Lógicas:
\begin{code}
infixr 0 .==>.
(.==>.) :: (Testable prop) => (a -> Bool) -> (a -> prop) -> a -> Property
p .==>. f = \a -> p a ==> f a

infixr 0 .<==>.
(.<==>.) :: (a -> Bool) -> (a -> Bool) -> a -> Property
p .<==>. f = \a -> (p a ==> property (f a)) .&&. (f a ==> property (p a))

infixr 4 .==.
(.==.) :: Eq b => (a -> b) -> (a -> b) -> (a -> Bool)
f .==. g = \a -> f a == g a

infixr 4 .<=.
(.<=.) :: Ord b => (a -> b) -> (a -> b) -> (a -> Bool)
f .<=. g = \a -> f a <= g a

infixr 4 .&&&.
(.&&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&&&. g = \a -> ((f a) && (g a))
\end{code}

%----------------- Soluções dos alunos -----------------------------------------%

\section{Soluções dos alunos}\label{sec:resolucao}
Os alunos devem colocar neste anexo as suas soluções para os exercícios
propostos, de acordo com o "layout" que se fornece. Não podem ser
alterados os nomes ou tipos das funções dadas, mas pode ser adicionado
texto, disgramas e/ou outras funções auxiliares que sejam necessárias.

Valoriza-se a escrita de \emph{pouco} código que corresponda a soluções
simples e elegantes.

\subsection*{Problema 1} \label{pg:P1}
São dadas:
\begin{code}
cataExpAr g = g . recExpAr (cataExpAr g) . outExpAr
anaExpAr g = inExpAr . recExpAr (anaExpAr g) . g
hyloExpAr h g = cataExpAr h . anaExpAr g

eval_exp :: Floating a => a -> (ExpAr a) -> a
eval_exp a = cataExpAr (g_eval_exp a)

optmize_eval :: (Floating a, Eq a) => a -> (ExpAr a) -> a
optmize_eval a = hyloExpAr (gopt a) clean


sd :: Floating a => ExpAr a -> ExpAr a
sd = p2 . cataExpAr sd_gen

ad :: Floating a => a -> ExpAr a -> a
ad v = p2 . cataExpAr (ad_gen v)
\end{code}

\\

\begin{eqnarray*}
\start
  |outExpAr . inExpAr = id|
%
\just\equiv{Definição de inExpAr; Fusão-+; Cancelamento-+}
%
|lcbr(
  outExpAr . const X = id . i1
)(
  lcbr(
    outExpAr . N  = id . i2 . i1
  )(
    lcbr(
      outExpAr . bin  = id . i2 . i2 . i1
    )(
      outExpAr . uncurry Un = id . i2 . i2 . i2
    )
  )
)|
%
\just\equiv{ Igualdade extensional, Natural-id }
%
|lcbr(
  (outExpAr . const X) () = i1 ()
)(
  lcbr(
    (outExpAr . N) a  =  (i2 . i1) a
  )(
    lcbr(
      (outExpAr . bin) (op,(l,r)) = (i2 . i2 . i1) (op,(l,r))
    )(
      (outExpAr . uncurry Un) (op,a) = (i2 . i2 . i2) (op,a)
    )
  )
)|
%
\just\equiv{ Def-comp, Def-const, Def-|N|, Def-|bin|, Def-Uncurry, Def-|Un| }
%
|lcbr(
  outExpAr X = i1 ()
)(
  lcbr(
    outExpAr (N a) = i2 $ i1 a
  )(
    lcbr(
      outExpAr (Bin op l r)  = i2 $ i2 $ i1 (op,(l,r))
    )(
      outExpAr (Un op a) = i2 $ i2 $ i2 (op,a)
    )
  )
)|
\qed
\end{eqnarray*}


\begin{eqnarray*}
\xymatrix@@C=2cm{
    & ExpAr \ A\ar@@/^3pc/[r]^{outExpAr} \ar@@{{}{ }{}}@@/^1.8pc/[r]_{\cong}
    & 1 + A + BinOp \times (ExpAr A)^2 + UnOp \times (ExpAr A)\ar@@/^3pc/[l]^{inExpAr}
}
\end{eqnarray*}


\begin{eqnarray*}
\start
|recExprAr f = id + (id + (id >< (f >< f) + id >< f ))|
%
\just\equiv{ Def-baseExpAr }
%
|recExprAr f = baseExpAr id id id f f id f|
\qed
\end{eqnarray*}

\begin{center}
\xymatrix@@C=2cm@@R=3cm{
    & ExpAr \ A\ar@@/^2pc/[r]^{outExpAr}\ar[d]_{|cata (g_eval a)|} & 1 + A + BinOp \times (ExpAr A)^2 + UnOp \times (ExpAr A)\ar[d]^{recExpAr \ |cata (g_eval a)|}\ar@@/^2pc/[l]^{inExpAr}  \\
    & |Nat0|  & 1 + |Nat0| + BinOp \times |Nat0|^2 + UnOp \times |Nat0|\ar[l]_(0.6){g\_eval \ a}
}
\end{center}

\begin{center}
\xymatrix@@C=2cm@@R=3cm{
    & ExpAr \ A\ar@@/^2pc/[r]^{outExpAr} & 1 + A + BinOp \times (ExpAr A)^2 + UnOp \times (ExpAr A)\ar@@/^2pc/[l]^{inExpAr}  \\
    & ExpAr \ A\ar[r]^(0.25){clean} \ar[u]^{|ana (clean)|} & 1 + A + BinOp \times (ExpAr A)^2 + UnOp \times (ExpAr A)\ar[u]_{recExpAr \ |ana(clean)|}
}
\end{center}

\begin{center}
\xymatrix@@C=2cm@@R=3cm{
    & ExpAr \ A\ar[r]^(0.25){clean} \ar[d]_{|ana (clean)|} & 1 + A + BinOp \times (ExpAr A)^2 + UnOp \times (ExpAr A)\ar[d]^{recExpAr \ |ana(clean)|}\\
    & ExpAr \ A\ar@@/^2pc/[r]^{outExpAr}\ar[d]_{|cata (gopt a)|} & 1 + A + BinOp \times (ExpAr A)^2 + UnOp \times (ExpAr A)\ar[d]^{recExpAr \ |cata (gopt a)|}\ar@@/^2pc/[l]^{inExpAr}  \\
    & |Nat0|  & 1 + |Nat0| + BinOp \times |Nat0|^2 + UnOp \times |Nat0|\ar[l]_(0.6){gopt \ a}
}
\end{center}

\begin{center}
\xymatrix@@C=2cm@@R=3cm{
    & ExpAr \ A\ar@@/^2pc/[r]^{outExpAr}\ar[d]_{|cata (sd_gen)|} & 1 + A + BinOp \times (ExpAr A)^2 + UnOp \times (ExpAr A)\ar[d]^{|recExpAr (cata (sd_gen))|}\ar@@/^2pc/[l]^{|inExpAr|}  \\
    & (ExpAr A)^2   & 1 + A + BinOp \times ((ExpAr A)^2 \times (ExpAr A)^2) + UnOp \times (ExpAr A)^2\ar[l]_(0.73){sd\_gen}
}
\end{center}

\begin{center}
\xymatrix@@C=2cm@@R=3cm{
    & ExpAr \ A\ar@@/^2pc/[r]^{|outExpAr|}\ar[d]_{|cata (ad_gen a)|} & 1 + A + BinOp \times (ExpAr A)^2 + UnOp \times (ExpAr A)\ar[d]^{|recExpAr (cata (ad_gen a))|}\ar@@/^2pc/[l]^{|inExpAr|}  \\
    & |Nat0|^2   & 1 + A + BinOp \times (|Nat0|^2 \times |Nat0|^2) + UnOp \times (|Nat0|^2)\ar[l]_(0.73){ad\_gen \ a }
}
\end{center}



Definir:

\begin{code}
outExpAr X = i1 ()
outExpAr (N a) = i2 $ i1 a
outExpAr (Bin op l r) = i2 $ i2 $ i1 (op,(l,r))
outExpAr (Un op a) = i2 $ i2 $ i2 (op,a)


recExpAr f  = baseExpAr id id id f f id f


g_eval_exp x (Left ()) = x
g_eval_exp x (Right (Left a)) = a
g_eval_exp x (Right (Right (Left (Sum,(e,d))))) = e+d
g_eval_exp x (Right (Right (Left (Product,(e,d))))) = e*d
g_eval_exp x (Right (Right (Right (Negate,a)))) = negate a
g_eval_exp x (Right (Right (Right (E,a)))) = expd a


clean a = (outExpAr . h)  a where
    h (Bin Product (N 0) r) = N 0
    h (Bin Product r (N 0) ) = N 0
    h (Un E (N 0)) = N 1
    h (Un Negate (N 0)) = N 0
    h x = x


gopt a = g_eval_exp a
\end{code}

\begin{code}
sd_gen :: Floating a =>
    Either () (Either a (Either (BinOp, ((ExpAr a, ExpAr a),
    (ExpAr a, ExpAr a))) (UnOp, (ExpAr a, ExpAr a)))) -> (ExpAr a, ExpAr a)
sd_gen (Left ()) = (X, N 1)
sd_gen (Right (Left a)) = (N a, N 0)
sd_gen (Right (Right (Left (Sum,((e1,d1),(e2,d2)))))) = (Bin Sum e1 e2, Bin Sum d1 d2)
sd_gen (Right (Right (Left (Product,((e1,d1),(e2,d2))))))
    = (Bin Product e1 e2, Bin Sum (Bin Product e1 d2) (Bin Product d1 e2 ))
sd_gen (Right (Right (Right (Negate,(e,d))))) = (Un Negate e , Un Negate d)
sd_gen (Right (Right (Right (E,(e,d))))) = (Un E e , Bin Product (Un E e) d)
\end{code}

\begin{code}
ad_gen x (Left ()) = (x, 1)
ad_gen x (Right (Left a)) = (a, 0)
ad_gen x (Right (Right (Left (Sum,((e1,d1),(e2,d2)))))) = (e1+e2, d1+d2)
ad_gen x (Right (Right (Left (Product,((e1,d1),(e2,d2)))))) = (e1*e2, e1*d2 + e2*d1)
ad_gen x (Right (Right (Right (Negate,(e,d))))) = (negate e, negate d)
ad_gen x (Right (Right (Right (E,(e,d))))) = (expd e, d * (expd e))
\end{code}

\subsection*{Problema 2}
Definir
\begin{code}
loop = g where g(c,a,b) = (div (c*a) b, a+4, b+1)
inic = (1,2,2)
prj = p where p(c,_,_) = c
\end{code}
por forma a que
\begin{code}
cat = prj . (for loop inic)
\end{code}
seja a função pretendida.
\textbf{NB}: usar divisão inteira.
Apresentar de seguida a justificação da solução encontrada.
\begin{eqnarray}
    C_n = \frac{(2n)!}{(n+1)! (n!) }
    \label{eq:cat}
\end{eqnarray}

\begin{math}
\begin{array}{cccc}
 & C_{0} & = & 1\\
 & C_{n+1} & = & \frac{C_{n}a_{n}}{b_{n}}\\
\\
 & a_{n} & = & 4n+2\\
 & b_{n} & = & n+2\\
\\
 & a_{0} & = & 2\\
 & a_{n+1} & = & a_{n}+4\\
\\
 & b_{0} & = & 2\\
 & b_{n+1} & = & b_{n}+1\\
\\
\end{array}
\end{math}


\subsection*{Problema 3}
\begin{center}
\xymatrix@@C=3cm@@R=2cm{
    & |Rational|^*\ar@@/^2pc/[r]^{|outList|}\ar[d]_{|cataList (h)|} & 1 + |Rational| \times |Rational|^* \ar[d]^{id + id \times |cataList (h)|}\ar@@/^2pc/[l]^{|inList|}  \\
    & |OverTime| \ |Rational|^*   & 1 + |Rational| \times |OverTime| \ |Rational|^* \ar[l]_{|h = either f g|}
}
\end{center}

\\

\begin{center}
\xymatrix@@C=3cm@@R=3cm{
    & [|Rational|^*]\ar[r]^{coalg} \ar[d]_{|anaList (coalg)|} & [|Rational|^*] + [|Rational|^*] \times [|Rational|^*]\ar[d]^{id + |anaList(coalg)| \times |anaList(coalg)|}\\
    & LTree \ [|Rational|^*] \ar@@/^2pc/[r]^{|outLTree|}\ar[d]_{|cataLTree (alg)|} & [|Rational|^*] + LTree \ [|Rational|^*] \times LTree \ [|Rational|^*] \ar[d]^{id + |cataLTree (alg)| \times |cataLTree (alg)| }\ar@@/^2pc/[l]^{|inLTree|}  \\
    & |OverTime| \ |Rational|^*   & |Rational|^* + |OverTime| \ |Rational|^* \times |OverTime| \ |Rational|^* \ar[l]_(0.6){alg \ = \ |either a b|}
}
\end{center}


\begin{code}
calcLine :: NPoint -> (NPoint -> OverTime NPoint)
calcLine = cataList h where
    h = either f g where
        f _ _ = nil
        g _ [] =  nil
        g (d,f) (x:xs) =  \z -> concat $ (sequenceA [singl . linear1d d x, f xs]) z
\end{code}


\begin{code}
deCasteljau :: [NPoint] -> OverTime NPoint
deCasteljau = hyloAlgForm alg  coalg where
    coalg = c where
        c [] = i1 []
        c [a] = i1 [a]
        c l = i2  (init l, tail l)
    alg = either a b where
        a [] = nil
        a [x]  =  const x
        b (e,d) = \pt -> (calcLine (e pt) (d pt)) pt

hyloAlgForm = hyloLTree
\end{code}


Uma outra solução para o deCasteljau, criando um novo tipo de dados intermedio.
\begin{code}
deCasteljau' :: [NPoint] -> OverTime NPoint
deCasteljau' = hyloAlgForm' alg  coalg where
  coalg = (id -|- (id -|- (split init  tail)))  . outSL
  alg = either (const nil) a
  a = either const b
  b (e,d) = \pt -> (calcLine (e pt) (d pt)) pt

outSL [] = i1 ()
outSL [a] = i2 (i1 a)
outSL l = i2  (i2 l)

hyloAlgForm' = h where
    h a b = cataC a . anaC b


data Castel a = Empty | Single a | InitTail (Castel a, Castel a) deriving Show

inC = either (const Empty) (either Single InitTail)

outC Empty = i1 ()
outC (Single a) = i2 (i1 a)
outC (InitTail (e,d)) = i2 (i2 (e,d))

fC f = id -|- (id -|- f >< f )

cataC f = f . fC (cataC f) .  outC
anaC g = inC . fC (anaC g) . g
\end{code}


\subsection*{Problema 4}
\begin{center}
\xymatrix@@C=3cm@@R=2cm{
    & |Nat0|^*\ar@@/^2pc/[r]^{|outL|}\ar[d]_{|cataL (either b q) = avg_aux|} & |Nat0| + |Nat0| \times |Nat0|^* \ar[d]^{id + id \times |cataL (either b q)|}\ar@@/^2pc/[l]^{|inL|}  \\
    & |Nat0|\times \N  & |Nat0| + |Nat0|\times (|Nat0| \times \N) \ar[l]_{|either b q|}
}
\end{center}


\begin{eqnarray*}
\start
|split avg length = cataL (either b q)|
%
\just\equiv{ Univeral-cata }
%
|split avg length . inL = cataL (either b q) . recL (split avg length)|
%
\just\equiv{ Fusão-+, Absorção-+, Eq-+, Definição de |inL|, Definição de |recL|}
%
|lcbr(
  split avg length . singl = b . id
)(
  split avg length . cons = q . id >< split avg length
)|
%
\just\equiv{ Igualdade extensional, Natural-id }
%
|lcbr(
  (split avg length . singl) x = b x
)(
  (split avg length . cons) (x,xs) = (q . id >< split avg length) (x,xs)
)|
%
\just\equiv{ Def-comp, Natural-id, Def-$\times$, Def-split, Definição de singl, Definição de cons }
%
|lcbr(
  split avg length [x] = b x
)(
  split avg length (x:xs) = q (x, (avg xs, length xs))
)|
\qed
\end{eqnarray*}



Solução para listas não vazias:
\begin{code}
avg = p1.avg_aux
\end{code}

\begin{code}
inL = either singl cons
outL [a] = i1 a
outL (a:x) = i2 (a,x)
recL f = id -|- id >< f
cataL g = g . recL (cataL g) . outL

avg_aux = cataL (either b q) where
   b x = (x,1)
   q (x,(a,l)) = ((x + (a*l)) / (l+1) ,l+1)
\end{code}


\begin{center}
\xymatrix@@C=3cm@@R=2cm{
    & LTree \ |Nat0|\ar@@/^2pc/[r]^{|outLTree|}\ar[d]_{|cataLTree (gene)|} & |Nat0| + LTree \ |Nat0| \times LTree \ |Nat0| \ar[d]^{id + |cataLTree (gene)| \times |cataLTree (gene)|}\ar@@/^2pc/[l]^{|inLTree|}  \\
    & |Nat0|\times \N  & |Nat0| + (|Nat0| \times \N) \times (|Nat0| \times \N) \ar[l]_{gene \ = \ |either b q|}
}
\end{center}


\begin{math}
\start
|split avg length = cataLTree gene|
%
\just\equiv{ Univeral-cata, gene = |either b q| }
%
|split avg length . inLTree = cataLTree (either b q) . recLTree (split avg length)|
%
\just\equiv{ Fusão-+, Absorção-+, Eq-+, Definição de inL, Definição de recLTree}
%
|lcbr(
  split avg length . Leaf = b . id
)(
  split avg length . Fork = q . split avg length >< split avg length
)|
%
\just\equiv{ Igualdade extensional, Natural-id }
%
|lcbr(
  (split avg length . Leaf) a = b a
)(
  (split avg length . Fork) (LTree a, LTree a) = (q . split avg length >< split avg length) (LTree a, LTree a)
)|
%
\just\equiv{ Def-comp, Natural-id, Def-$\times$, Def-split, Definição de Leaf, Definição de Fork }
%
|lcbr(
  (split avg length) (Leaf a) = b a
)(
  (split avg length) (Fork (LTree a, LTree a)) = q ((avg (LTree a), length (LTree a)), (avg (LTree a), length (LTree a)))
)|
\qed
\end{math}


Solução para árvores de tipo \LTree:
\begin{code}
avgLTree = p1 . cataLTree (gene) where
  gene = either b q
  b a = (a,1)
  q((a1,l1),(a2,l2)) = (((a1*l1)+(a2*l2))/(l1+l2),l1+l2)
\end{code}



\subsection*{Problema 5}
Inserir em baixo o código \Fsharp\ desenvolvido, entre \verb!\begin{verbatim}! e \verb!\end{verbatim}!:

\begin{verbatim}
\end{verbatim}

\section{Resoluções Alternativas e Simplificações sugeridas por Alef}
%format outUnOp = "out_{" UnOp "}"
%format outBinOp = "out_{" BinOp "}"

%format out = "out_{A^{*}\times B^{*}}"

%format sl = "_{" [] "}"
%format (anaList (g)) = "\ana{"g"}" sl
%format (cataList (g)) = "\cata{" g "}" sl
%format (hyloList (f) (g)) = "{\llbracket}{" f "," g "}{\rrbracket}" sl
%format (baseList (f) (g)) = "B_{[]}(" f "," g ")"

%format sCastel = "_{Castel}"
%format (cataC (g)) = "\cata{" g "}" sCastel
%format (anaC (g)) = "\ana{" g "}" sCastel
%format inC = "in" sCastel
%format outC = "out" sCastel

%format NL = "_{"NList"}"
%format outL = "out" NL
%format recL = "rec" NL
%format (cataL (g)) = "\cata{"g"}" NL

%format ℚ = "\BbbQ"

%format ⊕ = "+"

%format (cataCastel (g)) = "\cata{" g "}_{Castel}"
%format (anaCastel (g)) = "\ana{" g "}_{Castel}"
%format inCastel = "in_{" Castel "}"
%format outCastel = "out_{" Castel "}"
%format fC  = "T_{Castel}"

%format ExpAr = "\mathsf{ExpAr}"
%format sExpAr = "{_{"ExpAr"}}"
%format outExpAr = "out" sExpAr

Nessa seção mostro uma forma alternativa que percebi
de resolver alguns problemas que não poderiam ser colocadas
na seção principal por alguma razão, entre elas por
usarem extensões que não estavam já no trabalho.

Também tem simplificações triviais que acho que,
para alguns pode facilitar o entendimento. São simplesmente
funções que já estão no trabalho reescritas com uma sintaxe
mais simples em Haskell (muitas vezes usando lambda case).
Conversando com o Tiago, ele achou que seria
mais complicado explicar dessa forma. Como no final são equivalentes,
decidi deixar aqui caso alguém ache mais fácil ou simplesmente esteja
interessado em ver um pouco mais da sintaxe de Haskell, já que a última
vez que tivemos uma cadeira que usasse a linguagem diretamente foi no
1° ano.

\subsection{Problema 1}
Para criar uma interpretação de um tipo |A| como um tipo |B|. Assim, por exemplo,
posso definir que uma expressão $x$ do tipo |ExpAr a| pode ser interpretada como
|Left ()| do tipo |OutExpAr a|.
\begin{code}
class Interpretation a b where
    to ∷ a → b
\end{code}


A fim de diminuir o número de parêntese e facilitar a legibilidade defini as funções:

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
%format OutExpAr' = OutExpAr
\begin{code}
type OutExpAr' a = () ∐ a ∐ BinExp a ∐ UnExp a
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
%format outExpAr' = outExpAr
\begin{code}
outExpAr' ∷ ExpAr a → OutExpAr a
outExpAr' = to ∷ ExpAr a → OutExpAr a
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

%format recExpAr' = recExpAr
%format g_eval_exp' = g_eval_exp

%if False
\begin{code}
recExpAr' ∷ (a → e) → b ∐ c ∐ d × a × a ∐ g × a → b ∐ c ∐ d × e × e ∐ g × e
recExpAr' f = baseExpAr id id id f f id f
\end{code}
%endif

Graças a essas funções auxiliares (que acho intuitivas),
podemos simplificar a escrita de |g_eval_exp'|:
\begin{code}
g_eval_exp' ∷ Floating c ⇒ c → b ∐ c ∐ BinOp × c × c ∐ UnOp × c → c
g_eval_exp' a = const a ∐ id ∐ ap . (outBinOp × id) ∐ ap . (outUnOp × id)
\end{code}


Sabemos que $e^0 = 1$, $-0 = 0$ e
$a = 0 ∨ b = 0 ⟹ ab = 0$.
Nós optimizamos esses 4 casos:
%format clean' = clean
\begin{code}
clean' ∷ (Eq a, Num a) ⇒ ExpAr a → OutExpAr a
clean' = \case
  (Un   E        (N 0)       )  → tag 1
  (Un   Negate   (N 0)       )  → tag 0
  (Bin  Product  (N 0)  _    )  → tag 0
  (Bin  Product  _      (N 0))  → tag 0
  a                             → outExpAr a
  where tag = i2 . i1
\end{code}

%format gopt' = gopt
%if False
\begin{code}
gopt' ∷ Floating a ⇒ a → () ∐ a ∐ BinOp × a × a ∐ UnOp × a → a
gopt' = g_eval_exp
\end{code}
%endif

Baseado na função |dup| definida em \texttt{Cp.hs}:
\begin{code}
type Dup d = d × d
\end{code}


Mais dois sinônimos:
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


A fim de criar código mais sucinto extrai o que se
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
%format sd_gen' = sd_gen
%format ad_gen' = ad_gen
\begin{code}
sd_gen' ∷ Floating a ⇒ () ∐ a ∐ Bin (Dup (ExpAr a)) ∐ Un (Dup (ExpAr a)) → Dup (ExpAr a)
sd_gen' = f ∐ g ∐ h ∐ k where
  f    = const (X, N 1)
  g a  = (N a, N 0)
  h    = bin_aux (Bin Sum) (Bin Product)
  k    = un_aux (Un Negate) (Bin Product) (Un E)

ad_gen' ∷ Floating a ⇒ a → () ∐ a ∐ (BinOp, Dup (Dup a)) ∐ (UnOp, Dup a) → Dup a
ad_gen' x = f ∐ g ∐ h ∐ k where
  f    = const (x, 1)
  g a  = (a, 0)
  h    = bin_aux (+) (*)
  k    = un_aux negate (*) expd
\end{code}

%if False
\subsection{Problema 2}
%format loop' = loop
%format prj' = prj
%format cat' = cat
%format inic' = inic
\begin{code}
loop' ∷ Integral c ⇒ (c, c, c) → (c, c, c)
loop' = g where g (a, b, c) = (div (a * b) c, b + 4, c + 1)

prj' ∷ (a, b, c) → a
prj' = p where p (a, _, _) = a

inic' ∷ (Num a, Num b, Num c) ⇒ (a, b, c)
inic' = (1, 2, 2)

cat' ∷ (Integral c1, Integral c2) ⇒ c1 → c2
cat' = prj' . for loop' inic'
\end{code}
%endif



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


Vou criar um |sequenceA'| (será uma versão menos genérica de |sequenceA|
uma vez que estamos sendo específicos no trabalho com listas).
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


Sabemos que, em |Applicative ((→) r)|, @pure = const@ e
|liftA2 q f g x = q (f x) (g x)|. Logo:
\begin{code}
sequenceA' ∷ [a → b] → a → [b]
sequenceA' = cataList (either b (uncurry g)) where
  b = (const (const []))
  g x ys = (\z → x z : ys z)
\end{code}



Lembre que |zipWithM'| como vimos recebia duas listas. No nosso caso
essas duas listas (digamos |xs| e |ys|) estão em um só argumento |t=(xs,ys)|
A seguir seguem uma série de equivalências. Dentro de cada @{}@ está uma
explicação/justificativa do que foi feito de uma passo para outro.
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
%format calcLine' = calcLine
\begin{code}
calcLine'1 = curry (zipWithM''15 (uncurry linear1d))

{- Def-|zipWithM''15| -}

calcLine' = curry (hyloList (either (const (const [])) h) out) where h (a,b) = cons . split (uncurry linear1d a) b
\end{code}

%if False
\begin{code}
calcLine'1 ∷ [ℚ] → [ℚ] → Float → [ℚ]
calcLine' ∷ [ℚ] → [ℚ] → Float → [ℚ]

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

%format deCasteljau'' = deCasteljau
%format outSL' = outSL
%format hyloAlgForm'' = hyloAlgForm
%format fC' = fC


%if False
\begin{code}
deCasteljau'' ∷ [NPoint] → OverTime NPoint
deCasteljau'' = hyloAlgForm'' alg coalg where
   coalg = (id ⊕ id ⊕ split init tail) . outSL'
   alg = const nil ∐ a
   a = const ∐ b
   b (e,d) pt = calcLine' (e pt) (d pt) pt
\end{code}
%endif

\subsubsection{Notação case}

Notação lambda simplifica expressão das funções:
\begin{code}
outSL' ∷ [a] → () ∐ a ∐ [a]
outSL' = \case
  []   → i1 ()
  [a]  → i2 (i1 a)
  l    → i2 (i2 l)
\end{code}


%if False
\begin{code}
hyloAlgForm'' ∷ (() ∐ b ∐ c × c → c) → (a → d ∐ b ∐ a × a) → a → c
hyloAlgForm'' = h where
    h a b = cataCastel a . anaCastel b

newtype Castel' a = Castel' (() ∐ a ∐ Castel a × Castel a)
data Castel'' a = Empty' | Single' a | InitTail' (Castel a × Castel a) deriving Show

inCastel ∷ b ∐ a ∐ Castel a × Castel a → Castel a
inCastel = const Empty ∐ Single ∐ InitTail
\end{code}
%endif


A notação case que acho mais simples mas requer uma extensão não usada no trabalho.
\begin{code}
outCastel ∷ Castel a → () ∐ a ∐ Castel a × Castel a
outCastel = \case
  Empty           → i1 ()
  Single a        → i2 (i1 a)
  InitTail (e,d)  → i2 (i2 (e,d))
\end{code}

%if False
\begin{code}
fC' ∷ (a → d) → b1 ∐ b2 ∐ a × a → b1 ∐ b2 ∐ d × d
fC' f = id ⊕ id ⊕ f × f

cataCastel ∷ (() ∐ b ∐ d × d → d) → Castel b → d
cataCastel f = f . fC (cataCastel f) . outCastel
anaCastel ∷ (a1 → b ∐ a2 ∐ a1 × a1) → a1 → Castel a2
anaCastel g = inCastel . fC (anaCastel g) . g
\end{code}
%endif


\subsection{Problema 4}
%format outL' = outL
\subsubsection{Notação case}
Notação lambda facilita legibilidade:
\begin{code}
outL' ∷ [a] → a ∐ a × [a]
outL' = \case
  [a]   → i1 a
  (a:x) → i2 (a,x)
\end{code}

%format recL' = recL
%format cataL' = cataL
%if False
\begin{code}
recL' ∷ (c → d) → (b1 ∐ b2 × c) → b1 ∐ b2 × d
recL'  f   = id ⊕ id × f

cataL' ∷ (b ∐ b × d → d) → [b] → d
cataL' g   = g . recL (cataL g) . outL
\end{code}
%endif

%{
%format a1
%format a2
%format l1
%format l2

%format avg_aux' = avg_aux
%format avgLTree' = avgLTree

%if False
\begin{code}
avg_aux' ∷ Fractional b ⇒ [b] → (b × b)
avg_aux' = cataL (either b q) where
   b a = (a,1)
   q (h,(a,l)) = ((h + (a*l)) / (l+1) ,l+1)
\end{code}
%endif

%if False
\begin{code}
avgLTree' ∷ Fractional b ⇒ LTree b → b
avgLTree' = p1 . cataLTree (either g q) where
  g a = (a,1)
  q((a1,l1),(a2,l2)) = (((a1*l1)+(a2*l2))/(l1+l2),l1+l2)
\end{code}
%endif
%}


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

%if False
\begin{code}
type ℚ = Rational
toℚ ∷ Real a ⇒ a → ℚ
toℚ = toRational
fromℚ ∷ Fractional a ⇒ ℚ → a
fromℚ = fromRational
\end{code}
%endif


%----------------- Fim do anexo com soluções dos alunos ------------------------%

%----------------- Índice remissivo (exige makeindex) -------------------------%

\printindex

%----------------- Bibliografia (exige bibtex) --------------------------------%

\bibliographystyle{plain}
\bibliography{cp2021t}

%----------------- Fim do documento -------------------------------------------%
\end{document}
