{-# OPTIONS_GHC -XNPlusKPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, FlexibleInstances #-}
module HaddockTeste where
import Solucoes
import Cp
import Test.QuickCheck

-- * Preâmbulo

{- $doc
[Cálculo de Programas](https://haslab.github.io/CP/) tem como objectivo principal ensinar
a programação de computadores como uma disciplina científica. Para isso
parte-se de um repertório de /combinadores/ que formam uma álgebra da
programação (conjunto de leis universais e seus corolários) e usam-se esses
combinadores para construir programas /composicionalmente/, isto é,
agregando programas já existentes.

Na sequência pedagógica dos planos de estudo dos dois cursos que têm
esta disciplina, opta-se pela aplicação deste método à programação
em [Haskell](http://www.haskell.org) (sem prejuízo da sua aplicação a outras linguagens
funcionais). Assim, o presente trabalho prático coloca os
alunos perante problemas concretos que deverão ser implementados em [Haskell](http://www.haskell.org).  Há ainda um outro objectivo: o de ensinar a documentar
programas, a validá-los e a produzir textos técnico-científicos de
qualidade.
-}

-- * Documentação

{- $doc

Para cumprir de forma integrada os objectivos
enunciados acima vamos recorrer a uma técnica de programação dita
"[literária](http://www.literateprogramming.com)" [[1](#1)], cujo princípio base é o seguinte:

"Um programa e a sua documentação devem coincidir."

Por outras palavras, o código fonte e a documentação de um
programa deverão estar no mesmo ficheiro.

O ficheiro @cp2021t.pdf@ que está a ler é já um exemplo de
[programação literária](http://www.literateprogramming.com): foi gerado a partir do texto fonte
@cp2021t.lhs@ que encontrará no [Material Pedagógico](https://haslab.github.io/CP/Material/) desta disciplina descompactando o ficheiro
@cp2021t.zip@ e executando:

@
  $ 'lhs2TeX' cp2021t.lhs > cp2021t.tex
  $ pdflatex cp2021t
@

em que [@lhs2tex@](https://hackage.haskell.org/package/lhs2tex) é
um pre-processador que faz "pretty printing"
de código Haskell em [\(\ \LaTeX\)](http://www.tug.org/index.html) e que deve desde já instalar executando

@
  $ 'cabal' install 'lhs2tex' --lib
@

Por outro lado, o mesmo ficheiro @cp2021t.lhs@ é executável e contém
o "kit" básico, escrito em [Haskell](http://www.haskell.org), para realizar o trabalho. Basta executar

@
  $ ghci cp2021t.lhs
@

Abra o ficheiro __/cp2021t.lhs/__ no seu editor de texto preferido
e verifique que assim é: todo o texto que se encontra dentro do ambiente

@
\\begin{code}
...
\\end{code}
@

é seleccionado pelo [GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html) para ser executado.

-}

-- * Como realizar o trabalho

{- $doc

Este trabalho teórico-prático deve ser realizado por grupos de 3 (ou 4) alunos.
Os detalhes da avaliação (datas para submissão do relatório e sua defesa
oral) são os que forem publicados na [página da disciplina](https://haslab.github.io/CP/) na /internet/.

Recomenda-se uma abordagem participativa dos membros do grupo
de trabalho por forma a poderem responder às questões que serão colocadas
na /defesa oral/ do relatório.

Em que consiste, então, o /relatório/ a que se refere o parágrafo anterior?
É a edição do texto que está a ser lido, preenchendo o anexo \ref{sec:resolucao}
com as respostas. O relatório deverá conter ainda a identificação dos membros
do grupo de trabalho, no local respectivo da folha de rosto.

Para gerar o PDF integral do relatório deve-se ainda correr os comando seguintes,
que actualizam a bibliografia (com [\(\textrm{Bib}\TeX\)](http://www.bibtex.org/)) e o índice remissivo (com [@makeindex@](https://www.ctan.org/pkg/makeindex)),

@
  $ bibtex cp2021t.aux
  $ makeindex cp2021t.idx
@

e recompilar o texto como acima se indicou. Dever-se-á ainda instalar
o utilitário [QuickCheck](https://wiki.haskell.org/Introduction_to_QuickCheck1),
que ajuda a validar programas em [Haskell](http://www.haskell.org) e a biblioteca [Gloss](https://hackage.haskell.org/package/gloss-1.13.1.1/docs/Graphics-Gloss.html) para
geração de gráficos 2D:

@
  $ 'cabal' install 'QuickCheck' 'gloss' --lib
@

Para testar uma propriedade [QuickCheck](https://wiki.haskell.org/Introduction_to_QuickCheck1) /prop/, basta invocá-la com o comando:

@
    > 'quickCheck' prop
    +++ OK, passed 100 tests.
@

Pode-se ainda controlar o número de casos de teste e sua complexidade,
como o seguinte exemplo mostra:

@
  > 'quickCheckWith' stdArgs { maxSuccess = 200, maxSize = 10 } prop
  +++ OK, passed 200 tests.
@

Qualquer programador tem, na vida real, de ler e analisar (muito!) código
escrito por outros. No anexo {ref: C} disponibiliza-se algum
código [Haskell](http://www.haskell.org) relativo aos problemas que se seguem. Esse anexo deverá
ser consultado e analisado à medida que isso for necessário.
-}

-- ** Stack

{- $doc
O [Stack](https://docs.haskellstack.org/en/stable/README/) é um programa útil para criar, gerir e manter projetos em [Haskell](http://www.haskell.org).
Um projeto criado com o Stack possui uma estrutura de pastas muito específica:

* Os módulos auxiliares encontram-se na pasta \emph{src}.
* O módulos principal encontra-se na pasta \emph{app}.
* A lista de depêndencias externas encontra-se no ficheiro \emph{package.yaml}.

Pode aceder ao [GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html) utilizando o comando:

@stack ghci@

Garanta que se encontra na pasta mais externa /do projeto/.
A primeira vez que correr este comando as depêndencias externas serão instaladas automaticamente.

Para gerar o PDF, garanta que se encontra na diretoria __app__.
-}

-- * Problemas

-- ** Problema 1

{- $doc2

Os /tipos de dados algébricos/ estudados ao longo desta disciplina oferecem
uma grande capacidade expressiva ao programador. Graças à sua flexibilidade,
torna-se trivial implementar [DSL]s(https://www.researchgate.net/publication/254462947_Experience_report_A_do-it-yourself_high-assurance_compiler)
e até mesmo [linguagens de programação](http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf).

Paralelamente, um tópico bastante estudado no âmbito de __DL__
é a derivação automática de expressões matemáticas, por exemplo, de derivadas.
Duas técnicas que podem ser utilizadas para o cálculo de derivadas são:

* /Symbolic differentiation/
* /Automatic differentiation/

/Symbolic differentiation/ consiste na aplicação sucessiva de transformações
(leia-se: funções) que sejam congruentes com as regras de derivação. O resultado
final será a expressão da derivada.

O leitor atento poderá notar um problema desta técnica: a expressão
inicial pode crescer de forma descontrolada, levando a um cálculo pouco eficiente.
/Automatic differentiation/ tenta resolver este problema,
calculando __o valor__ da derivada da expressão em todos os passos.
Para tal, é necessário calcular o valor da expressão __e__ o valor da sua derivada.

Vamos de seguida definir uma linguagem de expressões matemáticas simples e
implementar as duas técnicas de derivação automática.
Para isso, seja dado o seguinte tipo de dados,

@
data 'ExpAr' a = 'X'
           | 'N' a
           | 'Bin' 'BinOp' ('ExpAr' a) ('ExpAr' a)
           | 'Un' 'UnOp' ('ExpAr' a)
           deriving ('Eq', 'Show')
@

onde 'BinOp' e 'UnOp' representam operações binárias e unárias, respectivamente:

data 'BinOp' = 'Sum'
           | 'Product'
           deriving ('Eq', 'Show')

data 'UnOp' = 'Negate'
          | 'E'
          deriving ('Eq', 'Show')

O construtor 'E' simboliza o exponencial de base \(e\).

Assim, cada expressão pode ser uma variável, um número, uma operação binária
aplicada às devidas expressões, ou uma operação unária aplicada a uma expressão.

Por exemplo,

@
'Bin' 'Sum' 'X' ('N' 10)
@

designa \(x+10\) na notação matemática habitual.

A definição das funções 'inExpAr' e 'baseExpAr' para este tipo é a seguinte:

@
'inExpAr' = 'either' ('const' 'X') num_ops where
  num_ops = 'either' 'N' ops
  ops     = 'either' bin ('uncurry' 'Un')
  bin(op, (a, b)) = 'Bin' op a b

'baseExpAr' f g h j k l z = f '-|-' (g '-|-' (h '><' (j '><' k) '-|-' l '><' z))
@

Defina as funções 'outExpAr' e 'recExpAr',
e teste as propriedades que se seguem.

#. Propriedade
    'inExpAr' e 'outExpAr' são testemunhas de um isomorfismo,
    isto é, 'inExpAr' ⋅ 'outExpAr' = 'id' e 'outExpAr' ⋅ 'idExpAr' = 'id':

@
'prop_in_out_idExpAr' :: ('Eq' a) => 'ExpAr' a → 'Bool'
'prop_in_out_idExpAr' = 'inExpAr' ⋅ 'outExpAr' .==. 'id'

'prop_out_in_idExpAr' :: ('Eq' a) => 'OutExpAr' a → 'Bool'
'prop_out_in_idExpAr' = 'outExpAr' ⋅ 'inExpAr' .==. 'id'
@

[#1#]: D.E. Knuth. Literate Programming. CSLI Lecture Notes Number 27. Stanford University Center for the Study of Language and Information, Stanford, CA, USA, 1992.
-}
