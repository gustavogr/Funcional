\documentclass[11pt,fleqn]{article}

\usepackage{tikz}
\usepackage{multicol}
\usepackage{latexsym}
\usepackage{array}
\usepackage[english,spanish]{babel}
\usepackage{lmodern}
\usepackage{listings}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}
\usepackage{xcolor}

\usepackage{algorithmic}
\usepackage{algorithm}

\usetikzlibrary{positioning,shapes,folding,positioning,shapes,trees}

\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=blue
}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}



\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey}
}

\long\def\ignore#1{}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Respuesta de la Tarea 1}

\author{Gustavo Gutiérrez\\
11-10428\\
\href{mailto:11-10428@usb.ve}{<11-10428@usb.ve>}}

\date{Abril 27, 2016}

\maketitle

\pagebreak

\section{Machine Learning}

\ignore{

> import Data.List
> import Data.Functor
> import Data.Monoid
> import Data.Foldable (foldMap)
> import Data.Tree
> import Graphics.Rendering.Chart.Easy
> import Graphics.Rendering.Chart.Backend.Cairo

> data Sample a = Sample { x :: [a], y :: a }
>      deriving (Show)

> data Hypothesis a = Hypothesis { c :: [a] }
>      deriving (Show)

> alpha :: Double
> alpha = 0.03

> epsilon :: Double
> epsilon = 0.0000001

> guess :: Hypothesis Double
> guess = Hypothesis { c = [0, 0, 0] }

> training :: [Sample Double]
> training = [ 
>  Sample { x = [  0.1300098690745405, -0.2236751871685913 ], y = 399900 },
>  Sample { x = [ -0.5041898382231769, -0.2236751871685913 ], y = 329900 },
>  Sample { x = [  0.502476363836692, -0.2236751871685913 ], y = 369000 },
>  Sample { x = [ -0.7357230646969468, -1.537766911784067 ], y = 232000 },
>  Sample { x = [  1.257476015381594, 1.090416537446884 ], y = 539900 },
>  Sample { x = [ -0.01973172848186497, 1.090416537446884 ], y = 299900 },
>  Sample { x = [ -0.5872397998931161, -0.2236751871685913 ], y = 314900 },
>  Sample { x = [ -0.7218814044186236, -0.2236751871685913 ], y = 198999 },
>  Sample { x = [ -0.7810230437896409, -0.2236751871685913 ], y = 212000 },
>  Sample { x = [ -0.6375731099961096, -0.2236751871685913 ], y = 242500 },
>  Sample { x = [ -0.07635670234773261, 1.090416537446884 ], y = 239999 },
>  Sample { x = [ -0.0008567371932424295, -0.2236751871685913 ], y = 347000 },
>  Sample { x = [ -0.1392733399764744, -0.2236751871685913 ], y = 329999 },
>  Sample { x = [  3.117291823687202,   2.40450826206236 ], y = 699900 },
>  Sample { x = [ -0.9219563120780225, -0.2236751871685913 ], y = 259900 },
>  Sample { x = [  0.3766430885792084,  1.090416537446884 ], y = 449900 },
>  Sample { x = [ -0.856523008944131,  -1.537766911784067 ], y = 299900 },
>  Sample { x = [ -0.9622229601604173, -0.2236751871685913 ], y = 199900 },
>  Sample { x = [  0.7654679091248329,  1.090416537446884 ], y = 499998 },
>  Sample { x = [  1.296484330711414,   1.090416537446884 ], y = 599000 },
>  Sample { x = [ -0.2940482685431793, -0.2236751871685913 ], y = 252900 },
>  Sample { x = [ -0.1417900054816241, -1.537766911784067 ], y = 255000 },
>  Sample { x = [ -0.4991565072128776, -0.2236751871685913 ], y = 242900 },
>  Sample { x = [ -0.04867338179108621, 1.090416537446884 ], y = 259900 },
>  Sample { x = [  2.377392165173198,  -0.2236751871685913 ], y = 573900 },
>  Sample { x = [ -1.133356214510595,  -0.2236751871685913 ], y = 249900 },
>  Sample { x = [ -0.6828730890888036, -0.2236751871685913 ], y = 464500 },
>  Sample { x = [  0.6610262906611214, -0.2236751871685913 ], y = 469000 },
>  Sample { x = [  0.2508098133217248, -0.2236751871685913 ], y = 475000 },
>  Sample { x = [  0.8007012261969283, -0.2236751871685913 ], y = 299900 },
>  Sample { x = [ -0.2034483103577911, -1.537766911784067 ], y = 349900 },
>  Sample { x = [ -1.259189489768079,  -2.851858636399542 ], y = 169900 },
>  Sample { x = [  0.04947657290975102, 1.090416537446884 ], y = 314900 },
>  Sample { x = [  1.429867602484346,  -0.2236751871685913 ], y = 579900 },
>  Sample { x = [ -0.2386816274298865,  1.090416537446884 ], y = 285900 },
>  Sample { x = [ -0.7092980768928753, -0.2236751871685913 ], y = 249900 },
>  Sample { x = [ -0.9584479619026928, -0.2236751871685913 ], y = 229900 },
>  Sample { x = [  0.1652431861466359,  1.090416537446884 ], y = 345000 },
>  Sample { x = [  2.78635030976002,    1.090416537446884 ], y = 549000 },
>  Sample { x = [  0.202993168723881,   1.090416537446884 ], y = 287000 },
>  Sample { x = [ -0.4236565420583874, -1.537766911784067 ], y = 368500 },
>  Sample { x = [  0.2986264579195686, -0.2236751871685913 ], y = 329900 },
>  Sample { x = [  0.7126179335166897,  1.090416537446884 ], y = 314000 },
>  Sample { x = [ -1.007522939253111,  -0.2236751871685913 ], y = 299000 },
>  Sample { x = [ -1.445422737149154,  -1.537766911784067 ], y = 179900 },
>  Sample { x = [ -0.1870899845743182,  1.090416537446884 ], y = 299900 },
>  Sample { x = [ -1.003747940995387,  -0.2236751871685913 ], y = 239500 } ]

}

\subsection{Implementación}

\subsubsection{Comparar en punto flotante}

Dos flotantes se consideraran \emph{cercanos} en el caso en el que la
diferencia entre ambos sea menor a $\epsilon$.

\begin{lstlisting}

> veryClose :: Double -> Double -> Bool
> veryClose v0 v1 = abs (v1 - v0) < epsilon 

\end{lstlisting}

\subsubsection{Congruencia dimensional}

Para solucionar la congruencia dimensional se le hace un map a la lista
de Samples con una función que toma un Sample y le agrega un uno a su 
lista de coeficientes.

\begin{lstlisting}

> addOnes :: [Sample Double] -> [Sample Double]
> addOnes = map addOne 
>       where addOne sam = Sample { x = 1:(x sam), y = y sam }

\end{lstlisting}

\subsubsection{Evaluando Hipótesis}
La evaluación de la hipótesis sobre una muestra implica realizar el producto punto
de el vector de coeficientes de la hipotesis con el vector $x$ de la muestra.
Partiendo de la suposición de que ambos vectores tienen la misma dimensión
se puede definir el producto punto como un \emph{foldl} sobre el zip de ambos vectores.
La función pasada al fold toma el par ($h_i$ , $x_i$), multiplica ambos términos
y los suma al acumulador.

\begin{lstlisting}

> theta :: Hypothesis Double -> Sample Double -> Double
> theta h s = foldl acum 0 $ zip (c h) (x s)
>       where acum sum (h,s) = sum + (h*s)

\end{lstlisting}
El calculo del costo de la hipótesis se realizó con un \emph{foldl} y una 
función que saque las cuentas finales. 
Para poder realizar el cálculo en una sola pasada el acumulador del fold
debe ser un par ordenado donde se vayan acumulando la suma de las evaluaciones
y la cantidad de muestras observadas. Una vez terminado el fold se aplica la 
función \emph{result} que extrae la información del par generado y calcula el
costo final.

\begin{lstlisting}

> cost :: Hypothesis Double -> [Sample Double] -> Double
> cost h ss = result $ foldl acum (0,0) ss
>     where acum (sum, n) s = (sum + (theta h s - y s)^2 , n+1)
>           result (finalSum, finaln) = finalSum / (2 * finaln)

\end{lstlisting}

\subsection{Bajando por el gradiente}

\begin{lstlisting}

> descend :: Double -> Hypothesis Double -> [Sample Double]
>         -> Hypothesis Double
> descend alpha h ss = Hypothesis $ extract $ foldl improve ([],0) (c h)
>     where extract (xs,_) = reverse xs
>           improve (hn,n) hj = ((hj - (result $ foldl (error n) (0,0) ss)):hn, n+1)
>                 where result (acum, m) = acum * alpha / m 
>                       error n (acum, m) s = (acum + (theta h s - (y s))*((x s) !! n), m+1)

\end{lstlisting}

Sea $\theta_j$ el $j-$ésimo componente del vector $\theta$
correspondiente a la hipótesis actual que pretendemos mejorar. La
función debe calcular, para todo $j$

$$\theta'_j \leftarrow \theta_j -
\frac{\alpha}{m}\sum_{i=1}^m{(h_\theta(x^{(i)})-y^{(i)})x_j^{(i)}}$$

donde $m$ es el número de muestras de entrenamiento.

Su función debe ser escrita exclusivamente empleando funciones
de orden superior. En particular, se trata de dos \emph{fold}
anidados, cada uno de ellos realizando sus cómputos en
\emph{una sola pasada}. Debe escribirla de manera general,
suponiendo que \texttt{ss} podría tener una cantidad arbitraria
de muestras disponibles y que $j$ es arbitrario.

La segunda parte de este algoritmo debe partir de una hipótesis
inicial y el conjunto de entrenamiento, para producir una lista
de elementos tales que permitan determinar, para cada iteración,
cuál es la hipótesis mejorada y el costo de la misma.

\begin{lstlisting}

> gd :: Double -> Hypothesis Double -> [Sample Double]
>    -> [(Integer,Hypothesis Double,Double)]
> gd alpha h ss = undefined

\end{lstlisting}

Su función debe ser escrita como un \emph{unfold}. Note que esta
será la función ``tope'', por lo tanto debe asegurarse de agregar
los coeficientes 1 antes de comenzar a iterar, y mantener la
iteración hasta que la diferencia entre los costos de dos
hipótesis consecutivas sean $\epsilon-$despreciables.

\subsection{¿Cómo sé si lo estoy haciendo bien?}

Probar las funciones \texttt{veryClose}, \texttt{addOnes}
y \texttt{theta} es trivial por inspección. Para probar la
función \texttt{cost} tendrá que hacer algunos cálculos a
mano con muestras pequeñas y comprobar los resultados que
arroja la función. Preste atención que estas funciones
\emph{asumen} que las muestras ya incluyen el coeficiente
constante 1.

Probar la función \texttt{descend} es algo más complicado,
pero la sugerencia general es probar paso a paso si se
produce una nueva hipótesis cuyo costo es, en efecto, menor.

Con las definiciones en este archivo, si su algoritmo está
implantado correctamente, hay convergencia. Para que tenga
una idea

\begin{lstlisting}
  ghci> take 3 (gd alpha guess training)
  [(0,Hypothesis {c = [0.0,0.0,0.0]},6.559154810645744e10),
   (1,Hypothesis {c = [10212.379787234042,3138.9880129854737,...
   (2,Hypothesis {c = [20118.388180851063,6159.113611965675,...]
\end{lstlisting}

y si se deja correr hasta terminar converge (el \emph{unfold}
\textbf{termina}) y los resultados numéricos en la última tripleta
deberían ser muy parecidos a (indentación mía)

\begin{lstlisting}
  (1072,
  Hypothesis {c = [340412.65957446716,
                   110631.04133702737,
                   -6649.4653290010865]},
  2.043280050602863e9)
\end{lstlisting}

Para su comodidad, he incluido la función \texttt{graph} de la
magnífica librería \texttt{chart} que permite hacer gráficos
sencillos (línea, torta, barra, etc.). Puede usarla para verificar
que su función está haciendo el trabajo

\begin{lstlisting}
  ghci> graph "works" (gd alpha guess training)
\end{lstlisting}

y en el archivo \texttt{works.png} debe obtener una imagen
similar a
\begin{center}
        \includegraphics[width=11cm]{works.png}
\end{center}

\subsection{¿Aprendió?}

Una vez que el algoritmo converge, obtenga la última hipótesis
y úsela para predecir el valor $y$ asociado al vector
$(-0.44127, -0.22368)$.

\begin{verbatim}
  ghci> let (_,h,_) = last (gd alpha guess training)
  ghci> let s = Sample ( x = [1.0, -0.44127,-0.22368], y = undefined }
  ghci> theta h s
  293081.85236
\end{verbatim}

\section{Monoids}

Durante la discusión en clase acerca de \texttt{Monoid} se dejó
claro que para algunos tipos de datos existe más de una instancia
posible. En concreto, para los números puede construirse una
instancia \texttt{Sum} usando \texttt{(+)} como operación y
\texttt{0} como elemento neutro, pero también puede construirse
una instancia \texttt{Product} usando \texttt{(*)} como operación
y \texttt{1} como elemento neutro. La solución al problema resultó
ser el uso de tipos equivalentes pero incompatibles aprovechando
\texttt{newtype}.

Siguiendo esa idea, construya una instancia \texttt{Monoid}
\emph{polimórfica} para \emph{cualquier} tipo comparable, tal que
al aplicarla sobre cualquier \texttt{Foldable} conteniendo 
elementos de un tipo concreto comparable, se retorne el máximo
valor almacenado, si existe. La aplicación se logra con la
función

\begin{verbatim}
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
\end{verbatim}

Note que en este caso \texttt{a} es el tipo comparable, y la
primera función debe levantar el valor libre al \texttt{Monoid}
calculador de máximos. Piense que el \texttt{Foldable} \texttt{t}
\emph{podría} estar vacío (lista, árbol, \ldots) así que el
\texttt{Monoid} debe operar con ``seguridad''

Oriéntese con los siguientes ejemplos

\begin{verbatim}
ghci> foldMap (Max . Just) []
Max {getMax = Nothing}
ghci> foldMap (Max . Just) ["foo","bar","baz"]
Max {getMax = Just "foo"}
ghci> foldMap (Max . Just) (Node 6 [Node 42 [], Node 7 [] ])
Max {getMax = Just 42}
ghci> foldMap (Max . Just) (Node [] [])
\end{verbatim}

\section{Zippers}

Considere el tipo de datos

\begin{lstlisting}

> data Filesystem a = File a | Directory a [Filesystem a]

\end{lstlisting}

Diseñe un zipper seguro para el tipo \texttt{Filesystem}
proveyendo todas las funciones de soporte que permitan trasladar
el foco dentro de la estructura de datos, así como la modificación
de cualquier posición dentro de la estructura.

\begin{lstlisting}

 data Breadcrumbs a = undefined

 type Zipper a = (Filesystem a, Breadcrumbs a)

 goDown   ::
 goRight  ::
 goLeft   ::
 goBack   ::
 tothetop :: 
 modify   ::
 focus    ::
 defocus  ::

\end{lstlisting}

\end{document}
