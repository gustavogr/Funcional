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
de el vector de coeficientes de la hipótesis con el vector $x$ de la muestra.
Partiendo de la suposición de que ambos vectores tienen la misma dimensión
se puede definir el producto punto como un \texttt{foldl} sobre el zip de ambos vectores.
La función pasada al fold toma el par ($h_i$ , $x_i$), multiplica ambos términos
y los suma al acumulador.

\begin{lstlisting}

> theta :: Hypothesis Double -> Sample Double -> Double
> theta h s = foldl acum 0 $ zip (c h) (x s)
>       where acum sum (h,s) = sum + (h*s)

\end{lstlisting}
El cálculo del costo de la hipótesis se realizó con un \texttt{foldl} y una 
función que saque las cuentas finales. 
Para poder realizar el cálculo en una sola pasada el acumulador del fold
debe ser un par ordenado donde se vayan acumulando la suma de las evaluaciones
y la cantidad de muestras observadas. Una vez terminado el fold se aplica la 
función \texttt{result} que extrae la información del par generado y calcula el
costo final.

\clearpage

\begin{lstlisting}

> cost :: Hypothesis Double -> [Sample Double] -> Double
> cost h ss = result $ foldl acum (0,0) ss
>     where acum (sum, n) s = (sum + (theta h s - y s)^2 , n+1)
>           result (finalSum, finaln) = finalSum / (2 * finaln)

\end{lstlisting}

\subsection{Bajando por el gradiente}

La función \texttt{descend} fue escrita con dos folds como fue indicado.
El fold externo recorre la lista de variables de la hipótesis a mejorar,
llevando un control de cual es la posición que se está recorriendo, pues es
necesario para el fold interno. Luego el fold interno se encarga de realizar 
el cálculo de la variable mejorada. Para hacer esto recorre todo el conjunto
de muestras acumulando la sumatoria de error y contando cuantas muestras se han
observado. Los resultados de ambos fold deben ser pasados a sendas funciones 
auxiliares que extraigan el valor deseado del acumulador resultante.

\begin{lstlisting}

> descend :: Double -> Hypothesis Double -> [Sample Double]
>         -> Hypothesis Double
> descend alpha h ss = Hypothesis $ 
>                       extract $ foldl improve ([],0) (c h)
>   where extract (xs,_)    = reverse xs
>         improve (h',n) hj = ((hj - 
>                               (result $ foldl (err n) (0,0) ss)
>                             ):h', n+1)
>           where result (ac, m)  = ac * alpha / m 
>                 err n (ac, m) s = (ac + 
>                                       (theta h s - (y s))*
>                                              ((x s) !! n)
>                                   , m+1)

\end{lstlisting}

Finalmente queda definir la función gd. Fue escrita usando un \texttt{unfoldr}.
Las semillas son de tipo \texttt{(Hypothesis Double, Integer)} pues se debe
llevar también el número de iteraciones para agregarlo a la tupla de resultado.
La iteración se termina cuando el costo de la nueva hipótesis y el costo de la 
anterior son $e-$cercanas.

\begin{lstlisting}

> gd :: Double -> Hypothesis Double -> [Sample Double]
>    -> [(Integer,Hypothesis Double,Double)]
> gd alpha h ss = unfoldr go (h,0)
>   where go (h,n) = if veryClose (cost h ssw1) (cost h' ssw1) 
>                    then Nothing
>                    else Just ((n, h, cost h ssw1), (h', n+1))
>           where ssw1 = addOnes ss
>                 h'   = descend alpha h ssw1

\end{lstlisting}

\section{Monoids}

Para tomar en cuenta los casos en los que el \texttt{Foldable} esté
vacío se define el tipo \texttt{Max a} como una instancia de 
\texttt{Maybe a}.

\begin{lstlisting}

> newtype Max a = Max { getMax :: Maybe a } deriving (Eq, Show)

\end{lstlisting}

Para que nuestro tipo \texttt{Max a} pueda instanciar la clase \texttt{Monoid}
debemos pedir como mínimo que el tipo \texttt{a} sea instancia de \texttt{Ord}.

\begin{lstlisting}

> instance Ord a => Monoid (Max a) where

\end{lstlisting}

Solo falta definir las funciones \texttt{mempty} y \texttt{mappend}.
\texttt{Mempty} lo usaremos para representar el caso base de nuestro
\emph{Monoide}, que en el caso de la clase \texttt{Maybe} es \texttt{Nothing}.

\begin{lstlisting}

>   mempty = Max Nothing

\end{lstlisting}

\texttt{Mappend} de dos elementos se define como el máximo de sus dos valores, en 
caso de que ninguno sea \texttt{Nothing}.  

\begin{lstlisting}

>   mappend x y = case getMax x of 
>                   Nothing -> y
>                   Just n -> case getMax y of 
>                               Just m -> (Max . Just) $ max m n
>                               _ -> x 

\end{lstlisting}

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
