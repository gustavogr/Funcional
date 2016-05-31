
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

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 2}

\author{Ernesto Hernández-Novich\\
86-17791\\
\href{mailto:emhn@usb.ve}{<emhn@usb.ve>}}

\date{Mayo 12, 2016}

\maketitle

\pagebreak

\section*{Yo dawg! I heard you liked functors\ldots}

\ignore{

> {-# LANGUAGE UndecidableInstances #-}

> import Control.Applicative
> import Control.Monad

> data Bonus f a = Malus a
>                | Bonus (f (Bonus f a))

> instance (Show a, Show (f (Bonus f a))) => Show (Bonus f a) where
>   show (Malus x) = "Malus " ++ show x
>   show (Bonus bs)= "Bonus " ++ show bs

}


Declaración de instancias de \texttt{Functor}, \texttt{Applicative}
y \texttt{Monad} para el tipo \texttt{Functor~f}.

Para \texttt{Functor} es necesario implementar \texttt{fmap}. Para 
el constructor \texttt{Bonus} es necesario hacer \texttt{fmap} sobre 
el functor interno y luego \texttt{fmap} sobre el \texttt{Bonus} interno.

\begin{lstlisting}

> instance Functor f => Functor (Bonus f) where
>   fmap g (Malus a)  = Malus (g a)
>   fmap g (Bonus bs) = Bonus (fmap (fmap g) bs)

\end{lstlisting}

El \texttt{Applicative} pide la implentación de \texttt{pure} y del operador 
\texttt{<*>}. Si el primer elemento es un \texttt{Malus} simplemente mappeamos su función 
sobre el segundo elemento. Si el segundo elemento es un \texttt{Malus} mappeamos la 
evaluacion de su elemento sobre todo el primer elemento. Finalmente si ambos 
elementos son \texttt{Bonus}, mappeamos \texttt{b1 <*>} sobre b2.

\begin{lstlisting}

> instance Functor f => Applicative (Bonus f) where
>   pure = Malus
>   (Malus f)    <*>     x      = fmap f x
>   bs@(Bonus _) <*> (Malus a)  = fmap (flip ($) a) bs
>   bs1          <*> (Bonus f)  = Bonus (fmap (bs1 <*>) f)

\end{lstlisting}

Para el \texttt{Monad} es necesario \texttt{return} y el \texttt{bind}.
Si el primer elemento del bind es un \texttt{Malus} simplemente sacamos
su valor y lo pasamos a la función. Si es un \texttt{Bonus}, mappeamos el bind
dentro de su functor.

\begin{lstlisting}

> instance Functor f => Monad (Bonus f) where
>   return = Malus
>   (Malus x)  >>= f = f x
>   (Bonus bs) >>= f = Bonus (fmap (flip (>>=) f) bs)

\end{lstlisting}

\section*{Can I has pizza?}


\begin{lstlisting}

> data Want a = Want ((a -> Pizza) -> Pizza)

> data Pizza = Eat (IO Pizza)
>            | Combo Pizza Pizza
>            | Happy

> instance Show Pizza where
>    show (Eat x)     = " :-P "
>    show (Combo x y) = " combo(" ++ show x 
>                                 ++ "," 
>                                 ++ show y ++ ") "
>    show Happy       = " :-D "

\end{lstlisting}

Al \texttt{Want} hay que pasarle una función que devuelva una \texttt{Pizza}. El único
constructor que no necesita elementos es \texttt{Happy}.

\begin{lstlisting}

> want :: Want a -> Pizza
> want (Want f) = f (\_ -> Happy)

\end{lstlisting}

\texttt{happy} simplemente devuelve un \texttt{Want} que solo devuelve un \texttt{Happy}.

\begin{lstlisting}

> happy :: Want a
> happy = Want (\_ -> Happy)

\end{lstlisting}

\texttt{nomnom} recibe una acción de \texttt{IO}. Se devuelve un 
\texttt{Want} que cuando reciba su función \texttt{(a -> Pizza)} liftea
esa función a la acción monádica para que pase de ser \texttt{IO a} a \texttt{IO Pizza}
y pasarlo al constructor \texttt{Eat}.

\begin{lstlisting}

> nomnom :: IO a -> Want a
> nomnom ioAct = Want (\f -> Eat (liftM f ioAct))

\end{lstlisting}

\texttt{combo} recibe un \texttt{Want} y devuelve otro que espera una función de tipo
\texttt{() -> Pizza} y cuando la recibe devuelve un \texttt{Combo}.

\begin{lstlisting}

> combo :: Want a -> Want ()
> combo w = Want (\f -> Combo (want w) (f ()))

\end{lstlisting}

\texttt{pana} combina dos \texttt{Want}s en uno solo. Para ello encierra las funciones
de los dos \texttt{Want} evaluadas con la funcion que recibirá el resultado y los junta
con un \texttt{Combo}.

\begin{lstlisting}

> pana :: Want a -> Want a -> Want a
> pana (Want f1) (Want f2) = Want (\x -> Combo (f1 x) (f2 x))

\end{lstlisting}

\texttt{pizzeria} funciona con recursion de cola sobre la lista que recibe.
Por cada Pizza que lee revisa su constructor. Si es un \texttt{Eat}, ejecuta su
acción monádica y el resultado (que es una \texttt{Pizza}) lo pone al final de la 
lista. Si es un \texttt{Combo} simplemente agrega sus dos \texttt{Pizzas} al final
de la cola. Si es un \texttt{Happy} no hace nada.


\begin{lstlisting}

> pizzeria :: [Pizza] -> IO ()
> pizzeria []     = return ()
> pizzeria (p:ps) = do case p of 
>                         Eat f -> do piz <- f
>                                     pizzeria (ps ++ [piz])
>                         Combo p1 p2 -> pizzeria (ps ++ [p1,p2])
>                         Happy -> return ()

\end{lstlisting}

El \texttt{bind} del \texttt{Monad} devuelve un Want que incluye el anterior.
Al \texttt{Want} inicial le pasa una función que evalúa el \texttt{a} que recibe
con la función \texttt{g}. El resultado de esto es otro \texttt{Want}, al cual le 
extraemos su función interna y esa la evaluaríamos con el argumento que espera el
\texttt{Want} final.

\begin{lstlisting}

> instance Monad Want where
>   return x       = Want (\f -> f x)
>   (Want w) >>= g = Want (\b -> w (\a -> (exctract (g a)) b))
>       where exctract (Want a) = a

\end{lstlisting}

\ignore{
\begin{lstlisting}

> hambre :: Want ()
> hambre = pana (ponle (topping 42)) 
>               (pana (ponle (topping 69))
>                     (pana (ponle (topping 17)) 
>                           (ponle (topping 23) >> nomnom (putStrLn ""))))
> 
> tengo :: Want a -> IO ()
> tengo x = pizzeria [want x]
> 
> topping :: Int -> String
> topping 17 = "/nlmce"
> topping 23 = "/y./p6"
> topping 42 = "htptuc2"
> topping 69 = "t:irofr"
> 
> ponle :: String -> Want ()
> ponle xs = mapM_ (nomnom . putChar) xs

\end{lstlisting}
}

\end{document}
