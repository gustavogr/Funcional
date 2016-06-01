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

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 3}

\author{Gustavo Gutiérrez\\
11-10428\\
\href{mailto:11-10428@usb.ve}{<11-10428@usb.ve>}}

\date{Junio 1, 2016}

\maketitle

\pagebreak

\ignore{

> {-# LANGUAGE TemplateHaskell #-}

> import Test.QuickCheck
> import Test.QuickCheck.All
> import Data.Maybe (fromJust)

> type Buffer = (String,String)

}

\section{Buffer de texto}

\subsection{Implementación}

\begin{lstlisting}
 
> empty :: Buffer                     -- Buffer nuevo
> empty = ("","")
 
> cursor :: Buffer -> Maybe Char      -- Leer bajo el cursor
> cursor (_,[])   = Nothing
> cursor (_,c:_)  = Just c
 
> insert :: Char -> Buffer -> Buffer  -- ...antes del cursor
> insert c (as,ds) = (c:as, ds)
 
> delete :: Buffer -> Buffer          -- ...anterior al cursor
> delete ([],ds)   = ([],ds)
> delete (_:as,ds) = (as,ds)
 
> remove :: Buffer -> Buffer          -- ...bajo al cursor
> remove (as,[])   = (as,[])
> remove (as,_:ds) = (as,ds)
 
> left :: Buffer -> Buffer            -- Cursor a la izquierda
> left ([],ds)   = ([],ds)
> left (a:as,ds) = (as,a:ds)
 
> right :: Buffer -> Buffer           -- Cursor a la derecha
> right (as,[])   = (as,[])
> right (as,d:ds) = (d:as,ds)
 
> atLeft :: Buffer -> Bool            -- Extremo izquierdo?
> atLeft (as,_) = null as
 
> atRight :: Buffer -> Bool           -- Extremo derecho?
> atRight (_,ds) = null ds

\end{lstlisting}

\subsection{Propiedades a probar}

Moverse a la izquierda y luego a la derecha sobre un buffer
devuelve el mismo buffer.

\begin{lstlisting}
 
> prop_right_and_left bf =
>   not (atRight bf) ==> bf == (left . right) bf  
 
\end{lstlisting}

Usar \texttt{delete} es lo mismo que moverse a la izquierda y usar
\texttt{remove}.

\begin{lstlisting}

> prop_left_and_remove bf =
>   not (atLeft bf) ==> delete bf == (remove . left) bf
 
> prop_remove_at_right str =
>   bf == remove bf
>     where bf = (str,"")
 
> prop_delete_at_left str = 
>   bf == delete bf
>     where bf = ("",str)
 
> prop_empty_buffer =
>   bf == left bf && bf == right bf && cursor bf == Nothing
>     where bf = empty
 
> prop_insert_and_check c bf =
>   c == fromJust bf'
>     where bf' = (cursor . left . insert c) bf
 
> prop_insert_and_delete c bf =
>   bf == (delete . insert c) bf
 
> return []
> runTests = $quickCheckAll

> main = runTests
 
\end{lstlisting}
