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

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 3 \\ Parsec}

\author{Gustavo Gutiérrez\\
11-10428\\
\href{mailto:11-10428@usb.ve}{<11-10428@usb.ve>}}

\date{Junio 1, 2016}

\maketitle

\pagebreak

\ignore{

> import System.Environment
> import Text.ParserCombinators.Parsec

}

\section{Estructura principal del Parser}

\noindent
Un archivo \texttt{lhs} está formado por una combinación de 4 elementos:
\begin{itemize}
\item Encabezados principales (H1).
\item Encabezados secundarios (H2).
\item Bloques de código.
\item Párrafos.
\end{itemize}

\noindent
Un encabezado principal es una línea cuyo primer caracter distinto a un 
espacio es \texttt{*}. El resto de la línea es tomado como su contenido
simplificando los espacios en blanco.

\noindent
Los encabezados secundarios tienen la misma estructura que los principales
con la diferencia de que el primer caracter es un \texttt{\#}.

\noindent
Los bloques de código estan formados por varias líneas de código. Cada línea
de código empieza exactamente con el string ``\texttt{>} ''. El resto de
la línea es tomado como contenido, y a diferencia de los encabezados, los
espacios en blanco se pasan sin simplificar.

\noindent
Finalmente un párrafo esta formado por una o más líneas no vacías y termina
cuando encuentre una línea en blanco.

\begin{lstlisting}

> lhs :: Parser String
> lhs = do  elems <- many element
>           eof
>           return $ concat elems

> element :: Parser String
> element = eol <|> code <|> try(h1) <|> try(h2) <|> paragraph 

> h1 :: Parser String
> h1 = do whitespaces
>         char '*'
>         cont <- many $ reducedWhites
>         eol <|> eof'
>         return $ "<h1>" ++ concat cont ++ "</h1>\n"

> h2 :: Parser String
> h2 = do whitespaces
>         char '#'
>         cont <- many $ reducedWhites
>         eol <|> eof'
>         return $ "<h2>" ++ concat cont ++ "</h2>\n"

> code :: Parser String
> code = do cont <- many1 $ codeLine
>           return $ "<code>\n" ++ unlines cont ++ "</code>\n"

> codeLine :: Parser String
> codeLine = do string "> "
>               cont <- many $ validChar
>               eol <|> eof'
>               return $ concat cont
>               
> paragraph :: Parser String
> paragraph = do  cont <- many1 parLine
>                 eol <|> eof'
>                 return $ "<p>\n" ++ unlines cont ++ "</p>\n"

> parLine :: Parser String
> parLine = do  cont <- many1 reducedWhites
>               eol <|> eof'
>               return $ concat cont

\end{lstlisting}

\section{Funciones auxiliares}

\noindent
El parser \texttt{validChar} convierte cualquier caracter a su representación
válida en HTML. El parser \texttt{whitespaces} se utiliza para tomar todos los
espacios en blanco posible y convertirlos en uno solo. 

\noindent
A partir de esto se 
forma el parser \texttt{reducedWhites} que realiza las funciones de los dos 
parsers anteriores.


\begin{lstlisting}


> validChar :: Parser String
> validChar = do  c <- noneOf "\n"
>                 case c of
>                   '&' -> return "&amp;"
>                   '<' -> return "&lt;"
>                   '>' -> return "&gt;"
>                   otherwise -> return [c]

> reducedWhites :: Parser String
> reducedWhites = do  s <- validChar
>                     case s of
>                       " " -> whitespaces
>                       "\t" -> whitespaces
>                       otherwise -> return s

> whitespaces :: Parser String
> whitespaces = do  many $ oneOf " \t"
>                   return " "

> eol :: Parser String
> eol = do  char '\n'
>           return  ""

> eof' :: Parser String
> eof' = do eof
>           return ""

\end{lstlisting}

\section{Programa principal}

\begin{lstlisting}


> lhsName :: Parser String
> lhsName = do  cont <- manyTill anyChar $ string ".lhs"
>               eof <?> "extension .lhs."
>               return cont

> parseLHS :: String -> IO ()
> parseLHS file = do  input <- readFile file
>                     case parse lhs file input of
>                       Left e -> do putStr "Error: "
>                                    print e
>                       Right r -> case parse lhsName ".html" file of
>                         Left e -> do putStr "FileNameError: "
>                                      print e
>                         Right s -> writeFile (s ++ ".html") r

> main = do files <- getArgs
>           mapM_ parseLHS files 
 
\end{lstlisting}

 \end{document}