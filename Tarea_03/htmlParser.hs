import System.Environment
import Text.ParserCombinators.Parsec

lhs :: Parser String
lhs = do  elems <- many element
          eof
          return (concat elems)

element :: Parser String
element = h1 <|> h2 <|> code -- <|> paragraph 
          
h1 :: Parser String
h1 = do char '*'
        cont <- many $ noneOf "\n"
        eol
        return ("<h1>" ++ cont ++ "</h1>"++"\n")

h2 :: Parser String
h2 = do char '#'
        cont <- many $ noneOf "\n"
        eol
        return ("<h2>" ++ cont ++ "</h2>"++"\n")

code :: Parser String
code = do cont <- many $ codeLine
          eol
          return ("<code>" ++ unlines cont ++ "</code>" ++ "\n")
          
codeLine :: Parser String
codeLine = do string "> "
              cont <- many $ noneOf "\n"
              eol 
              return cont


        
-- paragraph :: Parser String
-- paragraph = do  cont <- many noBlankLine
--                 blankLine
--                 return ("<p>" ++ unlines cont ++ "<\p>")
                
-- noBlankLine :: Parser String
-- noBlankLine = do  cont <- many $ noneOf "\n"
--                   eol

eol :: Parser Char
eol = char '\n'

main = do
        [f] <- getArgs
        cont <- readFile f 
        print $ parse lhs "prueba" cont