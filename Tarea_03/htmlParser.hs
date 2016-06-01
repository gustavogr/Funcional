import System.Environment
import Text.ParserCombinators.Parsec

lhs :: Parser String
lhs = do  elems <- many element
          eof
          return $ concat elems

element :: Parser String
element = eol <|> code <|> try(h1) <|> try(h2) <|> paragraph 
          
h1 :: Parser String
h1 = do whitespace
        char '*'
        cont <- many $ validChar
        eol <?> "expecting end of line to finish h1 block."
        return $ "<h1>" ++ concat cont ++ "</h1>"++"\n"

h2 :: Parser String
h2 = do whitespace
        char '#'
        cont <- many $ validChar
        eol <?> "expecting end of line to finish h2 block."
        return $ "<h2>" ++ concat cont ++ "</h2>"++"\n"

code :: Parser String
code = do cont <- many1 $ codeLine
          return $ "<code>\n" ++ unlines cont ++ "</code>" ++ "\n"
          
codeLine :: Parser String
codeLine = do string "> "
              cont <- many $ validChar
              eol <?> "expecting end of line to finish codeLine." 
              return $ concat cont
              
paragraph :: Parser String
paragraph = do  cont <- many parLine
                eol <?> "expecting end of line to finish paragraph."
                return $ "<p>\n" ++ unlines cont ++ "</p>" ++ "\n"

parLine :: Parser String
parLine = do  cont <- many1 validChar
              eol <?> "expecting end of line to finish parLine."
              return $ concat cont

validChar :: Parser String
validChar = do  c <- noneOf "\n"
                case c of
                  '&' -> return "&amp;"
                  '<' -> return "&lt;"
                  '>' -> return "&gt;"
                  ' ' -> whitespace
                  otherwise -> return [c]

whitespace :: Parser String
whitespace = do many $ char ' '
                return " "

eol :: Parser String
eol = do  char '\n'
          return  ""

lhsName :: Parser String
lhsName = do  cont <- manyTill anyChar $ string ".lhs"
              eof
              return cont

parseLHS :: String -> IO ()
parseLHS file = do  input <- readFile file
                    case parse lhs file input of
                      Left e -> do  putStr "Error: "
                                    print e
                      Right r -> case parse lhsName ".html" file of
                          Left e -> do  putStr "FileNameError: "
                                        print e
                          Right s -> writeFile (s ++ ".html") r

main = do
        files <- getArgs
        mapM_ parseLHS files 
        -- print $ parse lhs "prueba" cont