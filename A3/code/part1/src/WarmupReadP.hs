module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E ::= T Eopt | "-" T Eopt
--   Eopt ::= "+" T Eopt | "-" T Eopt | eps
--   T ::= num | "(" E ")" 


import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
    deriving (Eq, Show)



pExp :: ReadP Exp
pExp = do e1 <- pTerm; pEopt e1 
       <|> do symbol "-"; e1 <- pTerm; pEopt (Negate e1)

pTerm :: ReadP Exp
pTerm = do n <- pNum; return $ Num n
        <|> do symbol "("; e <- pExp; symbol ")"; return e

pEopt :: Exp -> ReadP Exp
pEopt e1 = do ao <- pAddOp; e2 <- pTerm ; pEopt (ao e1 e2)
            <|> return e1


whitespace :: ReadP ()
whitespace =
    do _ <- skipSpaces; return ()

lexeme :: ReadP a -> ReadP a
lexeme p = do a <- p; whitespace; return a

symbol :: String -> ReadP ()
symbol s = lexeme $ do string s; return ()

-- digit :: ReadP Char
-- digit = satisfy (\char -> char >= '0' && char <= '9')

pNum :: ReadP Int
pNum = lexeme $ do ds <- many1 (satisfy isDigit); return $ read ds -- Import from Data.Char

pAddOp :: ReadP (Exp -> Exp -> Exp)
pAddOp = do symbol "+"; return Add
         <|> do symbol "-"; return (\e1 e2 -> Add e1 (Negate e2))

-- parseString :: String -> Either ParseError Exp
-- parseString s = case get (do whitespace; a <- get; eof; return a) s of
--                     [] -> Left "Cannot parse"
--                     [(a, _)] -> Right a
--                     _ -> error "Grammar is ambiguous!"

parseString :: String -> Either ParseError Exp
parseString s = case readP_to_S (do whitespace; e <- pExp; eof; return e) s of
                    [] -> Left "Cannot parse"
                    [(e, _)] -> Right e
                    _ -> error "Grammar is ambiguous!"

