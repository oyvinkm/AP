-- Skeleton file for Boa Parser.
{-
E = Expr

-- Oper --
E :== E Oper E

-- Non Left Recursive --
E :== T Eopt
Eopt :== '==' T Eopt | '!='T Eopt | '<' T Eopt | '<=' T Eopt | '>' T Eopt | '>=' T Eopt | 'in' T Eopt | 'not' 'in' T Eopt | e 
T :== F Topt
Topt :== '+' F Topt | '-' F Topt | e
F :== X Fopt
Fopt :== '*' X Fopt | '//' X Fopt | '%' X Fopt | e
X :== numConst | stringConst | ident | '(' E ')'
-}

module BoaParser (ParseError, parseString) where
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
import BoaAST
-- add any other other imports you need

type Parser a = ReadP a

type ParseError = String -- you may replace this
 
pExpr :: Parser Exp
pExpr = do t1 <- pTerm; pExprOpt t1

pExprOpt :: Exp -> Parser Exp
pExprOpt t1 = do eo <- getExprOpt; t2 <- pTerm; return (eo t1 t2) -- Might not work, not sure what we are returning 
              <|> return t1

getExprOpt :: Parser (Exp -> Exp -> Exp)
getExprOpt = do symbol "=="; return $ (\e1 e2 -> Oper Eq e1 e2)
            <|> do symbol "!="; return $ (\e1 e2 -> Not $ Oper Eq e1 e2)
            <|> do symbol "<"; return $ (\e1 e2 -> Oper Less e1 e2)
            <|> do symbol "<="; return $ (\e1 e2 -> Not $ Oper Greater e1 e2)
            <|> do symbol ">"; return $ (\e1 e2 -> Oper Greater e1 e2)
            <|> do symbol ">="; return $ (\e1 e2 -> Not $ Oper Less e1 e2)
            <|> do symbol "in"; return $ (\e1 e2 -> Oper In e1 e2)
            -- <|> do symbol "not in"; return $ (\e1 e2 -> Not $ Oper In e1 e2)

pTerm :: Parser Exp
pTerm = do f1 <- pFactor; pTermOpt f1

pTermOpt :: Exp -> Parser Exp
pTermOpt f1 = do to <- getTermOpt; f2 <- pFactor; pTermOpt (to f1 f2)
              <|> return f1

getTermOpt :: Parser (Exp -> Exp -> Exp)
getTermOpt = do symbol "+"; return $ (\e1 e2 -> Oper Plus e1 e2)
             <|> do symbol "e"; return $ (\e1 e2 -> Oper Minus e1 e2)

pFactor :: Parser Exp
pFactor = do x1 <- pX; pFactorOpt x1

pFactorOpt :: Exp -> Parser Exp 
pFactorOpt x1 = do fo <- getFactorOpt; x2 <- pX; pFactorOpt (fo x1 x2)
              <|> return x1

getFactorOpt :: Parser (Exp -> Exp -> Exp)
getFactorOpt = do symbol "*"; return $ (\e1 e2 -> Oper Times e1 e2)
             <|> do symbol "//"; return $ (\e1 e2 -> Oper Div e1 e2)
             <|> do symbol "%"; return $ (\e1 e2 -> Oper Mod e1 e2)

pX :: Parser Exp
pX = do n <- pNum; return $ Const $ IntVal n
    <|> do s <- pString; return $ Const $ StringVal s
    <|> do i <- pIdent; return $ Var i
    <|> lexeme (do na <- string "not";whitespace; e1 <- pExpr; return $ Not e1)
-- Not canot parse expressions except constants: e.g. "not x < 5"

-- Needs to check for None, True, False, for, if, in & not
pIdent :: ReadP String
pIdent = lexeme $ do fst <- (satisfy (\char -> isLetter char || char == '_'))
                     snd <- many (satisfy (\char -> isDigit char || isLetter char || char == '_')); return $ [fst] ++ snd 

-- Only matches ' 
isQuote :: Char -> Bool
isQuote char =
  any (char ==) "'"

-- Does not escape/newline or anything
pString :: Parser String
pString = lexeme $ do _ <- char '\''; s <- manyTill(satisfy isAscii) (satisfy isQuote); return $ s


-- Not done can't properly handle 0000
pNum :: Parser Int
pNum = do _ <- count 2 (satisfy (\char -> char == '0')); return $ 100
      <|> lexeme (do _ <- char '-'; ds <- many1 (satisfy isDigit); return $ negate $ read ds)
      <|> lexeme (do ds <- many1 (satisfy isDigit); return $ read ds)
    
whitespace :: Parser ()
whitespace =
    do _ <- skipSpaces; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

{- parseString :: String -> Either ParseError Program
parseString = undefined  -- define this -}

parseString :: String -> Either ParseError Exp
parseString s = case readP_to_S (do whitespace; e <- pExpr; eof; return e) s of
                    [] -> Left "Cannot parse"
                    [(e, _)] -> Right e
                    _ -> error "Grammar is ambiguous!"