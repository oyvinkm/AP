-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
import BoaAST
import Control.Monad
-- add any other other imports you need

type Parser a = ReadP a

type ParseError = String -- you may replace this
reserved :: [String]
reserved = ["None", "True", "False", "for", "if", "in", "not"]

keyword :: String -> Parser ()
keyword s = lexeme $ do s' <- munch isAlphaNum 
                        Control.Monad.unless (s' == s) pfail

pProgram :: Parser [Stmt]
pProgram = do pStmts

pStmts :: Parser [Stmt]
pStmts = do sm <- pStmt; return [sm]
            <|> do sm <- pStmt
                   symbol ";"
                   sms <- pStmts
                   return (sm: sms)


pStmt :: Parser Stmt
pStmt = do var <- pIdent; symbol "="; e <- pExpr; return $ SDef var e
       <|> do e <- pExpr; return $ SExp e
 
pExpr :: Parser Exp
pExpr = do pExprNot

pExprNot :: Parser Exp
pExprNot = do keyword "not"; Not <$> pExpr
          <|> do t1 <- pTerm; pExprOpt t1 

pExprOpt :: Exp -> Parser Exp
pExprOpt t1 = do eo <- getExprOpt; t2 <- pTerm; return (eo t1 t2)
              <|> return t1

getExprOpt :: Parser (Exp -> Exp -> Exp)
getExprOpt = do symbol "=="; return $ Oper Eq
            <|> do symbol "<"; return $ Oper Less
            <|> do symbol ">"; return $ Oper Greater
            <|> do keyword "in"; return $ Oper In
            <|> do symbol "<="; return (\e1 e2 -> Not $ Oper Greater e1 e2)
            <|> do symbol ">="; return (\e1 e2 -> Not $ Oper Less e1 e2)
            <|> do symbol "!="; return (\e1 e2 -> Not $ Oper Eq e1 e2)
            <|> do keyword "not"; do keyword "in"
                   return (\e1 e2 -> Not $ Oper In e1 e2)

pTerm :: Parser Exp
pTerm = do f1 <- pFactor; pTermOpt f1

pTermOpt :: Exp -> Parser Exp
pTermOpt f1 = do to <- getTermOpt; f2 <- pFactor; pTermOpt (to f1 f2)
              <|> return f1

getTermOpt :: Parser (Exp -> Exp -> Exp)
getTermOpt = do symbol "+"; return $ Oper Plus
             <|> do symbol "-"; return $ Oper Minus

pFactor :: Parser Exp
pFactor = do x1 <- pX; pFactorOpt x1

pFactorOpt :: Exp -> Parser Exp 
pFactorOpt x1 = do fo <- getFactorOpt; x2 <- pX; pFactorOpt (fo x1 x2)
              <|> return x1

getFactorOpt :: Parser (Exp -> Exp -> Exp)
getFactorOpt = do symbol "*"; return $ Oper Times
             <|> do symbol "//"; return $ Oper Div
             <|> do symbol "%"; return $ Oper Mod


pX :: Parser Exp
pX = lexeme (do n <- pNum; return $ Const $ IntVal n)
    <|> lexeme (do symbol "\'"; do s <- pString; symbol "\'" 
                                   return $ Const $ StringVal s)
    <|> lexeme (do pAtom)
    <|> lexeme (do i <- pIdent; return $ Var i)
    <|> do i <- pIdent; symbol "("; ez <- pExprz; symbol ")"
           return $ Call i ez
    <|> do symbol "("; e <- pExpr; symbol ")"; return e
    <|> do symbol "["; e <- pExpr; c <- pCCFor; cs <- pClausez; symbol "]"
           return $ Compr e (c:cs)
    <|> do symbol "[";ez <- pExprz; symbol "]"; return $ List ez


pAtom :: Parser Exp
pAtom = do keyword "True"; return $ Const TrueVal
        <|> do keyword "False"; return $ Const FalseVal
        <|> do keyword "None"; return $ Const NoneVal


pIdent :: Parser String
pIdent = lexeme $ do c <- satisfy(\c -> isAlpha c || c == '_')
                     cs <- many (satisfy (\c -> isAlphaNum c || c == '_'))
                     let i = c:cs
                     if i `notElem` reserved then return i
                     else do pfail


pStringAgain :: Parser String
pStringAgain = do symbol "\n";s <- pString; return $ "\n" ++ s
         <|> do symbol "\\"; s <- pString; return $ "\\" ++ s
         <|> do symbol "\'"; s <- pString; return $ "'" ++ s


pString :: Parser String
pString = do s <- many(satisfy isAscii)
             x <- many pStringAgain
             return $ s ++ concat x


pNum :: Parser Int
pNum = do _ <- char '0' 
          many1 (satisfy isDigit)
          pfail
      <|> lexeme (do _ <- symbol "-" -- handles negative numbers
                     d <- satisfy (\char -> char >= '1' && char <= '9')
                     ds <- many (satisfy isDigit) 
                     return $ negate $ read $ d : ds)
      <|> lexeme (do d <- (satisfy (\char -> char >= '1' && char <= '9'))
                     ds <- many (satisfy isDigit)
                     return $ read $ d : ds)
      <|> lexeme (do _ <- skipMany (satisfy (== '-')); d <- string "0"
                     return $ read d)


pClausez :: Parser [CClause]
pClausez = do return []
          <|> do ccFor <- pCCFor; cs <- pClausez; return (ccFor : cs)
          <|> do ccIf <- pCCIf; cs <- pClausez; return (ccIf : cs)

pCCFor :: Parser CClause
pCCFor = lexeme $ do _ <- keyword "for"
                     var <- pIdent 
                     _ <- keyword "in"
                     e <- pExpr
                     return $ CCFor var e

pCCIf :: Parser CClause
pCCIf = lexeme $ do _ <- keyword "if"
                    e <- pExpr
                    return $ CCIf e


pExprz :: Parser [Exp]
pExprz = do return []
            <|> do pExprs

-- Containing 1 or more Exps.
pExprs :: Parser [Exp]
pExprs = do e <- pExpr
            symbol ","
            es <- pExprs
            return (e:es)
            <|> do e1 <- pExpr; return [e1]

---------------HELPER FUNCTIONS--------------
whitespace :: Parser ()
whitespace =
    do _ <- skipSpaces; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

parseString :: String -> Either ParseError Program
parseString s = case readP_to_S (do whitespace; p <- pProgram; eof; 
                                    return p) s of
                    [] -> Left "Cannot parse"
                    [(p, _)] -> Right p
                    _ -> error "Grammar is ambiguous!"

