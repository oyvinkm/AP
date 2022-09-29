-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
import BoaAST
-- add any other other imports you need

type Parser a = ReadP a

type ParseError = String -- you may replace this


pProgram :: Parser [Stmt]
pProgram = do p <- pStmts; return p

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
             <|> do symbol "-"; return $ (\e1 e2 -> Oper Minus e1 e2)

pFactor :: Parser Exp
pFactor = do x1 <- pX; pFactorOpt x1

pFactorOpt :: Exp -> Parser Exp 
pFactorOpt x1 = do fo <- getFactorOpt; x2 <- pX; pFactorOpt (fo x1 x2)
              <|> return x1

getFactorOpt :: Parser (Exp -> Exp -> Exp)
getFactorOpt = do symbol "*"; return $ (\e1 e2 -> Oper Times e1 e2)
             <|> do symbol "//"; return $ (\e1 e2 -> Oper Div e1 e2)
             <|> do symbol "%"; return $ (\e1 e2 -> Oper Mod e1 e2)



-- questions
-- pX: Not cannot parse expressions except constants: e.g. "not x < 5", however "not (x < 5) works"
-- pIdent: Needs to check for None, True, False, for, if, in & not, how do we fail?
-- pString: Does not escape/newline or anything


------ pFuncs --------

pX :: Parser Exp
pX = do n <- pNum; return $ Const $ IntVal n
    <|> do s <- pString; return $ Const $ StringVal s
    <|> do i <- pIdent; return $ Var i
    <|> do bool <- pBool; return bool
    <|> lexeme (do _ <- string "not";whitespace; e1 <- pExpr; return $ Not e1)
    <|> do symbol "("; e <- pExpr; symbol ")"; return e
    <|> do compr <- pComprExp; return compr
    -- <|> do symbol "["
    --        ez <- (\e -> pExprz e)
    --        symbol "]"; return ez
    -- <|> do symbol "["
    --        e <- pExpr
    --        for <- pCClause


    -- <|> 
-- Not canot parse expressions except constants: e.g. "not x < 5", however "not (x < 5) works"

pBool :: Parser Exp
pBool = do _ <- string "True"; return $ Const TrueVal
        <|> do _ <- string "False"; return $ Const FalseVal
        <|> do _ <- string "None"; return $ Const NoneVal

-- Needs to check for None, True, False, for, if, in & not
pIdent :: Parser String
pIdent = lexeme ( do c <- (satisfy (\char -> isLetter char || char == '_'))
                     s <- many (satisfy (\char -> isDigit char || isLetter char || char == '_')); return $ [c] ++ s )
                  -- <|> do s <- keywords keywordz; err <- pfail; return err

keywordz = ["None", "True", "False", "for", "if", "in", "not"]

-- Does not escape/newline or anything
pString :: Parser String
pString = lexeme ( do _ <- char '\''; s <- manyTill(satisfy isAscii) (satisfy isQuote); return $ s )


pNum :: Parser Int
pNum = do _ <- char '0'; many1 (satisfy (isDigit)); err <- pfail; return err -- should just fail
      <|> lexeme (do _ <- char '-' -- handles negative
                     d <- (satisfy (\char -> char >= '1' && char <= '9'))
                     ds <- many (satisfy isDigit) 
                     return $ negate $ read $ [d] ++ ds)
      <|> lexeme (do d <- (satisfy (\char -> char >= '1' && char <= '9')) -- handles 0 < 
                     ds <- many (satisfy isDigit)
                     return $ read $ [d] ++ ds)
      <|> lexeme (do d <- string "0"; return $ read $ d)
      -- <|> lexeme (do _)
pComprExp :: Parser Exp
pComprExp = lexeme (do symbol "["
                       e1 <- pExpr
                       cclauses <- many pCClause
                       symbol "]"
                       return $ Compr e1 cclauses)


pCClause :: Parser CClause
pCClause = do _ <- string "for"
              var <- pIdent 
              e <- pExpr
              return $ CCFor var e
        <|> do _ <- string "if";
                e <- pExpr;
                return $ CCIf e
                    -- <|> do _ 




-- Containin 0 or more Exps. We're having a hard time figuring out what type we should return
-- and how we do lists
pExprz :: [Exp] -> Parser [Exp]
pExprz es = do e1 <- pExprs; return e1
            <|> return es

-- Containing 1 or more Exps.
pExprs :: Parser [Exp]
pExprs = do e <- pExpr
            symbol ","
            es <- pExprs
            return (e: es)
            <|> do e1 <- pExpr; return [e1]


-- pStmts :: Parser [Stmt]
-- pStmts = do sm <- pStmt; return [sm]
--             <|> do sm <- pStmt
--                    symbol ";"
--                    sms <- pStmts
--                    return (sm: sms)

---------------HELPER FUNCTIONS--------------
whitespace :: Parser ()
whitespace =
    do _ <- skipSpaces; return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

-- Only matches ' 
isQuote :: Char -> Bool
isQuote char = 
  any (char ==) "'"
-----PARSE------

{- parseString :: String -> Either ParseError Program
parseString = undefined  -- define this -}
parseString :: String -> Either ParseError Program
parseString s = case readP_to_S (do whitespace; p <- pProgram; eof; return p) s of
                    [] -> Left "Cannot parse"
                    [(p, _)] -> Right p
                    _ -> error "Grammar is ambiguous!"


-- parseString :: String -> Either ParseError Exp
-- parseString s = case readP_to_S (do whitespace; e <- pExpr; eof; return e) s of
--                     [] -> Left "Cannot parse"
--                     [(e, _)] -> Right e
--                     _ -> error "Grammar is ambiguous!"