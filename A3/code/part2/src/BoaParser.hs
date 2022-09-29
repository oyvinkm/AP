-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
import BoaAST
-- add any other other imports you need

type Parser a = ReadP a

type ParseError = String -- you may replace this
reserved :: [String]
reserved = ["None", "True", "False", "for", "if", "in", "not"]
keyword :: String -> Parser ()
keyword s = lexeme $ do s' <- many1 (satisfy isAlphaNum)
                        if s' == s then return ()
                        else return pfail $ "expected " ++ s


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
pExpr = do e <- pExprNot; return e

pExprNot :: Parser Exp
pExprNot = do keyword "not"; Not <$> pExpr
          <|> do t1 <- pTerm; pExprOpt t1 

pExprOpt :: Exp -> Parser Exp
pExprOpt t1 = lexeme(do eo <- getExprOpt; t2 <- pTerm; return (eo t1 t2)) -- Might not work, not sure what we are returning 
              <|> return t1

getExprOpt :: Parser (Exp -> Exp -> Exp)
getExprOpt = do symbol "=="; return $ (\e1 e2 -> Oper Eq e1 e2)
            <|> do symbol "!="; return $ (\e1 e2 -> Not $ Oper Eq e1 e2)
            <|> do symbol "<"; return $ (\e1 e2 -> Oper Less e1 e2)
            <|> do symbol "<="; return $ (\e1 e2 -> Not $ Oper Greater e1 e2)
            <|> do symbol ">"; return $ (\e1 e2 -> Oper Greater e1 e2)
            <|> do symbol ">="; return $ (\e1 e2 -> Not $ Oper Less e1 e2)
            -- <|> do symbol "not"; keyword "in"; return $ Not (\e1 e2 -> Oper In e1 e2)
            -- <|> do symbol "not"; return $ (\e1 _ -> Not e1)
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
pX = lexeme (do n <- pNum; return $ Const $ IntVal n)
    <|> lexeme (do s <- pString; return $ Const $ StringVal s)
    <|> lexeme (do i <- pIdent; return $ Var i)
    <|> lexeme (do bool <- pAtom; return bool)
    -- <|> lexeme (do _ <- symbol "not"; e1 <- pExpr; return $ Not e1)
    <|> do i <- pIdent; symbol "("; ez <- pExprz; symbol ")"; return $ Call i ez
    <|> do symbol "("; e <- pExpr; symbol ")"; return e
    <|> do symbol "["; e <- pExpr; c <- pCCFor; cs <- pClausez; symbol "]"; return $ Compr e (c:cs)
    <|> do symbol "[";ez <- pExprz; symbol "]"; return $ List ez

    -- <|> 
-- Not canot parse expressions except constants: e.g. "not x < 5", however "not (x < 5) works"

-- Look at keywords in slides
pAtom :: Parser Exp
pAtom = do keyword "True"; return $ Const TrueVal
        <|> do keyword "False"; return $ Const FalseVal
        <|> do keyword "None"; return $ Const NoneVal

-- Needs to check for None, True, False, for, if, in & not

pIdent :: Parser String
pIdent = lexeme (do c <- (satisfy (\char -> isLetter char || char == '_'))
                    s <- many (satisfy (\char -> isDigit char || isLetter char || char == '_'))
                    case (c:s) of 
                        s -> if s `elem` reserved then do err <- pfail; return $ err
                              else return s)


-- Does not escape/newline or anything
pStringEscape :: Parser ()
pStringEscape = do symbol "\\"; satisfy(\char -> char == '\'' || char == '\\' || char == 'n'); return () 
              <|> do err <- pfail; return err
pString :: Parser String
pString = lexeme (do _ <- symbol "\'" 
                     s <- manyTill(satisfy isAscii) (pStringEscape) 
                     ss <- pString
                     symbol "\'"
                     return $ (s ++ ss))


pNum :: Parser Int
pNum = lexeme(do _ <- char '0'; many1 (satisfy (isDigit)); err <- pfail; return err) -- should just fail
      <|> lexeme (do _ <- symbol "-" -- handles negative
                     d <- (satisfy (\char -> char >= '1' && char <= '9'))
                     ds <- many (satisfy isDigit) 
                     return $ negate $ read $ [d] ++ ds)
      <|> lexeme (do d <- (satisfy (\char -> char >= '1' && char <= '9')) -- handles 0 < 
                     ds <- many (satisfy isDigit)
                     return $ read $ [d] ++ ds)
      <|> lexeme (do _ <- skipMany (satisfy (\c -> c == '-')); d <- string "0"; return $ read $ d)


pClausez :: Parser [CClause]
pClausez = do return []
          <|> do ccFor <- pCCFor; cs <- pClausez; return (ccFor : cs)
          <|> do ccIf <- pCCIf; cs <- pClausez; return (ccIf : cs)

pCCFor :: Parser CClause
pCCFor = lexeme (do _ <- symbol "for"
                    whitespace
                    var <- pIdent 
                    _ <- symbol "in"
                    whitespace
                    e <- pExpr
                    return $ CCFor var e)
pCCIf :: Parser CClause
pCCIf = lexeme (do _ <- symbol "if"
                   whitespace
                   e <- pExpr
                   return $ CCIf e)




-- Containin 0 or more Exps. We're having a hard time figuring out what type we should return
-- and how we do lists
pExprz :: Parser [Exp]
pExprz = do return []
            <|> do es <- pExprs; return es

-- Containing 1 or more Exps.
pExprs :: Parser [Exp]
pExprs = do e <- pExpr
            symbol ","
            es <- pExprs
            return (e:es)
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