module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Char
import Text.Parsec.Language

import Data.Char(isUpper, isDigit)

import Signature
import Term
import Formula
import Proof

def = emptyDef { commentStart = "--"
               , commentEnd = "\n"
               , identStart = alphaNum
               , identLetter = alphaNum
               , opStart = oneOf "-∧∨=¬"
               , opLetter = oneOf "~&=:"
               , reservedOpNames = ["∧", "∨", "->", "¬", "="]
               , reservedNames = []
               }

TokenParser { parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , integer = m_integer
            , symbol = m_symbol
            , lexeme = m_lexeme
            , semiSep1 = m_semiSep1
            , commaSep1 = m_commaSep1
            , whiteSpace = m_whiteSpace } = makeTokenParser def

parseFormula :: Parser Formula
parseFormula = buildExpressionParser formulaTable parseOperand <?> "formula"

formulaTable = [
          [ Infix (m_reservedOp "∧" >> return (\l -> \r -> And l r)) AssocRight]
        , [ Infix (m_reservedOp "∨" >> return (\l -> \r -> Or l r))  AssocRight]
        , [ Infix (m_reservedOp "->" >> return (\l -> \r -> Imp l r))  AssocRight]
        ]

parseOperand :: Parser Formula
parseOperand = parseEq

parseEq :: Parser Formula
parseEq = do t1 <- parseTerm
             m_symbol "="
             t2 <- parseTerm
             return $ Eq t1 t2

parseTerm :: Parser Term
parseTerm = m_parens parseTerm
        <|> try parseFApp
        <|> parseVarConst

parseFApp :: Parser Term
parseFApp = do f <- m_identifier
               args <- m_parens $ m_commaSep1 parseTerm
               return $ FApp f args

parseVarConst :: Parser Term
parseVarConst = let isConst c = isDigit c || isUpper c
                    varConst s = if isConst $ head s then Const s else Var s
                in
                do s <- m_identifier
                   return $ varConst s

parseSignature :: Parser Signature
parseSignature = do return sig_empty

parseContext :: Parser Context
parseContext = many parseFormula

parseProof :: Parser Proof
parseProof = do fs <- many parseFormula
                return $ reverse fs

parseProofText :: Parser (Signature, Context, Proof)
parseProofText = do sig   <- parseSignature
                    ctxt  <- parseContext
                    m_symbol "|-"
                    proof <- parseProof
                    return (sig, ctxt, proof)

parse :: String -> (Signature, Context, Proof)
parse text = case Text.Parsec.parse parseProofText "" text of
  Left err    -> error $ show err
  Right proof -> proof

