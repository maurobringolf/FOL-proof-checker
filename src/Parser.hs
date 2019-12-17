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
               , opStart = oneOf "-∧∨=¬<"
               , opLetter = oneOf "~&=:"
               , reservedOpNames = ["∧", "∨", "->", "¬", "=", "<->"]
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
            , commaSep = m_commaSep
            , commaSep1 = m_commaSep1
            , whiteSpace = m_whiteSpace } = makeTokenParser def

parseFormula :: Signature -> Parser Formula
parseFormula sig = buildExpressionParser formulaTable (parseOperand sig) <?> "formula"

formulaTable = [
          [ Infix (m_reservedOp "∧" >> return (\l -> \r -> And l r)) AssocRight]
        , [ Infix (m_reservedOp "∨" >> return (\l -> \r -> Or l r))  AssocRight]
        , [ Infix (m_reservedOp "->" >> return (\l -> \r -> Imp l r))  AssocRight]
        , [ Infix (m_reservedOp "<->" >> return (\l -> \r -> And (Imp l r) (Imp r l))) AssocRight]
        ]

parseOperand :: Signature -> Parser Formula
parseOperand sig = (m_parens (parseFormula sig))
               <|> try (parseEq sig)
               <|> try (parseRel sig)
               <|> (parseNot sig)
               <|> (parseFA sig)

parseRel :: Signature -> Parser Formula
parseRel sig = do r <- m_identifier
                  args <- (try (m_parens (m_commaSep (parseTerm sig))) <|> return [])
                  return $ Rel r args

parseNot :: Signature -> Parser Formula
parseNot sig = do m_symbol "¬"
                  f <- parseOperand sig
                  return $ Not f

parseFA :: Signature -> Parser Formula
parseFA sig = do m_symbol "∀"
                 x <- m_identifier
                 f <- m_parens (parseFormula sig)
                 return $ FA x f

parseEq :: Signature -> Parser Formula
parseEq sig = do t1 <- parseTerm sig
                 m_symbol "="
                 t2 <- parseTerm sig
                 return $ Rel "=" [t1, t2]

parseTerm :: Signature -> Parser Term
parseTerm sig = parseExp sig

parseExp :: Signature -> Parser Term
parseExp sig = buildExpressionParser (map (\f -> [ Infix (m_symbol f >> return (\l -> \r -> FApp f [l, r])) AssocRight] ) (binary_functions sig)) (parseAtom sig) <?> "term"

parseAtom :: Signature -> Parser Term
parseAtom sig = m_parens (parseTerm sig)
            <|> try (parseFApp sig)
            <|> parseVarConst sig

parseFApp :: Signature -> Parser Term
parseFApp sig = do f <- m_identifier
                   args <- m_parens $ m_commaSep1 (parseTerm sig)
                   return $ FApp f args

parseVarConst :: Signature -> Parser Term
parseVarConst sig = let isConst c = c `elem` (constants sig)
                        varConst s = if isConst s then Const s else Var s
                    in
                    do s <- m_identifier
                       return $ varConst s

parseDetailedSignature :: Parser Signature
parseDetailedSignature = do m_symbol "constants:"
                            cs <- m_commaSep m_identifier
                            -- TODO Do we need functions and relations?
                            return $ sig_empty { constants = cs }

parseSignature :: Parser Signature
parseSignature = try parseDetailedSignature
             <|> return sig_empty

parseContext :: Signature -> Parser Context
parseContext sig = many (parseFormula sig)

parseProof :: Signature -> Parser Proof
parseProof sig = do fs <- many (parseFormula sig)
                    return $ reverse fs

parseTheory :: Parser (Signature, Context)
parseTheory = m_symbol "#PA" >> return (sig_PA, ctxt_PA)
                 

parsePreamble :: Parser (Signature, Context)
parsePreamble = parseTheory
            <|> (do sig <- parseSignature
                    ctxt <- parseContext sig
                    return (sig, ctxt))

parseProofText :: Parser (Signature, Context, Proof)
parseProofText = do (sig, ctxt) <- parsePreamble
                    ctxt' <- parseContext sig
                    m_symbol "|-"
                    proof <- parseProof sig
                    eof
                    return (sig, ctxt ++ ctxt', proof)

parse :: String -> (Signature, Context, Proof)
parse text = case Text.Parsec.parse parseProofText "" text of
  Left err    -> error $ show err
  Right proof -> proof

