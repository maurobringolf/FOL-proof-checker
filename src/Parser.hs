module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Char
import Text.Parsec.Language

import Data.Char(isUpper, isDigit)

import Signature(Signature, Symbol)
import qualified Signature as Sig
import Term
import Formula
import Proof
import AstUtils(substF)
import Context
import Data.Char(isSymbol)
import qualified Theory.GT
import qualified Theory.PA
import qualified Theory.ZF

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
               <|> try (parseInfixRel sig)
               <|> (parseNot sig)
               <|> (parseFA sig)
               <|> (parseEX sig)

parseRel :: Signature -> Parser Formula
parseRel sig = do r <- choice (map (\(r,_) -> m_symbol r) (Sig.relations sig))
                  args <- (try (m_parens (m_commaSep (parseTerm sig))) <|> return [])
                  return $ Rel r args

parseInfixRel :: Signature -> Parser Formula
parseInfixRel sig = do t1 <- parseTerm sig
                       r <- choice (map (\(r,_) -> m_symbol r) (filter (\(_,n) -> n == 2) (Sig.relations sig)))
                       t2 <- parseTerm sig
                       return $ Rel r [t1, t2]

parseNot :: Signature -> Parser Formula
parseNot sig = do m_symbol "¬"
                  f <- parseOperand sig
                  return $ Not f

parseFA :: Signature -> Parser Formula
parseFA sig = do m_symbol "∀"
                 x <- m_identifier
                 f <- m_parens (parseFormula sig)
                 return $ FA x f

parseEX :: Signature -> Parser Formula
parseEX sig = do m_symbol "∃"
                 x <- m_identifier
                 f <- m_parens (parseFormula sig)
                 return $ EX x f

parseEq :: Signature -> Parser Formula
parseEq sig = do t1 <- parseTerm sig
                 m_symbol "="
                 t2 <- parseTerm sig
                 return $ Rel "=" [t1, t2]

parseTerm :: Signature -> Parser Term
parseTerm sig = parseExp sig

parseExp :: Signature -> Parser Term
parseExp sig = buildExpressionParser (map (\f -> [ Infix (m_symbol f >> return (\l -> \r -> FApp f [l, r])) AssocRight] ) (Sig.binary_functions sig)) (parseAtom sig) <?> "term"

parseAtom :: Signature -> Parser Term
parseAtom sig = m_parens (parseTerm sig)
            <|> try (parseFApp sig)
            <|> parseVarConst sig

parseFApp :: Signature -> Parser Term
parseFApp sig = do f <- choice (map (\(f,_) -> m_symbol f) (Sig.functions sig))
                   args <- m_parens $ m_commaSep1 (parseTerm sig)
                   return $ FApp f args

parseVarConst :: Signature -> Parser Term
parseVarConst sig = let isConst c = c `elem` (Sig.constants sig)
                        varConst s = if isConst s then Const s else Var s
                    in
                    do s <- m_identifier
                       return $ varConst s

parseDetailedSignature :: Parser Signature
parseDetailedSignature = do cs <- try parseConstantSig <|> return []
                            fs <- try parseFunctionsSig <|> return []
                            rs <- try parseRelationSig <|> return []
                            return $ Sig.empty { Sig.constants = cs, Sig.relations = rs, Sig.functions = fs }

parseConstantSig :: Parser [String]
parseConstantSig = do m_symbol "#constants:"
                      cs <- m_commaSep m_identifier
                      return cs

parseFunctionsSig :: Parser [(Sig.Symbol, Int)]
parseFunctionsSig = do m_symbol "#functions:"
                       fs <-  (m_commaSep  ((do f <- m_identifier
                                                n <- m_parens m_integer
                                                return (f,fromIntegral n)) :: Parser (Sig.Symbol, Int)))
                       return fs

parseRelationSig :: Parser [(Sig.Symbol, Int)]
parseRelationSig = do m_symbol "#relations:"
                      rs <-  (m_commaSep  ((do r <- m_identifier
                                               n <- m_parens m_integer
                                               return (r,fromIntegral n)) :: Parser (Sig.Symbol, Int)))
                      return rs

parseSignature :: Parser Signature
parseSignature = try parseDetailedSignature
             <|> return Sig.empty

parseContext :: Signature -> Parser Context
parseContext sig = do axs <- many (parseFormula sig)
                      return $ map Literal axs

parseProof :: Signature -> [Formula] -> Parser Proof
parseProof sig fs = (eof >> return fs)
                    <|> (do (f, sig') <- parseProofStep sig
                            gs <- parseProof sig' (f:fs)
                            return gs)

parseProofStep :: Signature -> Parser (Formula, Signature)
parseProofStep sig = parseCommand sig
                     <|> (do f <- parseFormula sig
                             return (f,sig))

parseCommand :: Signature -> Parser (Formula, Signature)
parseCommand = parseConstDef -- <|> parseRelDef <|> parseFunDef

parseConstDef :: Signature -> Parser (Formula, Signature)
parseConstDef sig = do m_symbol "#define"
                       c <- m_identifier 
                       m_symbol "as"
                       x <- m_identifier
                       m_symbol "in"
                       f <- parseFormula sig
                       return (constDefProofObligation x f, sig { Sig.constants = c : Sig.constants sig })

constDefProofObligation :: Symbol -> Formula -> Formula
constDefProofObligation x f = let x' = x ++ "_"
                              in
                              EX x (And f (FA x' (Imp (substF x (Var x') f) (Rel "=" [Var x, Var x']))))



parseTheory :: Parser (Signature, Context)
parseTheory = do try (m_symbol "#PA" >> return (Sig.pa, Theory.PA.axioms))
                 <|> try (m_symbol "#GT" >> return (Sig.gt, Theory.GT.axioms))
                 <|> try (m_symbol "#ZF" >> return (Sig.st, Theory.ZF.axioms))

parsePreamble :: Parser (Signature, Context)
parsePreamble = parseTheory
            <|> (do sig <- parseSignature
                    ctxt <- parseContext sig
                    return (sig, ctxt))

parseProofText :: Parser (Signature, Context, Proof)
parseProofText = do (sig, ctxt) <- parsePreamble
                    ctxt' <- parseContext sig
                    m_symbol "|-"
                    proof <- parseProof sig []
                    return (sig, ctxt ++ ctxt', proof)

parse :: String -> (Signature, Context, Proof)
parse text = case Text.Parsec.parse parseProofText "" text of
  Left err    -> error $ show err
  Right proof -> proof

