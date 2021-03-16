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
import AstUtils(substF, forall)
import Context
import Data.Char(isSymbol)
import qualified Theory.PA

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

parseProof :: Signature -> [Formula] -> Parser (Proof, Context)
parseProof sig fs = (eof >> return (fs, []))
                    <|> (do (f, sig', ctxt) <- parseProofStep sig
                            (gs, ctxt') <- parseProof sig' (f:fs)
                            return (gs, ctxt ++ ctxt'))

parseProofStep :: Signature -> Parser (Formula, Signature, Context)
parseProofStep sig = parseCommand sig
                     <|> (do f <- parseFormula sig
                             return (f,sig, []))

parseCommand :: Signature -> Parser (Formula, Signature, Context)
parseCommand sig = try (parseConstDef sig)
               <|> parseFunDef sig

parseConstDef :: Signature -> Parser (Formula, Signature, Context)
parseConstDef sig = do m_symbol "#define"
                       c <- m_identifier 
                       m_symbol "as"
                       x <- m_identifier
                       m_symbol "in"
                       theta_c <- parseFormula sig
                       return (constDefProofObligation x theta_c, sig { Sig.constants = c : Sig.constants sig }, [Literal $ constDefinition c x theta_c])

constDefProofObligation :: Symbol -> Formula -> Formula
constDefProofObligation x f = funDefProofObligation [] x f

constDefinition :: Symbol -> Symbol -> Formula -> Formula
constDefinition c x f = substF x (Const c) f

parseFunDef :: Signature -> Parser (Formula, Signature, Context)
parseFunDef sig = do m_symbol "#define"
                     f <- m_identifier
                     xs <- m_parens (m_commaSep1 m_identifier)
                     m_symbol "as"
                     y <- m_identifier
                     m_symbol "in"
                     theta_f <- parseFormula sig
                     return (funDefProofObligation xs y theta_f, sig { Sig.functions = (f, length xs) : Sig.functions sig }, [])

funDefProofObligation :: [Symbol] -> Symbol -> Formula -> Formula
funDefProofObligation xs y f = let y' = y ++ "_"
                               in
                               forall xs (EX y (And f (FA y' (Imp (substF y (Var y') f) (Rel "=" [Var y, Var y'])))))

parseTheory :: Parser (Signature, Context)
parseTheory = try (m_symbol "#PA" >> return (Sig.pa, Theory.PA.axioms))

parsePreamble :: Parser (Signature, Context)
parsePreamble = parseTheory
            <|> (do sig <- parseSignature
                    ctxt <- parseContext sig
                    return (sig, ctxt))

parseProofText :: Parser (Signature, Context, Proof)
parseProofText = do (sig, ctxt) <- parsePreamble
                    ctxt' <- parseContext sig
                    m_symbol "|-"
                    (proof, ctxt'') <- parseProof sig []
                    return (sig, ctxt ++ ctxt' ++ ctxt'', proof)

parse :: String -> (Signature, Context, Proof)
parse text = case Text.Parsec.parse parseProofText "" text of
  Left err    -> error $ show err
  Right proof -> proof

