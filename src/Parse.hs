module Parse where

import Control.Monad
import Data.Void (Void)
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parseExp :: String -> String -> Either (ParseErrorBundle String Void) Exp
parseExp = parse (pExp <* eof)

type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reserved :: Parser ()
reserved = void $ choice $ map (try . keyword) ["let", "in", "fun", "true", "false"]

keyword :: String -> Parser ()
keyword x = void $ lexeme (string x <* notFollowedBy alphaNumChar)

operator :: String -> Parser ()
operator op = void $ lexeme (string op <* notFollowedBy opLetter)
  where
    opLetter = oneOf "+-*/%=><:;|&"

lowerIdent :: Parser String
lowerIdent = label "lower ident" $
  lexeme $ do
    notFollowedBy reserved
    (:) <$> lowerChar <*> many alphaNumChar

pVar :: Parser Exp
pVar = Var <$> lowerIdent <?> "variable"

integer :: Parser Integer
integer = lexeme L.decimal

pIntC :: Parser Exp
pIntC = Const . Int <$> integer

pBoolC :: Parser Exp
pBoolC = Const . Bool <$> ((keyword "true" >> pure True) <|> (keyword "false" >> pure False))

pSingleExp :: Parser Exp
pSingleExp = pVar <|> pIntC <|> pBoolC <|> between (symbol "(") (symbol ")") pExp

pApp :: Parser Exp
pApp = foldl App <$> pSingleExp <*> some pSingleExp

pLam :: Parser Exp
pLam = label "lambda" $ do
  void $ lexeme $ string "fun" <* notFollowedBy alphaNumChar
  x <- lowerIdent
  operator "->"
  Lam x <$> pExp

pLet :: Parser Exp
pLet = label "let" $ do
  void $ lexeme $ string "let" <* notFollowedBy alphaNumChar
  x <- lowerIdent
  operator "="
  e1 <- pExp
  void $ lexeme $ string "in" <* notFollowedBy alphaNumChar
  Let x e1 <$> pExp

pExp :: Parser Exp
pExp = try pApp <|> pLam <|> pLet <|> pSingleExp
