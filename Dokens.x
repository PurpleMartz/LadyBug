{
module Dokens (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+               ;
  "(::)".*              ;
  given                 { tok (\p s -> TokenGiven p) }
  take                  { tok (\p s -> TokenTake p) }
  such                  { tok (\p s -> TokenSuch p) }
  that                  { tok (\p s -> TokenThat p) }
  and                   { tok (\p s -> TokenAnd p) }
  obey                  { tok (\p s -> TokenObey p) }
  $digit+               { tok (\p s -> TokenInt p (read s)) }
  \=                    { tok (\p s -> TokenEq p) }
  \(                    { tok (\p s -> TokenLPar p) }
  \)                    { tok (\p s -> TokenRPar p) }
  \,                    { tok (\p s -> TokenComma p) }
  $alpha [$alpha $digit \_]*     { tok (\p s -> TokenVar p s) }
  $alpha [$alpha $digit \_]* \. csv { tok (\p s -> TokenFile p s) }
{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:
tok f p s = f p s
-- The token type:
data Token =
    TokenGiven AlexPosn      |
    TokenTake  AlexPosn      |
    TokenSuch AlexPosn       |
    TokenThat AlexPosn       |
    TokenAnd AlexPosn        |
    TokenObey AlexPosn       |
    TokenEq AlexPosn         |
    TokenLPar AlexPosn       |
    TokenRPar AlexPosn       |
    TokenComma AlexPosn      |
    TokenVar AlexPosn String |
    TokenFile AlexPosn String|
    TokenInt AlexPosn Int
    deriving (Eq,Show)

token_posn (TokenGiven p) = p
token_posn (TokenTake p) = p
token_posn (TokenSuch p) = p
token_posn (TokenThat p) = p
token_posn (TokenAnd p) = p
token_posn (TokenObey p) = p
token_posn (TokenEq p) = p
token_posn (TokenLPar p) = p
token_posn (TokenRPar p) = p
token_posn (TokenComma p) = p
token_posn (TokenVar p _) = p
token_posn (TokenFile p _) = p
token_posn (TokenInt p _) = p
}
