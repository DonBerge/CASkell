-- filepath: /home/don-berge/Documentos/ALP-TP-FINAL/src/Parser/Parser.y
{
module Parser where

import Expr
import Structure

import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List (intercalate)
import Data.Char

}

%name parser
%tokentype { Token }
%error { parseError }

%token
  '+'    { TokenPlus }
  '-'    { TokenMinus }
  '*'    { TokenTimes }
  '/'    { TokenDiv }
  '('    { TokenLParen }
  ')'    { TokenRParen }
  ','    { TokenComma }
  '**'   { TokenPow }
  '^'    { TokenPow }
  '^^'   { TokenPow }
  number { TokenNumber $$ }
  symbol { TokenSymbol $$ }

%left '+' '-'
%left '*' '/'
%right '**' '^' '^^'
%%

Expression :   Expression '+' Expression  { $1 + $3 }
             | Expression '-' Expression  { $1 - $3 }
             | Expression '*' Expression  { $1 * $3 }
             | Expression '/' Expression  { $1 / $3 }
             | Expression '**' Expression { $1 ** $3 }
             | Expression '^' Expression  { $1 ** $3 }
             | Expression '^^' Expression { $1 ** $3 }
             | '(' Expression ')'         { $2 }
             | number                     { $1 }
             | symbol                     { symbol $1 } -- Si se trata de un simbolo, dejar el nombre como esta
             | symbol Arguments           { mkFun $1 $2 }--construct (Fun (mkFunName $1) $2) } 

Arguments : '(' CommaArguments ')' { $2 }

CommaArguments : Expression { $1 :| [] } 
               | Expression ',' CommaArguments { $1 <| $3 }
               

{

data Token
  = TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenPow
  | TokenLParen
  | TokenRParen
  | TokenComma
  | TokenNumber Expr
  | TokenSymbol String
  deriving (Show)

-- Construccion de funciones

mkFun :: String -> NonEmpty Expr -> Expr
mkFun name args 
  | lname `elem` ["sin", "cos", "tan", "exp", "log", "pi", "cot", "sec", 
      "csc", "asin", "acos", "atan", "asinh", "acosh", "atah", "sinh", "cosh", "tanh"] = construct $ Fun (capitalize lname) args
  | lname == "sqrt" = case args of
                        (x :| []) -> sqrt x
                        _ -> fail "La funcion raiz cuadrada solo toma un argumento"
  | otherwise = construct $ Fun name args
-- = let
                  --  lname = map toLower name
                  --in mkFun' lname
  where
    lname = map toLower name
    
    capitalize [] = []
    capitalize (c:cs) = toUpper c : cs

parseError :: [Token] -> a
parseError _ = error "Parse error"

----------------------------------

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexSymbolOrFunction (c:cs)
  | isDigit c = lexNumber (c:cs)
lexer ('*':'*':cs) = TokenPow : lexer cs
lexer ('^':cs) = TokenPow : lexer cs
lexer ('^':'^':cs) = TokenPow : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs

lexSymbolOrFunction cs = let
                           (s, cs') = span isAlpha cs
                         in TokenSymbol s : processArgs cs'
  where

    processArgs [] = []
    processArgs ('(':cs) = TokenLParen : processArgs cs
    processArgs (',':cs) = TokenComma : processArgs cs
    processArgs (')':cs) = TokenRParen : lexer cs
    processArgs cs = let
                      (arg, cs') = span (/= ',') cs
                    in lexer arg ++ processArgs cs'
    -- lexFunction cs = processArgs args : TokenRParen : lexer cs
    -- 
    -- processArgs [] = []
    -- processArgs (',',cs) = TokenComma : splitComma cs
    -- processArgs cs = let
    --                   (arg, cs') = span (/= ',') cs
    --                 in lexer arg ++ splitComma cs'

lexNumber cs = let
                (is, cs') = span isDigit cs -- Leer parte entera
                (ds, cs'') = if not (null cs') && head cs' == '.' -- Leer parte decimal
                              then span isDigit (tail cs')
                              else ("", cs')

                -- Juntar ambas partes
                stringNumber = case (is,ds) of
                                ("", xs) -> "0." ++ xs
                                (xs, "") -> xs
                                (xs, ys) -> xs ++ "." ++ ys

                -- Convertir el valor leido a un double y despues convertirlo a una expresion
                parsedNumber = (read stringNumber) :: Double
                
               in TokenNumber (realToFrac parsedNumber) : lexer cs''


parseExpr = parser . lexer

}