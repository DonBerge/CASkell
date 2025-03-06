{
{-|
  Module: Parser
  Description: Parseo de expresiones algebraicas

  Este modulo se encarga de parsear expresiones algebraicas y convertirlas al tipo 'Expr'.

  El parseo se realiza con la siguiente gramatica abstracta:

  @
    Expression : Expression '+' Expression
               | Expression '-' Expression
               | Expression '*' Expression
               | Expression '/' Expression
               | Expression '**' Expression
               | Expression '^' Expression 
               | Expression '^^' Expression
               | '(' Expression ')'         
               | Number                     
               | Symbol                    
               | Symbol '(' Arguments ')' 

    Arguments  : Expression 
               | Expression ',' Arguments
  @
-}
module Expr.Parser (
  parseExpr
) where

import Expr.ExprType
import Expr.Structure
import Expr.PrettyPrint -- Usado por los testcase

import Data.Number
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
  Number { TokenNumber $$ }
  Symbol { TokenSymbol $$ }

%left '+' '-'
%left '*' '/'
%right '**' '^' '^^'
%%

Expression :   Expression '+' Expression  { $1 + $3 }
           |   Expression '-' Expression  { $1 - $3 }
           |   Expression '*' Expression  { $1 * $3 }
           |   Expression '/' Expression  { $1 / $3 }
           |   Expression '**' Expression { $1 ** $3 }
           |   Expression '^' Expression  { $1 ** $3 }
           |   Expression '^^' Expression { $1 ** $3 }
           |   '(' Expression ')'         { $2 }
           |   Number                     { fromNumber $1 }
           |   Symbol                     { mkSymbol $1 } -- Si se trata de un simbolo, dejar el nombre como esta
           |   Symbol '(' Arguments ')'   { mkFun $1 $3 }
           |   error                      { undefinedExpr "Error de parseo" }

Arguments :    error                      { [undefinedExpr "Error de parseo"] }  -- Si hay un error al parsear los argumentos, se le pasa un argumento Undefined a la función. Autosimplificación se encarga de propagar el error.
          |    Expression                 { $1 : [] } 
          |    Expression ',' Arguments   { $1 : $3 }
               

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
  | TokenNumber Number
  | TokenSymbol String
  deriving (Show)

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : map toLower cs

mkSymbol :: String -> Expr
mkSymbol "Pi" = pi
mkSymbol name = symbol name

-- Construccion de funciones

mkFun :: String -> [Expr] -> Expr
mkFun name args 
  | lname `elem` ["sin", "cos", "tan", "exp", "log", "pi", "cot", "sec", 
      "csc", "asin", "acos", "atan", "asinh", "acosh", "atah", "sinh", "cosh", "tanh"] = function (capitalize lname) args
  | lname == "sqrt" = case args of
                        [x] -> sqrt x
                        _ -> undefinedExpr "La funcion raiz cuadrada solo toma un argumento"
  | otherwise = function name args
  where
    lname = map toLower name

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
                        in case capitalize s of
                          "Pi" -> TokenSymbol "Pi" : lexer cs'
                          _ -> TokenSymbol s : processArgs cs'
  where

    processArgs [] = []
    processArgs ('(':cs) = TokenLParen : processArgs cs
    processArgs (',':cs) = TokenComma : processArgs cs
    processArgs (')':cs) = TokenRParen : lexer cs
    processArgs cs = let
                      (arg, cs') = span (/= ',') cs
                    in lexer arg ++ processArgs cs'

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

-- | Parseo de expresiones algebraicas
--
--   La expresión se evalua a medida que se va parseando
--
-- === Ejemplos
--
-- >>> parseExpr "2 + 3"
-- 5
-- >>> parseExpr "x + x + x - y"
-- 3*x+(-1)*y
-- >>> parseExpr "f(x) + g(y) + sin(pi/2)"
-- 1+f(x)+g(y)
--
-- Los nombres de las funciones y simbolos conocidos no son sensibles a las mayusculas
--
-- >>> parseExpr "sin(pi/2) + COS(PI) + tAN (pI/4) + funCIONrARA(2.2)"
-- 1+funCIONrARA(2.2) 
parseExpr = parser . lexer

}