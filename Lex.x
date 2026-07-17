{
module Lex where

import Token
}

%wrapper "basic"

-- macros 
$digito = 0-9
@num = $digito+ \. $digito+

$letras = [a-zA-Z]
$espaco = [\ \t\n\r]
@literal = \" [^\"]* \"
@identificador = $letras [$letras $digito]*

tokens :-

  -- Ignorar espaços em branco e quebras de linha
  $espaco+                      ;

  "int"                         { \s -> TokenInt }
  "double"                      { \s -> TokenDouble }
  "string"                      { \s -> TokenString }
  "void"                        { \s -> TokenVoid }
  "if"                          { \s -> TokenIf }
  "else"                        { \s -> TokenElse }
  "while"                       { \s -> TokenWhile }
  "return"                      { \s -> TokenReturn }
  "print"                       { \s -> TokenPrint }
  "read"                        { \s -> TokenRead }

  "("                           { \s -> TokenAbrePar }
  ")"                           { \s -> TokenFechaPar }
  "{"                           { \s -> TokenAbreChave }
  "}"                           { \s -> TokenFechaChave }
  ","                           { \s -> TokenVirgula }
  ";"                           { \s -> TokenPontoVirgula }

  -- O motor do Alex aplica a regra do "maior casamento" (longest match), 
  -- garantindo que "==" seja capturado aqui antes de tentar capturar "=" isolado.
  -- Ele verifica sempre o de maior tamanho antes
  "=="                          { \s -> TokenEq }
  "/="                          { \s -> TokenNe }
  "<="                          { \s -> TokenLe }
  ">="                          { \s -> TokenGe }
  "&&"                          { \s -> TokenAnd }
  "||"                          { \s -> TokenOr }
  "="                           { \s -> TokenAtrib }
  "<"                           { \s -> TokenLt }
  ">"                           { \s -> TokenGt }
  "!"                           { \s -> TokenNot }

  -- Operadores Simples e de Atribuição 
  "+"                           { \s -> TokenMais }
  "-"                           { \s -> TokenMenos }
  "*"                           { \s -> TokenVezes }
  "/"                           { \s -> TokenDiv }

  -- Literais e Identificadores
  -- Ponto flutuante
  @num                          { \s -> TokenCDouble (read s) }
  
  -- Inteiros 
  $digito+                      { \s -> TokenCInt (read s) }
  
  -- Strings literais (entre aspas duplas)
  @literal                { \s -> TokenLitString (init (tail s)) }
  
  -- Identificadores (Variáveis e nomes de funções). Começam com letra e seguem com letras ou números.
  @identificador { \s -> TokenId s }

{
-- Testar
testLex :: IO ()
testLex = do
    conteudo <- getContents
    print (alexScanTokens conteudo)
}