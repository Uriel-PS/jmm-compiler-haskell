module Token where

data Token
  -- tipos de dados (só três) e retorno função void
  = TokenInt
  | TokenDouble
  | TokenString
  | TokenVoid

  -- palavras reservadas
  | TokenIf
  | TokenElse
  | TokenWhile
  | TokenReturn
  | TokenPrint
  | TokenRead

  -- símbolos
  | TokenAbrePar        -- (
  | TokenFechaPar       -- )
  | TokenAbreChave      -- {
  | TokenFechaChave     -- }
  | TokenVirgula        -- ,
  | TokenPontoVirgula   -- ;

  -- operadores
  | TokenAtrib          -- =
  | TokenMais           -- +
  | TokenMenos          -- -
  | TokenVezes          -- *
  | TokenDiv            -- /

  -- operadores relacionais
  | TokenLt             -- <
  | TokenGt             -- >
  | TokenLe             -- <=
  | TokenGe             -- >=
  | TokenEq             -- ==
  | TokenNe             -- /=

  -- operadores lógicos verificar se é assim 
  | TokenAnd            -- &&
  | TokenOr             -- ||
  | TokenNot            -- ! 

  --
  | TokenId String
  | TokenCInt Int
  | TokenCDouble Double
  | TokenLitString String
  deriving (Eq, Show)