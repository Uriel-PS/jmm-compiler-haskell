module AST where 

type Id = String

data Tipo = TDouble | TInt | TString | TVoid 
  deriving (Show, Eq)

data TCons = CDouble Double | CInt Int 
  deriving (Show, Eq)

data Expr 
  = Add Expr Expr 
  | Sub Expr Expr 
  | Mul Expr Expr
  | Div Expr Expr 
  | Neg Expr 
  | Const TCons
  | IdVar String 
  | Chamada Id [Expr] -- chamada de uma função, construtor guarda o nome e uma lista de [Expr], parâmetros.
  | Lit String
  | IntDouble Expr -- CASTING
  | DoubleInt Expr -- CASTING
  deriving Show

data ExprR 
  = Req Expr Expr 
  | Rdif Expr Expr
  | Rlt Expr Expr
  | Rgt Expr Expr 
  | Rle Expr Expr
  | Rge Expr Expr
  deriving Show

data ExprL 
  = And ExprL ExprL 
  | Or ExprL ExprL 
  | Not ExprL
  | Rel ExprR 
  deriving Show

-- construtor infixo dentro dos ":"
data Var = Id :#: (Tipo, Int) -- TABELA LOCAL!
    deriving Show

-- mesma coisa
data Funcao = Id :->: ([Var], Tipo)   -- TABELA GLOBAL! 
    deriving Show

data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco
  deriving Show
  -- 4 listas: 
  -- Lista das assinaturas das funções
  -- As implementações das funções (ID=nome da função), [Var] variaveis declaradas na função, e os comandos que ela executa.
  -- Variáveis declaradas na main
  -- Comandos da main


type Bloco = [Comando]

data Comando 
  = If ExprL Bloco Bloco
  | While ExprL Bloco
  | Atrib Id Expr
  | Leitura Id
  | Imp Expr
  | Ret (Maybe Expr)
  | Proc Id [Expr]
  --  Chamada de Procedimento. 
  -- É parecido com a Chamada, mas usado quando uma função é chamada isoladamente como um comando 
  -- (ex: fazerAlgo(2);) e não no meio de uma conta (diferente de x = fazerAlgo(2) + 1;).
  deriving Show