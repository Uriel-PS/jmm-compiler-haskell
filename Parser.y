{
module Parser where

import Lex
import Token
import AST
}

%name parserLinguagem
%tokentype { Token }
%error { parseError }

%token
  'int'         { TokenInt }
  'double'      { TokenDouble }
  'string'      { TokenString }
  'void'        { TokenVoid }
  'if'          { TokenIf }
  'else'        { TokenElse }
  'while'       { TokenWhile }
  'return'      { TokenReturn }
  'print'       { TokenPrint }
  'read'        { TokenRead }
  '('           { TokenAbrePar }
  ')'           { TokenFechaPar }
  '{'           { TokenAbreChave }
  '}'           { TokenFechaChave }
  ','           { TokenVirgula }
  ';'           { TokenPontoVirgula }
  '='           { TokenAtrib }
  '+'           { TokenMais }
  '-'           { TokenMenos }
  '*'           { TokenVezes }
  '/'           { TokenDiv }
  '<'           { TokenLt }
  '>'           { TokenGt }
  '<='          { TokenLe }
  '>='          { TokenGe }
  '=='          { TokenEq }
  '/='          { TokenNe }
  '&&'          { TokenAnd }
  '||'          { TokenOr }
  '!'           { TokenNot }
  id            { TokenId $$ }
  cInt          { TokenCInt $$ }
  cDouble       { TokenCDouble $$ }
  litString     { TokenLitString $$ }

-- Precedência e Associatividade para eliminar conflitos/ambiguidade
%left '||' '&&'
%left '+' '-'
%left '*' '/'
%right '!'

%%

-- NaoTerminal : Regra { Ação em Haskell }
-- $1, $2 variaveis posicionais, de acordo com cada regra

Programa : ListaFuncoes BlocoPrincipal { Prog (map fst $1) (map snd $1) (fst $2) (snd $2) }
         | BlocoPrincipal              { Prog [] [] (fst $1) (snd $1) }

ListaFuncoes : ListaFuncoes Funcao { $1 ++ [$2] }
             | Funcao              { [$1] }

Funcao : TipoRet id '(' ParamFormais ')' BlocoPrincipal { ($2 :->: ($4, $1), ($2, $4 ++ fst $6, snd $6)) } -- Note que ele junta (++) os parâmetros formais ($4) com as variáveis locais (fst $6), pois para a JVM, ambos viram variáveis locais na memória
       | TipoRet id '(' ')' BlocoPrincipal              { ($2 :->: ([], $1), ($2, fst $5, snd $5)) }

TipoRet : Tipo   { $1 }
        | 'void' { TVoid }

ParamFormais : ParamFormais ',' ParamFormal { $1 ++ [$3] }
             | ParamFormal                  { [$1] }

ParamFormal : Tipo id { $2 :#: ($1, 0) }

BlocoPrincipal : '{' Declaracoes ListaCmd '}' { ($2, $3) }
               | '{' ListaCmd '}'             { ([], $2) }

Declaracoes : Declaracoes Declaracao { $1 ++ $2 }
            | Declaracao             { $1 }

Declaracao : Tipo ListaId ';' { map (\nome -> nome :#: ($1, 0)) $2 }

Tipo : 'int'    { TInt }
     | 'double' { TDouble }
     | 'string' { TString }

ListaId : ListaId ',' id { $1 ++ [$3] }
        | id             { [$1] }

Bloco : '{' ListaCmd '}' { $2 } -- Diferente do BlocoPrincipal, blocos de um if ou while não permitem declarar variáveis novas. Ele só extrai a lista de comandos ($2).

ListaCmd : ListaCmd Comando { $1 ++ [$2] } 
         | Comando          { [$1] }

Comando : CmdSe       { $1 }
        | CmdEnquanto { $1 }
        | CmdAtrib    { $1 }
        | CmdEscrita  { $1 }
        | CmdLeitura  { $1 }
        | Retorno     { $1 }
        | ChamadaProc { $1 }

CmdSe : 'if' '(' ExpressaoLogica ')' Bloco                       { If $3 $5 [] }
      | 'if' '(' ExpressaoLogica ')' Bloco 'else' Bloco          { If $3 $5 $7 }

CmdEnquanto : 'while' '(' ExpressaoLogica ')' Bloco { While $3 $5 }

CmdAtrib : id '=' ExpressaoAritmetica ';' { Atrib $1 $3 }
         | id '=' litString ';'           { Atrib $1 (Lit $3) }

CmdEscrita : 'print' '(' ExpressaoAritmetica ')' ';' { Imp $3 }
           | 'print' '(' litString ')' ';'           { Imp (Lit $3) }

CmdLeitura : 'read' '(' id ')' ';' { Leitura $3 }

Retorno : 'return' ExpressaoAritmetica ';' { Ret (Just $2) }
        | 'return' litString ';'           { Ret (Just (Lit $2)) }
        | 'return' ';'                     { Ret Nothing }

-- para converter o tipo de chamada para proc 
ChamadaProc : ChamadaFunc ';' { let (Chamada n p) = $1 in Proc n p } -- Chamada de função, comando solto.

ExpressaoLogica : ExpressaoLogica '&&' ExpressaoLogica{ And $1 $3 }
                | ExpressaoLogica '||' ExpressaoLogica { Or $1 $3 }
                | '!' ExpressaoLogica                  { Not $2 }
                | ExpressaoRelacional                  { Rel $1 }
                | '(' ExpressaoLogica ')'              { $2 }

ExpressaoRelacional : ExpressaoAritmetica '==' ExpressaoAritmetica { Req $1 $3 }
                    | ExpressaoAritmetica '/=' ExpressaoAritmetica { Rdif $1 $3 }
                    | ExpressaoAritmetica '<' ExpressaoAritmetica  { Rlt $1 $3 }
                    | ExpressaoAritmetica '>' ExpressaoAritmetica  { Rgt $1 $3 }
                    | ExpressaoAritmetica '<=' ExpressaoAritmetica { Rle $1 $3 }
                    | ExpressaoAritmetica '>=' ExpressaoAritmetica { Rge $1 $3 }

-- TRANSFORMAR ISSO EM FATOR, TERM, E EXPR
ExpressaoAritmetica : ExpressaoAritmetica '+' ExpressaoAritmetica { Add $1 $3 }
                    | ExpressaoAritmetica '-' ExpressaoAritmetica { Sub $1 $3 }
                    | ExpressaoAritmetica '*' ExpressaoAritmetica { Mul $1 $3 }
                    | ExpressaoAritmetica '/' ExpressaoAritmetica { Div $1 $3 }
                    | '-' ExpressaoAritmetica %prec '!'           { Neg $2 } -- maior precedencia
                    | cInt                                        { Const (CInt $1) }
                    | cDouble                                     { Const (CDouble $1) }
                    | id                                          { IdVar $1 }
                    | ChamadaFunc                                 { $1 }
                    | '(' ExpressaoAritmetica ')'                 { $2 }

ChamadaFunc : id '(' ParamReais ')' { Chamada $1 $3 } -- chamado de função, no meio de conta
            | id '(' ')'            { Chamada $1 [] }

ParamReais : ParamReais ',' ExpressaoAritmetica { $1 ++ [$3] }
           | ParamReais ',' litString           { $1 ++ [Lit $3] }
           | ExpressaoAritmetica                { [$1] }
           | litString                          { [Lit $1] }

{
parseError :: [Token] -> a
parseError s = error ("Erro de sintaxe: " ++ show (take 5 s))

testParser :: IO ()
testParser = do
    conteudo <- getContents
    let tokens = alexScanTokens conteudo
    print (parserLinguagem tokens)
}


-- O Método Bottom-Up (Shift-Reduce) e LALR(1) O seu parser usa uma técnica chamada Bottom-Up (De baixo para cima)
-- Ele lê os Tokens um por um da esquerda para a direita e os empilha (ação de Shift). 
-- Quando ele percebe que o topo da pilha forma exatamente o lado direito de uma regra gramatical (um handle),
--  ele os retira da pilha e os substitui pelo Não-Terminal correspondente (ação de Reduce)