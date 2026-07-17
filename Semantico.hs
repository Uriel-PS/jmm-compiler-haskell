module Semantico where

import Lex (alexScanTokens)
import Parser (parserLinguagem)
import AST

-- Convertendo para string, só para colocar nas mensagens de erro
mostrarTipo :: Tipo -> String
mostrarTipo TInt    = "int"
mostrarTipo TDouble = "double"
mostrarTipo TString = "string"
mostrarTipo TVoid   = "void"

-- ====================|
-- Monada do Professor:

data Result a = Result (Bool, String, a) deriving Show 

instance Functor Result where
    fmap f (Result (b, s, a)) = Result (b, s, f a)

instance Applicative Result where
    pure a = Result (False, "", a)
    Result (b1, s1, f) <*> Result (b2, s2, x) = Result (b1 || b2, s1 ++ s2, f x)

instance Monad Result where
    Result (b, s, a) >>= f = let Result (b', s', a') = f a 
                             in Result (b || b', s ++ s', a')

errorMsg :: String -> Result ()
errorMsg s = Result (True, "Erro: " ++ s ++ "\n", ()) 

warningMsg :: String -> Result ()
warningMsg s = Result (False, "Advertencia: " ++ s ++ "\n", ()) 


-- ====================|
-- Tabela de Símbolos: O que usaremos para guardar tudo que foi declarado (Funções e Variáveis) OBS, isso já está empacotado no Prog!!!!!
-- Tabela local: as variaveis que foram declaradas em uma função especifica, que está sendo analisada no momento
-- Tabela Global: As funções que foram declaradas em geral, seu tipo de retorno e o tipo dos parametros

buscarTabelaLocal :: [Var] -> String -> Maybe Tipo 
buscarTabelaLocal [] _ = Nothing 
buscarTabelaLocal ((nomeDaVez :#: (tipoDaVez, _)): restoDaLista ) nomeProcurado
                | nomeDaVez == nomeProcurado = Just tipoDaVez
                | otherwise                  = buscarTabelaLocal restoDaLista nomeProcurado 

buscarTabelaGlobal :: [Funcao] -> String -> Maybe ([Var], Tipo)
buscarTabelaGlobal [] _ = Nothing 
buscarTabelaGlobal ((nomeDaVez :->: (parametros, tipoRetorno)) : restoDaLista) nomeProcurado
                | nomeDaVez == nomeProcurado = Just (parametros, tipoRetorno)
                | otherwise                  = buscarTabelaGlobal restoDaLista nomeProcurado

-- ====================|
-- Verificação de Expressões e Tipos

verificarExpr :: [Funcao] -> [Var] -> String -> Expr -> Result (Tipo, Expr) 

verificarExpr tabelaGlobal tabelaLocal nomeFuncao (Const (CInt numero)) =
    return (TInt, Const (CInt numero))

verificarExpr tabelaGlobal tabelaLocal nomeFuncao (Const (CDouble numero)) =
    return (TDouble, Const (CDouble numero))

verificarExpr tabelaGlobal tabelaLocal nomeFuncao (Lit stringliteral) =
    return (TString, Lit stringliteral)

verificarExpr tabelaGlobal tabelaLocal nomeFuncao (IdVar nomeProcurado) =
    case buscarTabelaLocal tabelaLocal nomeProcurado of
        
        Just tipoEncontrado -> return (tipoEncontrado, IdVar nomeProcurado)
        
        Nothing -> do
            errorMsg ("Na funcao '" ++ nomeFuncao ++ "': A variavel '" ++ nomeProcurado ++ "' nao foi declarada!") 
            -- Retorna TVoid para o compilador não travar e conseguir procurar outros erros no resto do código
            return (TVoid, IdVar nomeProcurado) 

verificarExpr tg tl nf (Add esq dir) = verificarOpBinaria tg tl nf "Add" Add esq dir
verificarExpr tg tl nf (Sub esq dir) = verificarOpBinaria tg tl nf "Sub" Sub esq dir
verificarExpr tg tl nf (Mul esq dir) = verificarOpBinaria tg tl nf "Mul" Mul esq dir
verificarExpr tg tl nf (Div esq dir) = verificarOpBinaria tg tl nf "Div" Div esq dir

-- Expressão Aritmética Sinal Negativo (Neg)
verificarExpr tabelaGlobal tabelaLocal nomeFuncao (Neg expr) = do
    (tipo, novaExprAST) <- verificarExpr tabelaGlobal tabelaLocal nomeFuncao expr
    if tipo == TVoid then
        return (TVoid, Neg novaExprAST)
    else if tipo == TInt then
        return (TInt, Neg novaExprAST)
    else if tipo == TDouble then
        return (TDouble, Neg novaExprAST)
    else do
        errorMsg ("Na funcao '" ++ nomeFuncao ++ "': Operador unario (-) usado com tipo invalido (String).")
        return (TVoid, Neg novaExprAST)

-- Chamada de Função
verificarExpr tabelaGlobal tabelaLocal nomeFuncaoAtual (Chamada nomeFuncChamada paramsPassados) = do
    case buscarTabelaGlobal tabelaGlobal nomeFuncChamada of
        Nothing -> do
            errorMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Chamada a funcao nao declarada '" ++ nomeFuncChamada ++ "'.")
            return (TVoid, Chamada nomeFuncChamada paramsPassados)

        Just (paramsFormais, tipoRetorno) -> do
            
            if length paramsFormais /= length paramsPassados then do
                errorMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Quantidade errada de parametros ao chamar '" ++ nomeFuncChamada ++ "'.")
                return (tipoRetorno, Chamada nomeFuncChamada paramsPassados)
                
            else do
                novosParamsAST <- verificarListaParametros tabelaGlobal tabelaLocal nomeFuncaoAtual nomeFuncChamada paramsFormais paramsPassados
                return (tipoRetorno, Chamada nomeFuncChamada novosParamsAST)

-- Expressões Aritméticas
verificarOpBinaria :: [Funcao] -> [Var] -> String
                   -> String                       -- nome do operador (para a mensagem de erro)
                   -> (Expr -> Expr -> Expr)        -- construtor Add, Sub, Mul ou Div e operandos
                   -> Expr -> Expr                  -- lado esq e dir 
                   -> Result (Tipo, Expr)           -- tipo + AST
verificarOpBinaria tabelaGlobal tabelaLocal nomeFuncao nomeOp construtor esq dir = do
    (tipoEsq, novaEsqAST) <- verificarExpr tabelaGlobal tabelaLocal nomeFuncao esq
    (tipoDir, novaDirAST) <- verificarExpr tabelaGlobal tabelaLocal nomeFuncao dir
    if tipoEsq == TVoid || tipoDir == TVoid then
        return (TVoid, construtor novaEsqAST novaDirAST)
    else if tipoEsq == TInt && tipoDir == TInt then
        return (TInt, construtor novaEsqAST novaDirAST)
    else if tipoEsq == TDouble && tipoDir == TDouble then
        return (TDouble, construtor novaEsqAST novaDirAST)
    else if tipoEsq == TInt && tipoDir == TDouble then
        return (TDouble, construtor (IntDouble novaEsqAST) novaDirAST)
    else if tipoEsq == TDouble && tipoDir == TInt then
        return (TDouble, construtor novaEsqAST (IntDouble novaDirAST))
    else do
        errorMsg ("Na funcao '" ++ nomeFuncao ++ "': Tipos invalidos na operacao de " ++ nomeOp ++ ", ao ser aplicada entre" ++ mostrarTipo tipoEsq ++ " e " ++ mostrarTipo tipoDir ++ ".")
        return (TVoid, construtor novaEsqAST novaDirAST)

-- FUNÇÃO AUXILIAR: para analisar os tipos dos parametros 
verificarListaParametros :: [Funcao] -> [Var] -> String -> String -> [Var] -> [Expr] -> Result [Expr]
verificarListaParametros tabelaGlobal tabelaLocal nomeFuncaoAtual nomeFuncChamada [] [] = 
    return []
verificarListaParametros tabelaGlobal tabelaLocal nomeFuncaoAtual nomeFuncChamada ((nomeVar :#: (tipoEsperado, _)) : restoFormais) (paramPassado : restoPassados) = do
    (tipoPassado, novoParamAST) <- verificarExpr tabelaGlobal tabelaLocal nomeFuncaoAtual paramPassado -- extraio o tipo real de paramPassado
    paramComCastAST <- 
        if tipoPassado == TVoid then
            return novoParamAST
        else if tipoEsperado == tipoPassado then
            return novoParamAST
        else if tipoEsperado == TDouble && tipoPassado == TInt then --do
            --warningMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Conversao (Int para Double) no parametro de '" ++ nomeFuncChamada ++ "'.")
            return (IntDouble novoParamAST)
        else if tipoEsperado == TInt && tipoPassado == TDouble then do
            warningMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Conversao (Double para Int) no parametro '" ++ nomeVar ++ "' com valor '" ++ mostrarTipo tipoPassado ++ "' passado para '" ++ nomeFuncChamada ++ "' que deveria ser '" ++ mostrarTipo tipoEsperado ++ "'.")
            return (DoubleInt novoParamAST)
        else do
            errorMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Tipo incompativel no parametro de '" ++ nomeFuncChamada ++ "'.")
            return novoParamAST
    restoValidadoAST <- verificarListaParametros tabelaGlobal tabelaLocal nomeFuncaoAtual nomeFuncChamada restoFormais restoPassados
    return (paramComCastAST : restoValidadoAST)

-- ====================|
-- Expressões Relacionais
verificarExprR :: [Funcao] -> [Var] -> String -> ExprR -> Result ExprR

verificarOpRelacional :: [Funcao] -> [Var] -> String
                      -> String
                      -> (Expr -> Expr -> ExprR)   -- construtor relacional dessa vez. devolve ExprR 
                      -> Expr -> Expr
                      -> Result ExprR
verificarOpRelacional tabelaGlobal tabelaLocal nomeFuncao nomeOp construtor esq dir = do
    (tipoEsq, novaEsqAST) <- verificarExpr tabelaGlobal tabelaLocal nomeFuncao esq
    (tipoDir, novaDirAST) <- verificarExpr tabelaGlobal tabelaLocal nomeFuncao dir
    if tipoEsq == TVoid || tipoDir == TVoid then
        return (construtor novaEsqAST novaDirAST)
    else if tipoEsq == TString && tipoDir == TString then
        return (construtor novaEsqAST novaDirAST)
    else if tipoEsq == TInt && tipoDir == TInt then
        return (construtor novaEsqAST novaDirAST)
    else if tipoEsq == TDouble && tipoDir == TDouble then
        return (construtor novaEsqAST novaDirAST)
    else if tipoEsq == TInt && tipoDir == TDouble then
        return (construtor (IntDouble novaEsqAST) novaDirAST)
    else if tipoEsq == TDouble && tipoDir == TInt then
        return (construtor novaEsqAST (IntDouble novaDirAST))
    else do
        errorMsg ("Na funcao '" ++ nomeFuncao ++ "': Tipos incompativeis na operacao relacional (" ++ nomeOp ++ ") entre '" ++ mostrarTipo tipoEsq ++ "' e '" ++ mostrarTipo tipoDir ++ "'." )
        return (construtor novaEsqAST novaDirAST)

verificarExprR tg tl nomeFuncao (Req  e d) = verificarOpRelacional tg tl nomeFuncao "=="  Req  e d
verificarExprR tg tl nomeFuncao (Rdif e d) = verificarOpRelacional tg tl nomeFuncao "/="  Rdif e d
verificarExprR tg tl nomeFuncao (Rlt  e d) = verificarOpRelacional tg tl nomeFuncao "<"   Rlt  e d
verificarExprR tg tl nomeFuncao (Rgt  e d) = verificarOpRelacional tg tl nomeFuncao ">"   Rgt  e d
verificarExprR tg tl nomeFuncao (Rle  e d) = verificarOpRelacional tg tl nomeFuncao "<="  Rle  e d
verificarExprR tg tl nomeFuncao (Rge  e d) = verificarOpRelacional tg tl nomeFuncao ">="  Rge  e d

-- ====================|
-- EXPRESSÕES LÓGICAS
verificarExprL :: [Funcao] -> [Var] -> String -> ExprL -> Result ExprL
verificarExprL tabelaGlobal tabelaLocal nomeFuncaoAtual (Rel exprRelacional) = do
    novaExprRAST <- verificarExprR tabelaGlobal tabelaLocal nomeFuncaoAtual exprRelacional
    return (Rel novaExprRAST)
-- Operador '&&' (And)
-- recursão para o lado esquerdo e para o lado direito
verificarExprL tabelaGlobal tabelaLocal nomeFuncaoAtual (And esq dir) = do
    novaEsqAST <- verificarExprL tabelaGlobal tabelaLocal nomeFuncaoAtual esq
    novaDirAST <- verificarExprL tabelaGlobal tabelaLocal nomeFuncaoAtual dir
    return (And novaEsqAST novaDirAST)
-- Operador '||' (Or)
verificarExprL tabelaGlobal tabelaLocal nomeFuncaoAtual (Or esq dir) = do
    novaEsqAST <- verificarExprL tabelaGlobal tabelaLocal nomeFuncaoAtual esq
    novaDirAST <- verificarExprL tabelaGlobal tabelaLocal nomeFuncaoAtual dir
    return (Or novaEsqAST novaDirAST)
-- Operador Unário '!' (Not)
-- Exemplo: !(x == 5)
verificarExprL tabelaGlobal tabelaLocal nomeFuncaoAtual (Not expr) = do
    novaExprAST <- verificarExprL tabelaGlobal tabelaLocal nomeFuncaoAtual expr
    return (Not novaExprAST)

-- ====================|
-- Comandos
verificarComando :: [Funcao] -> [Var] -> Tipo -> String -> Comando -> Result Comando
verificarComando tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual (Atrib nomeVariavel exprDireita) = do
    (tipoDir, novaExprAST) <- verificarExpr tabelaGlobal tabelaLocal nomeFuncaoAtual exprDireita
    
    case buscarTabelaLocal tabelaLocal nomeVariavel of
        Nothing -> do
            errorMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Variavel '" ++ nomeVariavel ++ "' nao declarada para atribuicao.")
            return (Atrib nomeVariavel novaExprAST)
            
        Just tipoEsq -> do
            if tipoDir == TVoid then
                return (Atrib nomeVariavel novaExprAST)
                
            else if tipoEsq == tipoDir then
                return (Atrib nomeVariavel novaExprAST)
                
            else if tipoEsq == TDouble && tipoDir == TInt then --do 
                --warningMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Conversao (Int para Double) ao atribuir valor para '" ++ nomeVariavel ++ "'.")
                return (Atrib nomeVariavel (IntDouble novaExprAST))
                
            else if tipoEsq == TInt && tipoDir == TDouble then do
                warningMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Conversao (Double para Int) ao atribuir valor '" ++ mostrarTipo tipoDir ++ "' para variável '" ++ nomeVariavel ++ "' que é '" ++ mostrarTipo tipoEsq ++ "'.")
                return (Atrib nomeVariavel (DoubleInt novaExprAST))
                
            else do
                errorMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Tipos incompativeis ao atribuir valor para variável '" ++ nomeVariavel ++ "'.")
                return (Atrib nomeVariavel novaExprAST)

-- If (Se), usamos ExprL, mas precisamos verificar os BLOCOS também
verificarComando tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual (If condicao blocoVerdadeiro blocoFalso) = do
    
    novaCondicaoAST <- verificarExprL tabelaGlobal tabelaLocal nomeFuncaoAtual condicao
    novoBlocoVerdadeiro <- verificarBloco tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual blocoVerdadeiro
    novoBlocoFalso <- verificarBloco tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual blocoFalso
    return (If novaCondicaoAST novoBlocoVerdadeiro novoBlocoFalso)

-- While (Enquanto), mesma coisa
verificarComando tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual (While condicao bloco) = do
    novaCondicaoAST <- verificarExprL tabelaGlobal tabelaLocal nomeFuncaoAtual condicao
    novoBloco <- verificarBloco tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual bloco
    return (While novaCondicaoAST novoBloco)

-- Retorno (Ret)
-- O comando é apenas "return;" vazio (Nothing)
verificarComando tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual (Ret Nothing) = do
    -- Se a função é Void, sem problemas
    if tipoRetFuncao == TVoid then
        return (Ret Nothing)
    -- Se a função esperava um tipo, dar return vazio é erro
    else do
        errorMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Retorno vazio ('return;') em funcao que exige um valor de retorno.")
        return (Ret Nothing)

-- O comando é "return expressao;" (Just expr)
verificarComando tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual (Ret (Just expr)) = do
    -- verificar conta matemática do que está sendo retornado
    (tipoExpr, novaExprAST) <- verificarExpr tabelaGlobal tabelaLocal nomeFuncaoAtual expr
    -- Se a conta já deu erro antes, apenas repassa a AST
    if tipoExpr == TVoid then
        return (Ret (Just novaExprAST))
    else if tipoRetFuncao == tipoExpr then
        return (Ret (Just novaExprAST))
    else if tipoRetFuncao == TDouble && tipoExpr == TInt then --do
        -- warningMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Conversao (Int para Double) no retorno da funcao.")
        return (Ret (Just (IntDouble novaExprAST)))
    else if tipoRetFuncao == TInt && tipoExpr == TDouble then do
        warningMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Conversao (Double para Int) no retorno: valor '" ++ mostrarTipo tipoExpr ++ "' convertido para '" ++ mostrarTipo tipoRetFuncao ++ "'.")
        return (Ret (Just (DoubleInt novaExprAST)))
    else do
        errorMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Tipo de retorno incompativel: funcao retorna '" ++ mostrarTipo tipoRetFuncao ++ "' mas recebeu '" ++ mostrarTipo tipoExpr ++ "'.")
        return (Ret (Just novaExprAST))


-- Escrita (Imp) Ex: print(x + 2)
verificarComando tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual (Imp expr) = do
    -- O 'print' não exige um tipo específico, ele só precisa que a expressão lá dentro seja válida 
    (tipoExpr, novaExprAST) <- verificarExpr tabelaGlobal tabelaLocal nomeFuncaoAtual expr
    return (Imp novaExprAST)

-- Leitura (Leitura) Ex: read(x)
verificarComando tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual (Leitura nomeVariavel) = do
    --verificar se a variavel que vai receber o valor existe:
    case buscarTabelaLocal tabelaLocal nomeVariavel of
        Nothing -> do
            errorMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Variavel '" ++ nomeVariavel ++ "' nao declarada para leitura.")
            return (Leitura nomeVariavel)
        Just _ -> 
            return (Leitura nomeVariavel)

-- Chamada de Procedimento (Proc)
verificarComando tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual (Proc nomeFuncChamada paramsPassados) = do
    case buscarTabelaGlobal tabelaGlobal nomeFuncChamada of
        Nothing -> do
            errorMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Chamada a procedimento nao declarado '" ++ nomeFuncChamada ++ "'.")
            return (Proc nomeFuncChamada paramsPassados)
            
        Just (paramsFormais, tipoRetorno) -> do
            if length paramsFormais /= length paramsPassados then do
                errorMsg ("Na funcao '" ++ nomeFuncaoAtual ++ "': Quantidade errada de parametros ao chamar '" ++ nomeFuncChamada ++ "'.")
                return (Proc nomeFuncChamada paramsPassados)
            else do
                novosParamsAST <- verificarListaParametros tabelaGlobal tabelaLocal nomeFuncaoAtual nomeFuncChamada paramsFormais paramsPassados
                return (Proc nomeFuncChamada novosParamsAST)


-- FUNÇÃO AUXILIAR, verificar uma lista de comandos (Bloco, se levarmos em conta a AST)
verificarBloco :: [Funcao] -> [Var] -> Tipo -> String -> [Comando] -> Result [Comando]
verificarBloco tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual [] = 
    return []

verificarBloco tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual (comandoDaVez : restoDoBloco) = do
    
    novoComandoAST <- verificarComando tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual comandoDaVez
    restoValidadoAST <- verificarBloco tabelaGlobal tabelaLocal tipoRetFuncao nomeFuncaoAtual restoDoBloco
    return (novoComandoAST : restoValidadoAST)

verificarFuncao :: [Funcao] -> (Id, [Var], Bloco) -> Result (Id, [Var], Bloco)
verificarFuncao tabelaGlobal (nomeFuncao, varsLocais, bloco) = do
    
    case buscarTabelaGlobal tabelaGlobal nomeFuncao of
        
        Nothing -> do
            errorMsg ("Funcao '" ++ nomeFuncao ++ "' nao foi encontrada na tabela global!")
            return (nomeFuncao, varsLocais, bloco)

        Just (paramsFormais, tipoRetorno) -> do
            let tabelaLocalCompleta = varsLocais

            checarVariaveisDuplicadas tabelaLocalCompleta nomeFuncao
            
            novoBlocoAST <- verificarBloco tabelaGlobal tabelaLocalCompleta tipoRetorno nomeFuncao bloco
            
            return (nomeFuncao, varsLocais, novoBlocoAST)

-- FUNÇÃO AUXILIAR: Varre a lista de Funções do programa
verificarListaFuncoes :: [Funcao] -> [(Id, [Var], Bloco)] -> Result [(Id, [Var], Bloco)]
verificarListaFuncoes tabelaGlobal [] = return []
verificarListaFuncoes tabelaGlobal (funcDaVez : restoFuncoes) = do
    novaFuncAST <- verificarFuncao tabelaGlobal funcDaVez
    restoValidadoAST <- verificarListaFuncoes tabelaGlobal restoFuncoes
    return (novaFuncAST : restoValidadoAST)

verificarPrograma :: Programa -> Result Programa
verificarPrograma (Prog tabelaGlobal listaFuncoes varsMain blocoMain) = do
    checarFuncoesDuplicadas tabelaGlobal
    checarVariaveisDuplicadas varsMain "Main"

    novasFuncoesAST <- verificarListaFuncoes tabelaGlobal listaFuncoes
    novoBlocoMainAST <- verificarBloco tabelaGlobal varsMain TVoid "Main" blocoMain

    return (Prog tabelaGlobal novasFuncoesAST varsMain novoBlocoMainAST)

checarFuncoesDuplicadas :: [Funcao] -> Result ()
checarFuncoesDuplicadas [] = return () 
checarFuncoesDuplicadas ((nome :->: _) : resto) = do
    case buscarTabelaGlobal resto nome of
        Just _  -> errorMsg ("Funcao '" ++ nome ++ "' foi declarada multiplas vezes.")
        Nothing -> return ()
    checarFuncoesDuplicadas resto

checarVariaveisDuplicadas :: [Var] -> String -> Result ()
checarVariaveisDuplicadas [] _ = return ()
checarVariaveisDuplicadas ((nome :#: _) : resto) nomeFuncao = do
    case buscarTabelaLocal resto nome of
        Just _  -> errorMsg ("Na funcao '" ++ nomeFuncao ++ "': Variavel '" ++ nome ++ "' declarada multiplas vezes.")
        Nothing -> return ()
    checarVariaveisDuplicadas resto nomeFuncao


testSemantico :: IO ()
testSemantico = do
    conteudo <- getContents
    let tokens = alexScanTokens conteudo
    let ast = parserLinguagem tokens
    print (verificarPrograma ast)