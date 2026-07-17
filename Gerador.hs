module Gerador where

import AST
import Control.Monad.State

-- Gerador de labels unicos (State monad conforme indicado pelo professor)
novoLabel :: State Int String -- ( Tipo, String) monada que guarda o Tipo da variável e o texto Jasmin gerado)
novoLabel = do
    n <- get
    put (n + 1)
    return ("l" ++ show n)


-- Tabela local com indices de slot calculados pelo gerador (Mapeamento pros iload 1, etc)
-- IMPORTANTE: o parser coloca 0 em todos os slots (nome :#: (tipo, 0)) (de forma provisoria, agora vamos colocar o real)
-- O gerador é quem atribui os indices reais da JVM.


-- TEORIA JVM: Na Máquina Virtual Java, as variáveis locais e os parâmetros não são acessados pelo 
-- nome (como "x" ou "y"), mas sim por um índice numérico no Vetor de Variáveis Locais do Frame de execução
-- como iremos considerar indices/slots: 
--   Funcoes  : params primeiro (slot 0..), depois variaveis locais
--   Main     : slot 0 = String[] args (JVM obriga), variaveis a partir do slot 1


type TabelaLocal = [(Id, (Tipo, Int))] -- associa nome da variavel, ao seu tipo e endereço/indice na JVM

-- Quantidade de slots que um tipo ocupa (cada espaço no vetor de variaveis locais do frame
-- tem 32 bits. Ints, strings, booleanos cabem em 1 slot, mas exigem 2 slots (64 bits) (double = 2, o resto = 1)
nSlotsTipo :: Tipo -> Int
nSlotsTipo TDouble = 2
nSlotsTipo _       = 1

-- Atribui slots sequencialmente a partir de um indice inicial
atribuirSlots :: Int -> [Var] -> TabelaLocal
atribuirSlots _ [] = []
atribuirSlots slot ((nome :#: (t, _)) : resto) =
    (nome, (t, slot)) : atribuirSlots (slot + nSlotsTipo t) resto -- atribuo o endereço para cada Var, incrementando o indice
    -- empacto no formato atual da TabelaLocal

-- Total de slots necessarios (sem contar offset inicial), JASMIN precisa limite máximo de memória que a func vai usar (.limit locals n)
totalSlots :: [Var] -> Int
totalSlots = sum . map (\(_ :#: (t, _)) -> nSlotsTipo t)

-- tabela local
buscarLocal :: TabelaLocal -> Id -> Maybe (Tipo, Int)
buscarLocal tl nome = lookup nome tl 
-- lookup busca chave em listas [(chave, valor)] mesma coisa que a buscaTabelaLocal  só que dessa vez queremos o endereço


-- Tabela global (usada para descritores de chamada e tipos de retorno)
buscarGlobal :: [Funcao] -> Id -> Maybe ([Var], Tipo)
buscarGlobal [] _ = Nothing
buscarGlobal ((nome :->: info) : resto) id
    | nome == id = Just info
    | otherwise  = buscarGlobal resto id

-- =====================================================================
-- Descritores de tipo para Jasmin

descTipo :: Tipo -> String
descTipo TInt    = "I"
descTipo TDouble = "D"
descTipo TString = "Ljava/lang/String;" -- é objeto, referencia uma classe usando L seguida do caminho da classe e terminar com ;
descTipo TVoid   = "V"

-- Instrucoes de carga e armazenamento

instrLoad :: Tipo -> Int -> String
instrLoad TInt    n = "\tiload "  ++ show n ++ "\n" -- obs \t é só tabulação aqui, ja que fica na frente do label pra melhor visualização
instrLoad TDouble n = "\tdload "  ++ show n ++ "\n"
instrLoad TString n = "\taload "  ++ show n ++ "\n"
instrLoad TVoid   _ = "" -- ignora

instrStore :: Tipo -> Int -> String
instrStore TInt    n = "\tistore " ++ show n ++ "\n"
instrStore TDouble n = "\tdstore " ++ show n ++ "\n"
instrStore TString n = "\tastore " ++ show n ++ "\n"
instrStore TVoid   _ = ""

instrReturn :: Tipo -> String
instrReturn TDouble = "\tdreturn\n"
instrReturn TString = "\tareturn\n"
instrReturn TVoid   = "\treturn\n"
instrReturn _       = "\tireturn\n" -- cuidar aqui

-- =====================================================================
-- Geracao de expressoes aritmeticas → (Tipo, codigo Jasmin)

genExpr :: String -> [Funcao] -> TabelaLocal -> Expr -> State Int (Tipo, String)
-- Nome da classe/arquivo atula
-- tabela global
-- tabela local
-- expr (NÓ AST) que vamos traduzir pra bytecode

genInt :: Int -> String
genInt i
    | i == -1                   = "\ticonst_m1\n"
    | i >= 0 && i <= 5          = "\ticonst_" ++ show i ++ "\n"
    | i >= -128 && i <= 127     = "\tbipush " ++ show i ++ "\n"
    | i >= -32768 && i <= 32767 = "\tsipush " ++ show i ++ "\n"
    | otherwise                 = "\tldc " ++ show i ++ "\n"

genDouble :: Double -> String
genDouble d = "\tldc2_w " ++ formatDouble d ++ "\n"

-- Constante inteira
genExpr _ _ _ (Const (CInt i)) =
    return (TInt, genInt i) -- Load constant (ldc)

-- Constante double
genExpr _ _ _ (Const (CDouble d)) =
    return (TDouble, genDouble d) -- ldc2_w (load constante 2 words)

-- Literal string
genExpr _ _ _ (Lit s) =
    return (TString, "\tldc \"" ++ s ++ "\"\n") -- 1 word igual

-- Variavel: carrega o slot do tabelalocal
genExpr _ _ tl (IdVar nome) =
    case buscarLocal tl nome of
        Just (t, n) -> return (t, instrLoad t n)
        Nothing     -> return (TVoid, "; para nao dar erro" ++ "\n")

-- Conversao int -> double (inserida pelo analisador semantico)
genExpr c tg tl (IntDouble e) = do
    (_, code) <- genExpr c tg tl e -- ignora o tipo, ja vai ter cast
    return (TDouble, code ++ "\ti2d\n")

-- Conversao double -> int 
genExpr c tg tl (DoubleInt e) = do
    (_, code) <- genExpr c tg tl e
    return (TInt, code ++ "\td2i\n")

-- Negacao unaria
genExpr c tg tl (Neg e) = do
    (t, code) <- genExpr c tg tl e
    let instr = case t of { TDouble -> "\tdneg\n"; _ -> "\tineg\n" } -- se for double é dneg, se for qualquer coisa ineg
    return (t, code ++ instr)

-- Operacoes binarias aritmeticas
genExpr c tg tl (Add e1 e2) = genBinOp c tg tl e1 e2 "add"
genExpr c tg tl (Sub e1 e2) = genBinOp c tg tl e1 e2 "sub"
genExpr c tg tl (Mul e1 e2) = genBinOp c tg tl e1 e2 "mul"
genExpr c tg tl (Div e1 e2) = genBinOp c tg tl e1 e2 "div"

-- Chamada de funcao como expressao
genExpr c tg tl (Chamada nome args) = do -- rags parametros lista de expr
    argsCodes <- mapM (fmap snd . genExpr c tg tl) args -- mapa monadico, executa a função sobre cada item da lista, mantendo o state
    --  fmap precisa por causa da monada, snd pega só o String, isso é o bytecode de cada parametro
    let argsStr = concat argsCodes -- concatena tudo a lista de strings
    case buscarGlobal tg nome of
        Just (params, tipoRet) ->
            let paramDesc = concatMap (\(_ :#: (t, _)) -> descTipo t) params -- pega só o tipo
                retDesc   = descTipo tipoRet
                call      = "\tinvokestatic " ++ c ++ "/" ++ nome ++ -- jasmin exige o formato NomeDaClasse/NomeDaFuncao(TiposDeEntrada)TipoDeRetorno
                            "(" ++ paramDesc ++ ")" ++ retDesc ++ "\n"
            in  return (tipoRet, argsStr ++ call)
        Nothing ->
            return (TVoid, argsStr ++ "; tem que colocar pra nao dar erro" ++ "\n") -- já tratado pelo semantico

-- Operacao binaria: analisar tipo resultante e dar instrucao correta
genBinOp :: String -> [Funcao] -> TabelaLocal
         -> Expr -> Expr -> String
         -> State Int (Tipo, String)
genBinOp c tg tl e1 e2 op = do
    (t1, code1) <- genExpr c tg tl e1 -- esquerda primeiro
    (_, code2) <- genExpr c tg tl e2 -- nao precisamos verificar ja que o semantico ja fez isso!

    let prefix = case t1 of { TDouble -> "d"; _ -> "i" }
    return (t1, code1 ++ code2 ++ "\t" ++ prefix ++ op ++ "\n")

-- Formata double para que Jasmin aceite (evita notacao 'Infinity' etc.)
formatDouble :: Double -> String
formatDouble d
    | d == fromIntegral (round d :: Int) = show (round d :: Int) ++ ".0" -- força .0 no final
    | otherwise = show d

-- =====================================================================
-- Expressoes relacionais: salta para v se verdadeiro, f se falso
-- Convencao: true -> goto v  |  false -> goto f

genExprR :: String -> [Funcao] -> TabelaLocal
         -> String -> String -> ExprR
         -> State Int String
genExprR c tg tl v f exprR = do -- v = label se verdadeiro, f = label se falso
    let (tag, e1, e2) = decompoeR exprR
    (t1, code1) <- genExpr c tg tl e1
    (t2, code2) <- genExpr c tg tl e2
    let cmpCode = case (t1, t2) of
            (TString, TString) -> genCmpString tag v f
            (TDouble, _)       -> "\tdcmpg\n" ++ genCmpDouble tag v f
            (_, TDouble)       -> "\tdcmpg\n" ++ genCmpDouble tag v f
            _                  -> genCmpInt    tag v f
    return (code1 ++ code2 ++ cmpCode)

-- Decompoe o no relacional em (operador, lado esq, lado dir) e converte pra o sufixo que o jvm entende
decompoeR :: ExprR -> (String, Expr, Expr)
decompoeR (Req  e1 e2) = ("eq", e1, e2)
decompoeR (Rdif e1 e2) = ("ne", e1, e2)
decompoeR (Rlt  e1 e2) = ("lt", e1, e2)
decompoeR (Rgt  e1 e2) = ("gt", e1, e2)
decompoeR (Rle  e1 e2) = ("le", e1, e2)
decompoeR (Rge  e1 e2) = ("ge", e1, e2)

-- Comparacao entre inteiros: if_icmp<op>
genCmpInt :: String -> String -> String -> String
genCmpInt op v f =
    "\tif_icmp" ++ op ++ " " ++ v ++ "\n" ++
    "\tgoto " ++ f ++ "\n"

-- Comparacao entre doubles: dcmpg deixa -1/0/1 na pilha; depois if<op>
genCmpDouble :: String -> String -> String -> String
genCmpDouble op v f =
    "\tif" ++ op ++ " " ++ v ++ "\n" ++
    "\tgoto " ++ f ++ "\n"

-- Comparacao de strings via String.equals
genCmpString :: String -> String -> String -> String
genCmpString "eq" v f =
    "\tinvokevirtual java/lang/String/equals(Ljava/lang/Object;)Z\n" ++
    "\tifne " ++ v ++ "\n" ++
    "\tgoto " ++ f ++ "\n"
genCmpString "ne" v f =
    "\tinvokevirtual java/lang/String/equals(Ljava/lang/Object;)Z\n" ++
    "\tifeq " ++ v ++ "\n" ++
    "\tgoto " ++ f ++ "\n"
genCmpString _ v f =
    "\tgoto " ++ f ++ "\n"   -- outros relacionais em string: erro semantico ja tratou

-- =====================================================================
-- Expressoes logicas: salta para v se verdadeiro, f se falso
-- Convencao (mesma do codigo do professor):
--   true  -> goto v
--   false -> goto f


genExprL :: String -> [Funcao] -> TabelaLocal
         -> String -> String -> ExprL
         -> State Int String

-- Expressao relacional basica
genExprL c tg tl v f (Rel r) =
    genExprR c tg tl v f r

-- Negacao: inverte os rotulos
genExprL c tg tl v f (Not e) =
    genExprL c tg tl f v e

-- Conjuncao (&&) com curto-circuito:
--   se e1 falso -> goto f (curto-circuito)
--   se e1 verdadeiro -> cai em l1 → avalia e2
--   se e2 verdadeiro -> goto v
--   se e2 falso     -> goto f
genExprL c tg tl v f (And e1 e2) = do
    l1  <- novoLabel
    e1' <- genExprL c tg tl l1 f  e1
    e2' <- genExprL c tg tl v  f  e2 -- se chegou aqui, era verdade
    return (e1' ++ l1 ++ ":\n" ++ e2')

-- Disjuncao (||) com curto-circuito:
--   se e1 verdadeiro -> goto v (curto-circuito)
--   se e1 falso      -> cai em l1 → avalia e2
--   se e2 verdadeiro -> goto v
--   se e2 falso      -> goto f
genExprL c tg tl v f (Or e1 e2) = do
    l1  <- novoLabel
    e1' <- genExprL c tg tl v  l1 e1
    e2' <- genExprL c tg tl v  f  e2
    return (e1' ++ l1 ++ ":\n" ++ e2')

-- =====================================================================
-- Instrucao print (Imp)

genPrint :: Tipo -> String -> String
genPrint t exprCode =
    "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n" ++
    exprCode ++
    case t of
        TInt    -> "\tinvokevirtual java/io/PrintStream/println(I)V\n"
        TDouble -> "\tinvokevirtual java/io/PrintStream/println(D)V\n"
        TString -> "\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n"
        _       -> "; print: tipo nao suportado\n"

-- =====================================================================
-- Instrucao read (Leitura) via java.util.Scanner

genRead :: Tipo -> Int -> String
genRead t n =
    "\tnew java/util/Scanner\n" ++
    "\tdup\n" ++
    "\tgetstatic java/lang/System/in Ljava/io/InputStream;\n" ++
    "\tinvokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n" ++
    case t of
        TInt    -> "\tinvokevirtual java/util/Scanner/nextInt()I\n"
                   ++ "\tistore " ++ show n ++ "\n"
        TDouble -> "\tinvokevirtual java/util/Scanner/nextDouble()D\n"
                   ++ "\tdstore " ++ show n ++ "\n"
        TString -> "\tinvokevirtual java/util/Scanner/next()Ljava/lang/String;\n"
                   ++ "\tastore " ++ show n ++ "\n"
        _       -> "; read: tipo nao suportado\n"

-- =====================================================================
-- Geracao de comandos
genCmd :: String -> [Funcao] -> TabelaLocal -> Tipo -> Comando -> State Int String

-- Atribuicao
genCmd c tg tl _ (Atrib nome expr) = do
    (_, code) <- genExpr c tg tl expr
    case buscarLocal tl nome of
        Just (t, n) -> return (code ++ instrStore t n)
        Nothing     -> return (code ++ "; somente para nao dar erro " ++ "\n")

-- Impressao
genCmd c tg tl _ (Imp expr) = do
    (t, code) <- genExpr c tg tl expr
    return (genPrint t code)

-- Leitura
genCmd _ _ tl _ (Leitura nome) =
    case buscarLocal tl nome of
        Just (t, n) -> return (genRead t n)
        Nothing     -> return ("; somente para nao dar erro " ++ "\n")

-- Retorno sem valor
genCmd _ _ _ _ (Ret Nothing) =
    return "\treturn\n"

-- Retorno com expressao
genCmd c tg tl _ (Ret (Just expr)) = do
    (t, code) <- genExpr c tg tl expr
    return (code ++ instrReturn t)

-- Se (if sem else)
genCmd c tg tl tipoRet (If cond blocoV []) = do
    lTrue  <- novoLabel
    lFalse <- novoLabel
    lEnd   <- novoLabel
    condCode   <- genExprL c tg tl lTrue lFalse cond
    blocoVCode <- genBloco  c tg tl tipoRet blocoV
    return ( condCode ++
             lTrue  ++ ":\n" ++
             blocoVCode ++
             "\tgoto " ++ lEnd ++ "\n" ++
             lFalse ++ ":\n" ++
             lEnd   ++ ":\n" )

-- Se-senao (if-else)
genCmd c tg tl tipoRet (If cond blocoV blocoF) = do
    lTrue  <- novoLabel
    lFalse <- novoLabel
    lEnd   <- novoLabel
    condCode   <- genExprL c tg tl lTrue lFalse cond
    blocoVCode <- genBloco  c tg tl tipoRet blocoV
    blocoFCode <- genBloco  c tg tl tipoRet blocoF
    return ( condCode ++
             lTrue  ++ ":\n" ++
             blocoVCode ++
             "\tgoto " ++ lEnd ++ "\n" ++
             lFalse ++ ":\n" ++
             blocoFCode ++
             lEnd   ++ ":\n" )

-- Enquanto (while)
-- Estrutura gerada (igual ao fragmento do professor):
--   li:
--     <avaliacao da condicao>  -> goto lv (true) | goto lf (false)
--   lv:
--     <corpo do laco>
--     goto li
--   lf:
genCmd c tg tl tipoRet (While cond bloco) = do
    li <- novoLabel
    lv <- novoLabel
    lf <- novoLabel
    condCode  <- genExprL c tg tl lv lf cond
    blocoCode <- genBloco  c tg tl tipoRet bloco
    return ( li ++ ":\n" ++
             condCode ++
             lv ++ ":\n" ++
             blocoCode ++
             "\tgoto " ++ li ++ "\n" ++
             lf ++ ":\n" )

-- Chamada de procedimento (funcao usada como comando)
genCmd c tg tl _ (Proc nome args) = do
    argsCodes <- mapM (fmap snd . genExpr c tg tl) args
    let argsStr = concat argsCodes
    case buscarGlobal tg nome of
        Just (params, tipoRet) ->
            let paramDesc = concatMap (\(_ :#: (t, _)) -> descTipo t) params
                retDesc   = descTipo tipoRet
                call      = "\tinvokestatic " ++ c ++ "/" ++ nome ++
                            "(" ++ paramDesc ++ ")" ++ retDesc ++ "\n"
                -- Se a funcao retorna algo, descarta o resultado da pilha
                pop = case tipoRet of
                        TVoid   -> ""
                        TDouble -> "\tpop2\n" -- libera 2 espaço ocupado
                        _       -> "\tpop\n" -- libera 1 espaço ocupado
            in  return (argsStr ++ call ++ pop)
        Nothing ->
            return (argsStr ++ "; so para nao dar erro" ++ "\n")

-- Gera uma lista de comandos (bloco)
genBloco :: String -> [Funcao] -> TabelaLocal -> Tipo -> Bloco -> State Int String
genBloco c tg tl tipoRet cmds = do
    codes <- mapM (genCmd c tg tl tipoRet) cmds
    return (concat codes)

-- =====================================================================
-- Geracao de metodos (funcoes do programa)


genFuncao :: String -> [Funcao] -> (Id, [Var], Bloco) -> State Int String
genFuncao className tg (nome, vars, bloco) =
    case buscarGlobal tg nome of
        Nothing -> return ("; so para nao dar erro" ++ "\n")
        Just (params, tipoRet) -> do
            -- vars = params ++ variaveis_locais, todos com slot 0 (do parser)
            -- Atribuimos slots reais a partir de 0
            let tl         = atribuirSlots 0 vars
            let nLocals    = totalSlots vars -- tamanho fisico na memoria
            let paramDesc  = concatMap (\(_ :#: (t, _)) -> descTipo t) params
            let retDesc    = descTipo tipoRet
            let header     = ".method public static " ++ nome ++
                             "(" ++ paramDesc ++ ")" ++ retDesc ++ "\n" ++
                             "\t.limit stack 30\n" ++
                             "\t.limit locals " ++ show (max 1 nLocals) ++ "\n\n"
            bodyCode <- genBloco className tg tl tipoRet bloco
            -- Epilogo: return implicito para funcoes void (seguranca)
            let epilogo = case tipoRet of { TVoid -> "\treturn\n"; _ -> "" } -- se for do tipo void, no final tem return vazio
            return (header ++ bodyCode ++ epilogo ++ ".end method\n\n")

-- =====================================================================
-- Geracao do metodo main


genMain :: String -> [Funcao] -> [Var] -> Bloco -> State Int String
genMain className tg vars bloco = do
    -- Slot 0 = String[] args (obrigatorio pela JVM para o metodo main)
    -- Variaveis do programa comecam no slot 1
    let tl      = atribuirSlots 1 vars
    let nLocals = 1 + totalSlots vars   -- +1 para o String[] args no slot 0
    let header  = ".method public static main([Ljava/lang/String;)V\n" ++
                  "\t.limit stack 30\n" ++
                  "\t.limit locals " ++ show (max 1 nLocals) ++ "\n\n"
    bodyCode <- genBloco className tg tl TVoid bloco
    return (header ++ bodyCode ++ "\treturn\n.end method\n\n") -- tem returnvazio de segurança, ja que main nao retorna nada

-- =====================================================================
-- Cabecalho da classe


genCab :: String -> String
genCab nome =
    ".class public " ++ nome ++ "\n" ++
    ".super java/lang/Object\n\n" ++
    ".method public <init>()V\n" ++
    "\taload_0\n" ++
    "\tinvokenonvirtual java/lang/Object/<init>()V\n" ++
    "\treturn\n" ++
    ".end method\n\n"

-- =====================================================================
-- Geracao do programa completo

genProg :: String -> Programa -> State Int String
genProg className (Prog tg corpos varsMain blocoMain) = do
    funcCodes <- mapM (genFuncao className tg) corpos
    mainCode  <- genMain className tg varsMain blocoMain
    return (genCab className ++ concat funcCodes ++ mainCode)

-- | Ponto de entrada: recebe o nome da classe e o programa (AST anotada)
--   e devolve o codigo Jasmin como String.
gerar :: String -> Programa -> String
gerar nome prog = fst $ runState (genProg nome prog) 0 -- fst é o texto jasmin completão
