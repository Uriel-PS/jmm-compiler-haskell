module Main where

import System.Environment (getArgs)
import Lex (alexScanTokens)
import Parser (parserLinguagem)
import Semantico (verificarPrograma, Result(..))
import Gerador (gerar)

-- Para compilar:
-- alex Lex.x
-- happy --ghc Parser.y
-- ghc Main.hs
-- ./Main 'arquivo'

-- Montar bytecode JAVA (JASMIN)
-- java -jar jasmin.jar teste.j
-- java teste

main :: IO ()
main = do
    args <- getArgs
    if null args then
        putStrLn "Uso: ./Main <nome_do_arquivo>"
    else do
        let arquivo = head args
        codigoFonte <- readFile arquivo
        
        -- Análise Léxica e Sintática
        let astCrua = parserLinguagem (alexScanTokens codigoFonte)
        
        -- Análise Semântica
        let Result (deuErro, mensagens, astValidada) = verificarPrograma astCrua
        
        -- Imprime erros ou advertências
        putStrLn mensagens
        
        -- Geração de Código
        if not deuErro then do
            writeFile "Teste.j" (gerar "teste" astValidada)
            putStrLn "Compilacao concluida! Arquivo 'teste.j' gerado."
        else
            putStrLn "Compilacao abortada devido a erros semanticos."