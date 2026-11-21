module Main where

import Data.Array
import Data.List (foldl')
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Control.Exception (evaluate)

-- Implementacao com arrays

retornosDiarioVetor :: Array Int Double -> Array Int Double
retornosDiarioVetor precos =
    let (inicio, fim) = bounds precos
    in if fim <= inicio
       then listArray (inicio, inicio - 1) []
       else array (inicio, fim - 1)
            [(i, (precos ! (i + 1) - precos ! i) / precos ! i)
            | i <- [inicio .. fim - 1]]

mediaRetornoVetor :: Array Int Double -> Double
mediaRetornoVetor retornos =
    let valores = elems retornos
    in if null valores
       then 0
       else sum valores / fromIntegral (length valores)

produtoEscalarVetor :: Array Int Double -> Array Int Double -> Double
produtoEscalarVetor pesos retornos =
    let p = elems pesos
        r = elems retornos
    in if length p /= length r
       then error "Os arrays devem possuir o mesmo tamanho."
       else sum (zipWith (*) p r)

-- Implementacao com listas

retornosDiarioLista :: [Double] -> [Double]
retornosDiarioLista [] = []
retornosDiarioLista [_] = []
retornosDiarioLista (x:y:xs) =
    ((y - x) / x) : retornosDiarioLista (y:xs)

mediaRetornoLista :: [Double] -> Double
mediaRetornoLista retornos =
    if null retornos
    then 0
    else sum retornos / fromIntegral (length retornos)

produtoEscalarLista :: [Double] -> [Double] -> Double
produtoEscalarLista pesos retornos
    | length pesos /= length retornos =
        error "As listas devem possuir o mesmo tamanho."
    | otherwise = sum (zipWith (*) pesos retornos)

-- Funcoes auxiliares

medirTempo :: IO Double -> IO Double
medirTempo tarefa = do
    inicio <- getCPUTime
    resultado <- tarefa
    resultadoAvaliado <- evaluate resultado
    fim <- getCPUTime
    return (fromIntegral (fim - inicio) / 1.0e12)

gerarDados :: Int -> [Double]
gerarDados n =
    take n (iterate (\x -> x + 0.01) 10.0)

gerarDadosVariados :: Int -> [Double]
gerarDadosVariados n =
    map (\x -> fromIntegral (x `mod` 10000) / 100.0 + 1.0)
        (take n (iterate (\x -> (x * 16807 + 17) `mod` 2147483647) 1))

compararDesempenho :: IO ()
compararDesempenho = do
    putStrLn "\nComparacao de desempenho"
    let quantidade = 1000000
        precosListaGrande = gerarDadosVariados quantidade
        precosVetorGrande = listArray (0, quantidade - 1) precosListaGrande

    evaluate (foldl' (+) 0 precosListaGrande)

    putStrLn "\nCenario 1: retornos diarios e media"

    tempoLista1 <- medirTempo $ do
        let retornos = retornosDiarioLista precosListaGrande
        return (mediaRetornoLista retornos)

    tempoVetor1 <- medirTempo $ do
        let retornos = retornosDiarioVetor precosVetorGrande
        return (mediaRetornoVetor retornos)

    printf "Tempo com listas: %.6f segundos\n" tempoLista1
    printf "Tempo com arrays: %.6f segundos\n" tempoVetor1

    let pesosListaGrande = gerarDados quantidade
        retornosListaGrande = gerarDadosVariados quantidade
        pesosVetorGrande = listArray (0, quantidade - 1) pesosListaGrande
        retornosVetorGrande = listArray (0, quantidade - 1) retornosListaGrande

    evaluate (foldl' (+) 0 pesosListaGrande)
    evaluate (foldl' (+) 0 retornosListaGrande)

    putStrLn "\nCenario 2: produto escalar"

    tempoLista2 <- medirTempo $ do
        return (produtoEscalarLista pesosListaGrande retornosListaGrande)

    tempoVetor2 <- medirTempo $ do
        return (produtoEscalarVetor pesosVetorGrande retornosVetorGrande)

    printf "Tempo com listas: %.6f segundos\n" tempoLista2
    printf "Tempo com arrays: %.6f segundos\n" tempoVetor2

exemploUso :: IO ()
exemploUso = do
    let precos = [38.20, 38.80, 37.90, 39.10, 39.60]
        retornos = retornosDiarioLista precos
        media = mediaRetornoLista retornos
        pesos = [0.40, 0.35, 0.25]
        retornoAtivos = [0.0157, -0.0232, 0.0317]
        resultado = produtoEscalarLista pesos retornoAtivos

    putStrLn "Analise de portfolio"
    putStrLn "\nPrecos da acao PETR4:"
    print precos

    putStrLn "\nRetornos diarios:"
    print retornos

    putStrLn "\nMedia dos retornos:"
    printf "%.6f (%.4f%%)\n" media (media * 100)

    putStrLn "\nPesos do portfolio:"
    print pesos

    putStrLn "\nRetorno dos ativos:"
    print retornoAtivos

    putStrLn "\nRetorno esperado do portfolio:"
    printf "%.6f (%.4f%%)\n" resultado (resultado * 100)

main :: IO ()
main = do
    exemploUso
    compararDesempenho