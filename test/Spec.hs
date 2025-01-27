{-|
Module      : Main
Description : Este módulo serve para correr todos os testes de uma só vez.
              Para isso, importa os módulos de testes de cada tarefa e corre-os
              em conjunto com os testes básicos.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Main (main) where

import Test.HUnit

import Tarefa1Spec
import Tarefa2Spec
import Tarefa3Spec

-- | Teste da suite
testSuite :: Test
testSuite =
  TestLabel "Spec Test Suit" $
    test
      [ testesTarefa1,
        testesTarefa2,
        testesTarefa3
      ]

-- | Função para rodar os testes 
main :: IO ()
main = runTestTTAndExit $ test [testSuite]

