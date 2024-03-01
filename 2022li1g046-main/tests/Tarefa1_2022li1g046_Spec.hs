module Tarefa1_2022li1g046_Spec where

import LI12223
import Tarefa1_2022li1g046
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["Função que testa se Obstaculos são validos na Relva" ~: True ~=? validarObstaculoRelva (Mapa 4 [(Relva,[Arvore,Nenhum,Nenhum,Arvore])]),
                                              "Função que testa se Obstaculos são validos na Estrada" ~: True ~=? validarObstaculoEstrada (Mapa 4 [(Estrada 2,[Carro,Nenhum,Carro,Carro])]),
                                              "Função que testa se Obstaculos são validos no Rio" ~: True ~=? validarObstaculoRio (Mapa 4 [(Rio 1,[Nenhum,Tronco,Nenhum,Nenhum])]),
                                              "Função que testa se os Obstaculos são validos nos seus Terrenos" ~: True ~=? validarObstaculos (Mapa 4 [(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Rio 4,[Tronco,Tronco,Nenhum,Tronco]),(Estrada 3,[Carro,Nenhum,Nenhum,Nenhum])]),
                                              "Função que testa se o número de Obstaculos corresponde à largura do Mapa" ~: True ~=? contadorDeObstaculosF (Mapa 3 [(Rio 2,[Nenhum,Nenhum,Tronco]),(Estrada 1,[Carro,Carro,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])]),
                                              "Função que testa se o nos Obstaculos do Mapa existe pelo menos um 'Nenhum' em cada lista de Obstaculos" ~: True ~=? existirNenhumF(Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum]),(Estrada 5,[Nenhum,Nenhum,Nenhum,Carro,Carro]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])]),
                                              "Função que Testa se, numa lista de Terrenos, Rios contíguos têm direções opostas" ~: True ~=? validarRio [Rio 1, Estrada 2, Rio 2, Rio (-3)],
                                              "Função que testa se Rios consecutivos têm direções opostas, num Mapa" ~: True ~=? riosContrarios (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Rio (-2),[Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])]),
                                              "Função que testa se o tamanho do Carro é valido no Mapa" ~: True ~=? validaTamanhoCarro (Mapa 5 [(Estrada 5,[Carro,Carro,Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore])]),
                                              "Função que testa se o tamanho do Tronco é valido no Mapa" ~: True ~=? validaTamanhoTronco (Mapa 6 [(Rio 2,[Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore]),(Estrada 2,[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum])]),
                                              "Função que testa se a quantidade de Relvas consecutivas é valida no Mapa" ~: True ~=? validaTamanhoRelva (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore]),(Rio 2,[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]),
                                              "Função que testa se a quantidade de Estradas consecutivas é valida no Mapa" ~: True ~=? validaTamanhoEstrada (Mapa 3 [(Estrada 3,[Carro,Nenhum,Nenhum]),(Estrada 1,[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Nenhum])]),
                                              "Função que testa se o Mapa é válido - Teste 1 " ~: True ~=? mapaValido (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Arvore]),(Estrada (-2),[Carro,Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Rio (-3),[Tronco,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore,Nenhum])]),
                                              "Teste 2 " ~: True ~=? mapaValido (Mapa 6 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum]),(Rio 3,[Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum])]),
                                              "Teste 3 " ~: True ~=? mapaValido (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 5,[Carro,Nenhum,Carro]),(Estrada 3,[Nenhum,Nenhum,Carro]),(Rio 2,[Tronco,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])]),
                                              "Teste 4 " ~: True ~=? mapaValido (Mapa 5 [(Rio 1,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Rio (-1),[Nenhum,Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada 2,[Nenhum,Carro,Carro,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])]),
                                              "Teste 5 " ~: True ~=? mapaValido (Mapa 2 [(Estrada 2,[Carro,Nenhum]),(Relva,[Arvore,Nenhum]),(Rio 2, [Nenhum,Tronco]),(Relva,[Arvore,Nenhum])]),
                                              "Teste 6 " ~: True ~=? mapaValido (Mapa 7 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 5,[Carro,Carro,Nenhum,Carro,Nenhum,Nenhum,Carro]),(Estrada (-5),[Nenhum,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum]),(Rio 2,[Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Tronco])])
                                              ]
