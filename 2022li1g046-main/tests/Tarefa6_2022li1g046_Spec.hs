module Tarefa6_2022li1g046_Spec where

import LI12223
import Test.HUnit

import Tarefa1_2022li1g046
import Tarefa2_2022li1g046
import Tarefa3_2022li1g046
import Tarefa4_2022li1g046
import Tarefa5_2022li1g046

testsT6 :: Test
testsT6 = TestLabel "Testes Tarefa 6" $ test ["Função que anima o jogador no jogo" ~: Jogo (Jogador (4,7)) (Mapa 10 [(Estrada 2,[Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro]),
                                                                                                                     (Rio (-2),[Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Tronco,Tronco,Tronco]),
                                                                                                                     (Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                     (Rio 1,[Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco]),
                                                                                                                     (Estrada 2,[Nenhum,Carro,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro]),
                                                                                                                     (Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                     (Estrada (-1),[Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro]),
                                                                                                                     (Relva,[Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore])])~=? animaJogo (Jogo (Jogador (5,7)) (Mapa 10 [(Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Tronco]),(Relva, [Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),(Rio 1, [Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco]),(Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva, [Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore])])) (Move Esquerda),
                                              "Função que teste se o mapa do jogo é valido" ~: True ~=? mapaValido (Mapa 10 [(Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                                                                                                                            (Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Tronco]),
                                                                                                                                            (Relva, [Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                                            (Rio 1, [Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco]),
                                                                                                                                            (Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro]),
                                                                                                                                            (Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                                            (Estrada (-1),[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                                                                                            (Relva, [Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore])]),
                                              "Função que verifica se o jogo está numa posição válida ou acabou" ~: False ~=? jogoTerminou (Jogo (Jogador (5,7)) (Mapa 10 [(Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                                                                                                                            (Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Tronco]),
                                                                                                                                            (Relva, [Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                                            (Rio 1, [Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco]),
                                                                                                                                            (Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro]),
                                                                                                                                            (Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                                            (Estrada (-1),[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                                                                                            (Relva, [Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore])])),
                                              "Função que estende o mapa do jogo" ~: Mapa 10 [(Estrada (-1),[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                                              (Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                                                                              (Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Tronco]),
                                                                                              (Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                              (Rio 1,[Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco]),
                                                                                              (Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro]),
                                                                                              (Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                              (Estrada (-1),[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                                              (Relva,[Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore])] ~=? estendeMapa (Mapa 10 [(Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                                                                                                                                                                                         (Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Tronco]),
                                                                                                                                                                                                         (Relva, [Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                                                                                                         (Rio 1, [Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco]),
                                                                                                                                                                                                         (Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro]),
                                                                                                                                                                                                         (Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                                                                                                         (Estrada (-1),[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                                                                                                                                                         (Relva, [Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore])]) 3,
                                              "Função que põe o mapa em movimento ao longo do tempo, removendo a ultima linha e adicionando outra" ~: Jogo (Jogador (5,8)) (Mapa 10 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                                                                                                                                     (Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                                                                                                                                                                     (Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Tronco]),
                                                                                                                                                                                     (Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                                                                                     (Rio 1,[Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco]),
                                                                                                                                                                                     (Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro]),
                                                                                                                                                                                     (Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                                                                                     (Estrada (-1),[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum])]) ~=? deslizaJogo 2 (Jogo (Jogador (5,7)) (Mapa 10 [(Estrada 2,[Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                                                                                                                                                                                                                                                                                                            (Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Tronco]),
                                                                                                                                                                                                                                                                                                                            (Relva, [Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                                                                                                                                                                                                                            (Rio 1, [Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco]),
                                                                                                                                                                                                                                                                                                                            (Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro]),
                                                                                                                                                                                                                                                                                                                            (Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                                                                                                                                                                                                                                                                            (Estrada (-1),[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                                                                                                                                                                                                                                                                            (Relva, [Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore])]))]