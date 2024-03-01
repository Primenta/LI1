module Tarefa4_2022li1g046_Spec where

import LI12223
import Tarefa4_2022li1g046
import Test.HUnit

-- | Suite de testes referentes à tarefa 4 que verifica se o jogo terminou.
testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste da altura do mapa" ~: 2 ~=? alturaMapa (Mapa 2 [(Rio 1, [Nenhum,Tronco]),(Estrada 2, [Nenhum, Carro])]), -- Testa a função que calcula a altura do mapa.
												
-- Testa a função que calcula a largura do mapa.
												"Testa da largura do mapa" ~: 3 ~=? larguraMapa (Mapa 3 [(Rio 1, [Nenhum,Tronco]),(Estrada 2, [Nenhum, Carro])]),
-- Testa a função que verifica se o jogador se encontra dentro dos limites do mapa.
												"Testa da posição dentro dos limites do mapa" ~: True ~=? foraDoMapa (Jogo (Jogador (-1,0)) (Mapa 2 [(Relva,[Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco])])),
												"Testa da posição dentro dos limites do mapa" ~: False ~=? foraDoMapa (Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco])])),
-- Testa a função que caso o terreno onde o jogador se encontra seja do tipo rio verifica se o jogador está numa posição inválida caso não esteja sobre um tronco.
												"Teste da posição inválida num rio sobre a água" ~: True ~=? invalPosRio (Jogador (0,0)) [Nenhum,Tronco],
												"Teste da posição inválida num rio sobre um tronco" ~: False ~=? invalPosRio (Jogador (1,0)) [Nenhum,Tronco],
-- Testa a função que caso o terreno onde o jogador se encontra seja do tipo estrada verifica se o jogador está numa posição inválida caso esteja na posição de um carro.
												"Teste da posição inválida numa estrada" ~: True ~=? invalPosEstrada (Jogador (1,0)) [Nenhum,Carro],
												"Teste da posição inválida numa estrada" ~: False ~=? invalPosEstrada (Jogador (0,0)) [Nenhum,Carro],
-- Testa a função que verifica se o jogador se encontra numa posição inválida para qualquer tipo de terreno.
												"Teste da posição inválida em qualquer terreno" ~: True ~=? invalPos (Jogador (0,0)) (Estrada 1,[Carro,Nenhum]),
												"Teste da posição inválida em qualquer terreno" ~: False ~=? invalPos (Jogador (1,0)) (Relva,[Arvore,Nenhum]),
-- Testa a função que se verifica se o jogador se encontra numa inválida perante um qualquer mapa. 
												"Teste da posição inválida em qualquer terreno num mapa 'complexo' " ~: True ~=? validaPosicao (Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Nenhum,Arvore]),(Rio 1,[Tronco,Nenhum])])),
												"Teste da posição inválida em qualquer terreno num mapa 'complexo' " ~: False ~=? validaPosicao (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco])])),
-- Testa a função final que verifica se o jogo terminou de acordo com as condições impostas.
												"Verifica se o jogo terminou" ~: False ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco])])),
												"Verifica se o jogo terminou" ~: True ~=? jogoTerminou (Jogo (Jogador (1,0)) (Mapa 2 [(Estrada 2,[Nenhum,Carro]),(Rio 1,[Nenhum,Tronco])]))]





