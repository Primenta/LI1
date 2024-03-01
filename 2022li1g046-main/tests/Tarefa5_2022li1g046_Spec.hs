module Tarefa5_2022li1g046_Spec where

import LI12223
import Tarefa5_2022li1g046
import Test.HUnit

-- | Suite de testes referentes à tarefa 5 que verifica o funcionamento do deslize do mapa.
testsT5 :: Test
testsT5 = TestLabel "Testes Tarefa 5" $ test ["Teste da adição da atualização de um dado mapa com a adição de uma nova linha e a remoção da última" ~: Mapa 2 [(Rio 1,[Tronco,Tronco]),(Rio 2,[Nenhum,Tronco])] ~=? atualizaMap (Mapa 2 [(Rio 2,[Nenhum ,Tronco ]),(Relva,[Nenhum ,Nenhum ])]) 21,
												"Testa da função deslizaJogo que atualiza o mapa e as coordenadas do jogador" ~: Jogo (Jogador (1,1)) (Mapa 2 [(Rio 1,[Tronco,Tronco]),(Rio 2,[Nenhum,Tronco])]) ~=? deslizaJogo 21 (Jogo (Jogador (1,0)) (Mapa 2 [(Rio 2,[Nenhum ,Tronco ]),(Relva,[Nenhum ,Nenhum ])]))]


										