module Tarefa2_2022li1g046_Spec where

import LI12223
import Tarefa2_2022li1g046
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test [ 	
												-- | Testa as funções auxiliares que remetem para verificação das condições que são impostas para que os obstáculos sejam válidos.
												"Teste da contagem das unidades ocupadas por um tronco no final de uma lista de obstáculos" ~: 4 ~=? contarTroncosRioSeguidos [Nenhum,Tronco,Tronco,Tronco,Tronco],
												"Teste da contagem das unidades ocupadas por um carro no final de uma lista de obstáculos" ~: 2 ~=? contarCarrosEstradaSeguidos [Nenhum,Carro,Nenhum,Carro,Carro],
												"Teste da validação da condição de unidades ocupadas por um carro" ~: True ~=? validaCarrosSeguidos [Nenhum,Carro,Nenhum,Carro,Carro],
												"Teste da validação da condição de unidades ocupadas por um tronco" ~: False ~=? validaTroncosSeguidos [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco],
												-- | Testa a função proximosObstaculosValidos em diversos casos:
												"Teste dos próximos obstáculos válidos para um terreno do tipo Estrada" ~: [Nenhum,Carro] ~=? proximosObstaculosValidos 5 (Estrada 1, [Carro,Carro,Nenhum,Carro]),
												"Teste dos próximos obstáculos válidos para um terreno do tipo Rio" ~: [Nenhum,Tronco] ~=? proximosObstaculosValidos 3 (Rio 1, [Nenhum]),
												"Teste dos próximos obstáculos válidos para um terreno do tipo Relva" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 4 (Relva, [Nenhum,Arvore,Arvore]),
												"Teste do próximo obstáculo válido caso até ao momento não exista uma passagem possível para o jogador num rio" ~: [Tronco] ~=? proximosObstaculosValidos 2 (Rio 2, [Nenhum]),
												"Teste do próximo obstáculo válido caso até ao momento não exista uma passagem possível para o jogador numa relva ou estrada" ~: [Nenhum] ~=? proximosObstaculosValidos 5 (Relva, [Arvore,Arvore,Arvore,Arvore]),
												"Teste do próximo obstáculo válido caso o tronco já esteja a ocupar o seu limite de unidades" ~: [Nenhum] ~=? proximosObstaculosValidos 7 (Rio 1, [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]),
												"Teste do próximo obstáculo válido caso o carro já esteja a ocupar o seu limite de unidades" ~: [Nenhum] ~=? proximosObstaculosValidos 5 (Estrada 1, [Nenhum,Carro,Carro,Carro]),
												"Teste dos próximo obstáculo válidos caso seja necessária gerar uma lista inteira" ~: [Nenhum,Arvore] ~=? proximosObstaculosValidos 5 (Relva, []),
												"Teste dos próximo obstáculo válidos caso a lista de obstáculos já se encontre totalmente preenchida" ~: [] ~=? proximosObstaculosValidos 5 (Relva, [Arvore,Arvore,Nenhum,Arvore,Nenhum]),
											 	-- | Testa a função proximosTerrenosValidos para as diversas condições impostas:
												"Teste do próximo terreno válido caso tenha sido alcançado o limite de rios contiguos" ~: [Estrada 0,Relva] ~=? proximosTerrenosValidos (Mapa 2 [(Rio 1, [Nenhum,Tronco]),(Rio 2, [Tronco,Tronco]),(Rio 1, [Tronco,Nenhum]),(Rio 2, [Tronco,Tronco]),(Relva,[Arvore,Nenhum])]),
												"Teste do próximo terreno válido caso tenha sido alcançado o limite de 'estradas' ou 'relvas' contiguas" ~: [Rio 0,Relva] ~=? proximosTerrenosValidos (Mapa 2 [(Estrada 1, [Nenhum,Carro]),(Estrada 2, [Carro,Nenhum]),(Estrada 1, [Carro,Nenhum]),(Estrada 2, [Carro,Nenhum]),(Estrada 3,[Carro,Nenhum]),(Rio 1,[Tronco,Nenhum])]),
												"Teste do próximo terreno válido caso não tenha de ser 'respeitada' nenhuma condição" ~: [Rio 0, Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Estrada 2,[Carro,Nenhum])]),
												-- | Teste das funções que envolvem a seleção de elementos de forma aleatória.
												"Teste da função que seleciona um elemento de uma lista de forma 'aleatoria'" ~: Carro ~=? getElemento 66 [Nenhum,Carro],
												"Teste da função que seleciona um terreno de forma 'aleatoria'" ~: Rio 0 ~=? gerarTerreno (Mapa 3 [(Rio 2,[Nenhum,Tronco ,Nenhum ]),(Estrada 2,[Carro,Carro ,Nenhum ])]) 15,
												"Teste da função que gera uma lista de obstáculos aleatoria para um dado terreno" ~:[Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Tronco] ~=? gerarObstaculos 6 (Rio 0,[]) 99,
												"Teste da função que gera um valor de velocidade compreendido em [-2,2]" ~: 1 ~=? gerarVelocidade 5,
												-- | Teste das funções finais.
												"Teste da função que atribui um valor de velocidade gerado a um terreno" ~: Rio (-2) ~=? atribuiVelocidade (Rio 0) 34,
												"Teste da função geraLinha que gera uma nova linha com terreno e obstáculos aleatórios consoante as condições impostas" ~: (Rio (-2),[Nenhum,Tronco]) ~=? geraLinha (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Estrada 2,[Carro,Nenhum])]) 24,
												"Alternativa de teste da função geraLinha" ~: (Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum]) ~=? geraLinha (Mapa 8 [(Estrada 2,[Carro,Carro,Nenhum ,Carro ,Nenhum ,Carro,Nenhum ,Carro ]),(Rio 3,[Tronco ,Tronco ,Nenhum ,Nenhum ,Nenhum ,Tronco ,Nenhum ,Tronco ])]) 73,
												"Teste da função estendeMapa que adiciona a nova linha gerada ao mapa existente" ~: Mapa 8 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum]),(Estrada 2,[Carro,Carro,Nenhum,Carro,Nenhum,Carro,Nenhum,Carro]),(Rio 3,[Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Tronco])] ~=? estendeMapa (Mapa 8 [(Estrada 2,[Carro,Carro,Nenhum ,Carro ,Nenhum ,Carro,Nenhum ,Carro ]),(Rio 3,[Tronco ,Tronco ,Nenhum ,Nenhum ,Nenhum ,Tronco ,Nenhum ,Tronco ])]) 73]







