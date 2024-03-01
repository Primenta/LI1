module Tarefa3_2022li1g046_Spec where

import LI12223
import Tarefa3_2022li1g046
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test [
												"Teste da Função da movimentação do jogador para a Direita" ~: (Jogador (1,0)) ~=? moveJogador (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum])])) (Move Direita),
												"Teste da Função da movimentação do jogador para a Esquerda" ~: (Jogador (0,0)) ~=? moveJogador (Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum])])) (Move Esquerda),
												"Teste da Função da movimentação do jogador para Cima" ~: (Jogador (0,0)) ~=? moveJogador (Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum])])) (Move Cima),
												"Teste da Função da movimentação do jogador para Baixo" ~: (Jogador (0,1)) ~=? moveJogador (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum])])) (Move Baixo),
												-- | Teste da função moveJogador tendo em conta as restrições impostas
												"Teste da Função da movimentação do jogador para a Esquerda quando este se encontra no limite do mapa" ~: (Jogador (0,0)) ~=? moveJogador (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum])])) (Move Esquerda),
												"Teste da Função da movimentação do jogador para a Direita quando este se encontra no limite do mapa" ~: (Jogador (1,0)) ~=? moveJogador (Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum])])) (Move Direita),
												"Teste da Função da movimentação do jogador para Cima quando este se encontra no limite do mapa" ~: (Jogador (0,0)) ~=? moveJogador (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum])])) (Move Cima),
												"Teste da Função da movimentação do jogador para Baixo quando este se encontra no limite do mapa" ~: (Jogador (0,1)) ~=? moveJogador (Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum])])) (Move Baixo),
												"Teste da Função da movimentação do jogador para a Esquerda quando este se encontra à direita de uma árvore" ~: (Jogador (1,0)) ~=? moveJogador (Jogo (Jogador (1,0)) (Mapa 2 [(Relva,[Arvore,Nenhum]),(Rio 1,[Tronco,Nenhum])])) (Move Esquerda),
												"Teste da Função da movimentação do jogador para a Direita quando este se encontra à esquerda de uma árvore" ~: (Jogador (0,0)) ~=? moveJogador (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Arvore]),(Rio 1,[Tronco,Nenhum])])) (Move Esquerda),
												"Teste da Função da movimentação do jogador para Cima quando este se encontra por baixo de uma árvore" ~: (Jogador (0,1)) ~=? moveJogador (Jogo (Jogador (0,1)) (Mapa 2 [(Relva,[Arvore,Nenhum]),(Rio 1,[Tronco,Nenhum])])) (Move Cima),
												"Teste da Função da movimentação do jogador para Baixo quando este se encontra por cima de uma árvore" ~: (Jogador (0,0)) ~=? moveJogador (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Relva ,[Arvore,Nenhum])])) (Move Baixo),
												"Teste da Função da movimentação do jogador caso a jogada seja 'Parado' mas se encontra sob um tronco" ~: (Jogador (0,0)) ~=? moveJogador (Jogo (Jogador (1,0)) (Mapa 2 [(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Tronco,Nenhum])])) (Parado),
												"Teste da Função da movimentação do jogador caso a jogada seja 'Parado' e este não se encontra sob um tronco" ~: (Jogador (1,0)) ~=? moveJogador (Jogo (Jogador (1,0)) (Mapa 2 [(Estrada 1,[Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum])])) (Parado),
												-- | Teste das funções responsáveis pela atualização do mapa.
												"Teste da Função que atualiza as posições dos obstáculos de um Terreno dependendo da velocidade" ~: (Estrada 2,[Carro,Carro,Carro,Nenhum,Carro,Nenhum]) ~=? atualizaTerreno (Estrada 2, [Carro,Carro,Nenhum,Carro,Nenhum,Carro]) (Jogador (2,0)) 1,
												"Teste da Função que atualiza as posições dos obstáculos de um Terreno dependendo da velocidade" ~: (Rio (-3),[Tronco,Tronco,Nenhum,Nenhum]) ~=? atualizaTerreno (Rio (-3), [Tronco,Nenhum,Nenhum,Tronco]) (Jogador (1,0)) 0,
												"Teste da Função que atualiza um dado mapa" ~: [(Rio (-2),[Tronco,Nenhum,Nenhum,Tronco]),(Estrada 3,[Carro,Nenhum,Nenhum,Carro]),(Relva,[Arvore,Arvore,Arvore,Nenhum])] ~=? atualizaMapa [(Rio (-2),[Nenhum ,Tronco ,Tronco ,Nenhum ]),(Estrada 3, [Carro ,Carro ,Nenhum,Nenhum]),(Relva ,[Arvore ,Arvore ,Arvore ,Nenhum ])] (Jogador (0,0)) 0,
												"Teste da Função animaJogo que dado um jogo atualiza tanto os obstáculos de cada linha do mapa tanto como a posição de um jogador dado uma jogada" ~: Jogo (Jogador (3,2)) (Mapa 4 [(Rio (-2),[Nenhum,Tronco,Tronco,Nenhum]),(Estrada 3,[Nenhum,Carro,Nenhum,Carro]),(Relva,[Arvore,Arvore,Arvore,Nenhum])]) ~=? animaJogo (Jogo (Jogador (3,1)) (Mapa 4 [(Rio (-2),[Tronco,Nenhum,Nenhum,Tronco]),(Estrada 3,[Carro,Nenhum,Carro,Nenhum ]),(Relva,[Arvore,Arvore,Arvore,Nenhum])])) (Move Baixo)]