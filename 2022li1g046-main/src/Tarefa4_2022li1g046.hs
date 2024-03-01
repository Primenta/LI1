{- |
Module      : Tarefa4_2022li1g046
Description : Determinar se o jogo terminou
Copyright   : Miguel Tomás Antunes Pinto <a100815@alunos.uminho.pt>
              Tomás Araújo Santos <a104005@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}

module Tarefa4_2022li1g046 where


import LI12223
import Tarefa3_2022li1g046

-- | Função principal que emgloba as diversas funções auxiliares que verifica se o jogo terminou ou não.
jogoTerminou :: Jogo -> Bool
jogoTerminou j = foraDoMapa j || validaPosicao j


-- | Função definida que verifica se as coordenadas da posição do jogador estão 'dentro' dos limites do mapa.
foraDoMapa :: Jogo -> Bool
foraDoMapa (Jogo (Jogador (x,y)) mapa) = x < 0 || x > (larguraMapa mapa - 1) || y < 0 || y > (alturaMapa mapa - 1)

-- | Função que retira a largura do mapa dado.
larguraMapa :: Mapa -> Int
larguraMapa (Mapa l _) = l

-- | Função que retira a altura do mapa dado.
alturaMapa :: Mapa -> Int
alturaMapa (Mapa _ l) = length l


-- | Função que verifica se o jogador se encontra numa posição inválida caso o terreno seja ´Rio´.
invalPosRio :: Jogador -> [Obstaculo] -> Bool
invalPosRio (Jogador (x,y)) l = ((!!) l x) == Nenhum

-- |Função que verifica se o jogador se encontra numa posição inválida caso o terreno seja 'Estrada'.
invalPosEstrada :: Jogador -> [Obstaculo] -> Bool
invalPosEstrada (Jogador (x,y)) l = ((!!) l x) == Carro

-- |Função que verifica se o jogador se encontra numa posição inválida, tendo em conta todos os tipos de terrenos.
invalPos :: Jogador -> (Terreno,[Obstaculo]) -> Bool
invalPos j (Rio _, l) = invalPosRio j l
invalPos j (Estrada _,l) = invalPosEstrada j l
invalPos j _ = False

-- |Função que verifica se o jogador se encontra numa posição inválida dependendo da linha onde se encontra e do tipo de terreno da mesma.
validaPosicao:: Jogo -> Bool
validaPosicao (Jogo j (Mapa l t)) = invalPos j (linhaJogador (Jogo j (Mapa l t)))
