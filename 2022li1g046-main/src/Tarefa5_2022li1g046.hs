{- |
Module      : Tarefa4_2022li1g046
Description : Deslize do Mapa
Copyright   : Miguel Tomás Antunes Pinto <a100815@alunos.uminho.pt>
              Tomás Araújo Santos <a104005@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g046 where

import LI12223
import Tarefa2_2022li1g046
import System.Random


-- | Função encarregue do "deslize" do mapa, adicionando uma nova linha e removendo a última.

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo n (Jogo (Jogador (x,y)) (Mapa largura t))  =  (Jogo (Jogador (x,y+1)) (atualizaMap (Mapa largura t) n))

-- | Função auxiliar que adiciona uma nova linha ao mapa dado, sem a última linha (uso do 'init')

atualizaMap :: Mapa -> Int -> Mapa
atualizaMap (Mapa largura t) n = estendeMapa (Mapa largura (init t)) n

-- | Função auxiliar que atualiza as coordenadas do jogador consoante o deslize do mapa