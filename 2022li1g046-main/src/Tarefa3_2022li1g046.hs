{- |
Module      : Tarefa3_2022li1g046
Description : Movimentação do personagem e obstáculos
Copyright   : Miguel Tomás Antunes Pinto <a100815@alunos.uminho.pt>
              Tomás Araújo Santos <a104005@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g046 where

import LI12223


-- | Função que atualiza o jogo, movendo os obstáculos dos terrenos e movendo o jogador de acordo com a jogada.

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa largura t)) jogada = (Jogo (moveJogador (Jogo (Jogador (x,y)) (Mapa largura t)) jogada) (Mapa largura (atualizaMapa t (Jogador (x,y)) 0)))

-- | Função que move jogador tendo em conta as restrições impostas.

moveJogador :: Jogo -> Jogada -> Jogador
moveJogador (Jogo (Jogador (x,y)) (Mapa largura t)) jogada
    | jogada == Move Esquerda = if x == 0 then (Jogador (x,y)) else if f then (Jogador (x,y)) else (Jogador (x-1,y))
    | jogada == Move Direita = if x == largura - 1  then (Jogador (x,y)) else if f then (Jogador (x,y)) else (Jogador (x+1,y))
    | jogada == Move Cima = if y == 0 then (Jogador (x,y)) else   if f then (Jogador (x,y)) else (Jogador (x,y-1))
    | jogada == Move Baixo = if y == (length t - 1) then (Jogador (x,y)) else  if f then (Jogador (x,y)) else (Jogador (x,y+1))
    | jogada == Parado = if (valJogadorEmTronco (Jogo (Jogador (x,y)) (Mapa largura t))) then (atualizaJogador (Jogo (Jogador (x,y)) (Mapa largura t)))
                        else (Jogador (x,y))
                        where f = invalMoveParaArvore (Jogo (Jogador (x,y)) (Mapa largura t)) jogada

-- | Função que  atualiza a posição do jogador consoante a velocidade do terreno onde se encontra.
atualizaJogador :: Jogo -> Jogador
atualizaJogador (Jogo (Jogador (x,y)) (Mapa largura t)) = (Jogador ((x+z),y))
                                                        where z = (velocidadeTerreno (linhaJogador (Jogo (Jogador (x,y)) (Mapa largura t))))


-- | Função que retira a velocidade de um dado terreno

velocidadeTerreno :: (Terreno,[Obstaculo]) -> Int
velocidadeTerreno (Estrada v,_) = v
velocidadeTerreno (Rio v,_) = v
velocidadeTerreno (Relva,_) = 0

-- | Função que retira a linha onde o jogador se encontra.

linhaJogador :: Jogo -> (Terreno,[Obstaculo])
linhaJogador (Jogo (Jogador (x,y)) (Mapa _ t)) = (!!) t y   

-- | Função que verifica se o jogador se encontra sob um tronco dado uma linha de um mapa.

valPosTronco :: Jogador -> (Terreno,[Obstaculo]) -> Bool
valPosTronco (Jogador (x,y)) (_,l) = ((!!) l x) == Tronco

-- | Função que dado um qualquer mapa verifica se o jogador se contra sob um tronco.

valJogadorEmTronco :: Jogo -> Bool
valJogadorEmTronco (Jogo j m) = (valPosTronco j (linhaJogador (Jogo j m)))


-- | Função que verifica dado um Jogo e uma Jogada se o jogador 'estará a ir em direção a uma árvore'.

invalMoveParaArvore :: Jogo -> Jogada -> Bool
invalMoveParaArvore (Jogo (Jogador (x,y)) (Mapa largura t)) jogada
    | jogada == Move Direita = if (!!) l (x+1) == Arvore then True else False
    | jogada == Move Esquerda = if (!!) l (x-1) == Arvore then True else False
    | jogada == Move Cima = if (!!) l2 x == Arvore then True else False
    | jogada == Move Baixo = if (!!) l3 x == Arvore then True else False
                              where l = (snd (linhaJogador (Jogo (Jogador (x,y)) (Mapa largura t))))
                                    l2 = (snd (linhaJogador (Jogo (Jogador (x,y-1)) (Mapa largura t))))
                                    l3 = (snd (linhaJogador (Jogo (Jogador (x,y+1)) (Mapa largura t))))

-- | Função que atualiza um terreno, tendo em conta a velocidade deste.

atualizaTerreno :: (Terreno,[Obstaculo]) -> Jogador -> Int -> (Terreno,[Obstaculo])
atualizaTerreno (Estrada v, obs) (Jogador (x,y)) 1
    | v <= 0 = verificaAtropelamentoEsquerda 0 (Estrada v, obs) (Jogador (x,y))
    | otherwise = verificaAtropelamentoDireita 0 (Estrada v, obs) (Jogador (x,y))
atualizaTerreno (Estrada v,obs) _ _
    | v <= 0 = (Estrada v,(shiftEsquerdaLista v obs))
    | otherwise = (Estrada v,(shiftDireitaLista v obs))
atualizaTerreno (Rio v, obs) _ _
    | v <= 0 = (Rio v,(shiftEsquerdaLista v obs))
    | otherwise = (Rio v, (shiftDireitaLista v obs))
atualizaTerreno (Relva,obs) _ _= (Relva,obs)

-- | Função que verifica se o jogador é atropelado quando os carros se movem para a direita
verificaAtropelamentoDireita :: Int -> (Terreno,[Obstaculo]) -> Jogador -> (Terreno,[Obstaculo])
verificaAtropelamentoDireita c (Estrada v,obs) (Jogador (x,y))-- c remete para o contador da velocidade
    | c == v = (Estrada v,obs)
    | (!!) obs x == Carro = (Estrada v,obs)
    | (!!) obs x == Nenhum = verificaAtropelamentoDireita  (c+1) (Estrada v,shiftDireitaLista 1 obs) (Jogador (x,y))

-- | Função que verifica se o jogador é atropelado quando os carros se movem para a esquerda                                                                
verificaAtropelamentoEsquerda :: Int -> (Terreno,[Obstaculo]) -> Jogador -> (Terreno,[Obstaculo])
verificaAtropelamentoEsquerda c (Estrada v,obs) (Jogador (x,y)) -- c remete para o contador da velocidade
    | c == v = (Estrada v,obs)
    | (!!) obs x == Carro = (Estrada v,obs)
    | (!!) obs x == Nenhum = verificaAtropelamentoEsquerda (c-1) (Estrada v,shiftEsquerdaLista 1 obs) (Jogador (x,y))


-- | Função que movimenta uma lista de elementos n posições para a direita.
shiftDireitaLista :: Int -> [a] -> [a]
shiftDireitaLista v obs = drop ((length obs) - (mod v (length obs))) obs ++ take ((length obs) - (mod v (length obs))) obs

-- | Função que movimenta uma lista de elementos n posições para a esquerda.
shiftEsquerdaLista :: Int -> [a] -> [a]
shiftEsquerdaLista v obs = drop (mod (abs v) (length obs)) obs ++ take (mod (abs v) (length obs)) obs

-- | Função que atualiza o Mapa, tendo em conta a velocidade de cada Terreno.

atualizaMapa :: [(Terreno,[Obstaculo])] -> Jogador -> Int -> [(Terreno,[Obstaculo])]
atualizaMapa [] _ _ = []
atualizaMapa (h:t) (Jogador (x,y)) n --este n corresponde a um contador para que a função que verifica se o jogador foi atropelado seja apenas utilizada quando o jogador se encontra numa linha cujo terreno seja estrada 
    | n == y = atualizaTerreno h (Jogador (x,y)) 1:atualizaMapa t (Jogador (x,y)) (n+1)
    | otherwise = atualizaTerreno h (Jogador (x,y)) 0:atualizaMapa t (Jogador (x,y)) (n+1)

