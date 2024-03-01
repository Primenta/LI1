{- |
Module      : Tarefa2_2022li1g046
Description : Geração contínua de um mapa
Copyright   : Miguel Tomás Antunes Pinto <a100815@alunos.uminho.pt>
              Tomás Araújo Santos <a104005@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g046 where

import LI12223
import System.Random
import Tarefa1_2022li1g046

-- | Função final encarregue de juntar a nova linha gerada ao mapa já existente.

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa largura t) n = (Mapa largura ([(geraLinha (Mapa largura t) n)] ++ t))


-- | Função encarregue de gerar uma nova linha com a escolha de um terreno e uma lista de obstáculos de forma aleatória, onde esta aleatoriedade será criada através da seed que seja dada como input.
geraLinha :: Mapa -> Int -> (Terreno,[Obstaculo])
geraLinha (Mapa largura t) n = (atribuiVelocidade ((gerarTerreno (Mapa largura t) n)) n,gerarObstaculos largura ((gerarTerreno (Mapa largura t) n),[]) n)

-- | Função que atribui uma velocidade gerada ao terreno gerado.
atribuiVelocidade :: Terreno -> Int -> Terreno
atribuiVelocidade (Estrada v) n = (Estrada (gerarVelocidade n))
atribuiVelocidade (Rio v) n = (Rio (gerarVelocidade n))
atribuiVelocidade Relva n = Relva

-- | Função que gera um valor de velocidade.
gerarVelocidade :: Int -> Int
gerarVelocidade n = head ((randomRs ((-2), 2) (mkStdGen n))) -- limites de velocidade facilmente alteráveis.

-- | Fução encarregue da escolha de um terreno 'aleatório' para a criação de uma nova linha na função estendeMapa.
gerarTerreno :: Mapa -> Int -> Terreno
gerarTerreno m n = getElemento n (proximosTerrenosValidos m)

-- | Função encarregue da escolha de uma lista de obstáculos 'aleatórios' para a criação de uma nova linha na função estendeMapa.
gerarObstaculos :: Int -> (Terreno, [Obstaculo]) -> Int -> [Obstaculo]
gerarObstaculos largura t n
    | (proximosObstaculosValidos largura t) == [] = []
    | largura > 0 = [getElemento n (proximosObstaculosValidos largura t)]++gerarObstaculos (largura-1) t (div n 2) -- (div n 2) utilizado para criar uma aleatoriedade maior
    | otherwise = []

-- | Função cujo propósito é escolher aleatóriamente um elemento de uma lista.
getElemento :: Int -> [a] -> a
getElemento n l = ((!!) l (genRandN n l))

-- | Função que gera de forma aleatória um número que esteja contido num intervalo de valores através do input de uma seed.
genRandN :: Int -> [a] -> Int
genRandN n l = head ((randomRs (0, length l -1) (mkStdGen n)))


-- | Possibilidades de terrenos que podem preencher nova linha do mapa.
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ [(Rio _, _),(Rio _, _),(Rio _, _),(Rio _, _),_]) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ [(Estrada _, _),(Estrada _, _),(Estrada _, _),(Estrada _, _),(Estrada _, _),_]) = [Rio 0, Relva]
proximosTerrenosValidos (Mapa _ [(Relva, _),(Relva, _),(Relva, _),(Relva, _),(Relva, _),_]) = [Rio 0, Estrada 0]
proximosTerrenosValidos (Mapa _ _) = [Rio 0, Estrada 0, Relva]


-- | Possibilidades de obstáculos que podem preencher nova linha do mapa.
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos 0 _ = []
proximosObstaculosValidos largura (Rio _,lista)
    | length lista == largura = []
    | length lista == (largura - 1) && elem Tronco lista == False = [Tronco]
    | otherwise = proximosObstaculosValidosRio lista
proximosObstaculosValidos largura (Estrada _,lista)
    | length lista == largura = []
    | length lista == (largura - 1) && elem Nenhum lista == False = [Nenhum]
    | otherwise = proximosObstaculosValidosEstrada lista
proximosObstaculosValidos largura (Relva,lista)
    | length lista == largura = []
    | length lista == (largura - 1) && elem Nenhum lista == False = [Nenhum]
    | otherwise = proximosObstaculosValidosRelva lista


-- | Verifica as possibilidades de obstáculos no caso do terreno ser Rio.
proximosObstaculosValidosRio :: [Obstaculo] -> [Obstaculo]
proximosObstaculosValidosRio lista = if validaTroncosSeguidos lista == True
                                    then [Nenhum,Tronco]
                                    else [Nenhum]

-- | Conta o número de troncos seguidos no final de uma lista de obstáculos.
contarTroncosRioSeguidos :: [Obstaculo] -> Int
contarTroncosRioSeguidos [] = 0
contarTroncosRioSeguidos lista = somaListasF(somaListas(troncosSeguidos lista))

-- | Valida a condição das 5 unidades ocupadas por um tronco.
validaTroncosSeguidos :: [Obstaculo] -> Bool
validaTroncosSeguidos lista
    | contarTroncosRioSeguidos lista >= 5 = False
    | otherwise = True

-- | Verifica as possibilidades de obstáculos no caso do terreno ser Estrada.
proximosObstaculosValidosEstrada :: [Obstaculo] -> [Obstaculo]
proximosObstaculosValidosEstrada lista = if validaCarrosSeguidos lista == True
                                        then [Nenhum,Carro]
                                        else [Nenhum]

-- | Conta o número de carros seguidos no final de uma lista de obstáculos.
contarCarrosEstradaSeguidos :: [Obstaculo] -> Int
contarCarrosEstradaSeguidos [] = 0 
contarCarrosEstradaSeguidos lista = somaListasF(somaListas (carrosSeguidos lista))


-- | Valida a condição das 3 unidades ocupadas por um carro.
validaCarrosSeguidos :: [Obstaculo] -> Bool
validaCarrosSeguidos lista
    | contarCarrosEstradaSeguidos lista >= 3 = False
    | otherwise = True

-- | Verifica as possibilidades de obstáculos no caso do terreno ser Relva.
proximosObstaculosValidosRelva :: [Obstaculo] -> [Obstaculo]
proximosObstaculosValidosRelva lista = [Nenhum,Arvore]

-- | Torna uma lista de obstáculos em uma lista de listas de Int, em que Carro corresponde a 1 e qualquer outro obstáculo corresponde a 0
carrosSeguidos :: [Obstaculo] -> [[Int]]
carrosSeguidos [] = []
carrosSeguidos l = group (carros l)

-- | Faz a soma de listas
somaListas :: [[Int]] -> [Int]
somaListas [] = []
somaListas (x:xs) = sum x: somaListas xs

-- | Retorna o maior número da lista
somaListasF :: [Int] -> Int
somaListasF [] = 0
somaListasF l = maiorNumero l

-- | Torna uma lista de obstáculos em uma lista de listas de Int, em que Tronco corresponde a 1 e qualquer outro obstáculo corresponde a 0
troncosSeguidos :: [Obstaculo] -> [[Int]]
troncosSeguidos [] = []
troncosSeguidos l = group (troncos l)
