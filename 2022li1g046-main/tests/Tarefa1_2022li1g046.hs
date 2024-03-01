{- |
Module      : Tarefa1_2022li1g046
Description : Validação de um mapa
Copyright   : Miguel Tomás Antunes Pinto <a100815@alunos.uminho.pt>
              Tomás Araújo Santos <a104005@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g046 where

import LI12223

-- | A função validaObstaculos avalia se certos obstaculos são validos no terreno escolhido

validarObstaculos :: Mapa -> Bool
validarObstaculos (Mapa l []) = True
validarObstaculos (Mapa l ((Relva,y):xs)) = if validarObstaculoRelva (Mapa l ((Relva,y):xs)) == False then False else validarObstaculos (Mapa l xs)
validarObstaculos (Mapa l ((Rio x,y):xs)) = if validarObstaculoRio (Mapa l ((Rio x,y):xs)) == False then False else validarObstaculos (Mapa l xs)
validarObstaculos (Mapa l ((Estrada x,y):xs)) = if validarObstaculoEstrada (Mapa l ((Estrada x,y):xs)) == False then False else validarObstaculos (Mapa l xs)

-- | A função validarObstaculoRelva avalia se certos obstaculos são validos para o terreno Relva

validarObstaculoRelva :: Mapa -> Bool
validarObstaculoRelva (Mapa l []) = False
validarObstaculoRelva (Mapa l ((Relva,y):xs)) = if (elem Carro y || elem Tronco y) then False else True

-- | A função validarObstaculoRelva avalia se certos obstaculos são validos para o terreno Rio

validarObstaculoRio :: Mapa -> Bool
validarObstaculoRio (Mapa l []) = False
validarObstaculoRio (Mapa l ((Rio x,y):xs)) = if (elem Carro y || elem Arvore y) then False else True

-- | A função validarObstaculoRelva avalia se certos obstaculos são validos para o terreno Estrada

validarObstaculoEstrada :: Mapa -> Bool
validarObstaculoEstrada (Mapa l []) = False
validarObstaculoEstrada (Mapa l ((Estrada x,y):xs)) = if (elem Arvore y || elem Tronco y) then False else True

-- | Esta função conta o número de obstáculos que existe numa lista.

contadorDeObstaculos :: [Obstaculo] -> Int
contadorDeObstaculos [] = 0
contadorDeObstaculos (x:xs) = 1 + contadorDeObstaculos xs

-- | Esta função faz a comparação final se existe tantos obstaculos como o número da largura do mapa.

contadorDeObstaculosF :: Mapa -> Bool
contadorDeObstaculosF (Mapa l []) = True
contadorDeObstaculosF (Mapa l ((o,y):xs)) = if contadorDeObstaculos y /= l then False else contadorDeObstaculosF (Mapa l xs)

-- | A função existirNenhum confirma se existe pelo menos um obstáculo 'Nenhum' na lista de Obstaculos.

existirNenhum :: [Obstaculo] -> Bool
existirNenhum [] = False
existirNenhum (x:xs) = if x /= Nenhum then existirNenhum xs else True

-- | A função existirNenhumL confirma se existe pelo menos um obstáculo 'Nenhum' numa lista de listas de Obstaculos.

existirNenhumL :: [[Obstaculo]] -> Bool
existirNenhumL [] = True
existirNenhumL (l:ys) = if existirNenhum l == True then existirNenhumL ys else False

-- | A função existirNenhumF confirma se num Mapa existe pelo menos um obstáculo 'Nenhum'.

existirNenhumF :: Mapa -> Bool
existirNenhumF (Mapa l []) = True
existirNenhumF (Mapa l ((o,y):xs)) = existirNenhumL(separarPares2 (((o,y):xs)))

-- | A função validarRio recebe uma lista de Terrenos, mais especificamente de Rios, e verifica se cada Rio tem um sentido diferente

validarRio :: [Terreno] -> Bool
validarRio [] = True
validarRio [Rio x] = True
validarRio [Estrada x] = True
validarRio [Relva] = True
validarRio (Rio x: Rio y:xs) = if (x >= 0 && y <0) || (x < 0 && y >=0) then True else False
validarRio (Rio x: Estrada y:xs) = validarRio xs
validarRio (Rio x : Relva:xs) = validarRio xs
validarRio (Relva : Rio x : xs) = validarRio (Rio x:xs)
validarRio (Estrada y : Rio x : xs) = validarRio (Rio x:xs)
validarRio (Estrada y : Relva : xs) = validarRio (Relva :xs)
validarRio (Estrada y : Estrada x : xs) = validarRio (Estrada x:xs)
validarRio (Relva : Relva : xs) = validarRio (Relva:xs)
validarRio (Relva : Estrada x : xs) = validarRio (Estrada x:xs)

-- | A função separarPares será uma função auxiliar à função validarRio para criar compatibilidade de dados, neste caso separar a parte da lista do Terreno

separarPares :: [(Terreno,[Obstaculo])] -> [Terreno]
separarPares [] = []
separarPares ((x,y):xs) = x : separarPares xs

-- | A função separarPares2 será uma função auxiliar para criar compatibilidade de dados, neste caso separar a parte da lista de lista de Obstaculos.

separarPares2 :: [(Terreno,[Obstaculo])] -> [[Obstaculo]]
separarPares2 [] = []
separarPares2 ((x,y):xs) = y : separarPares2 xs

-- | A função riosContrarios verifica se num mapa rios consecutivos têm sentidos opostos

riosContrarios :: Mapa -> Bool
riosContrarios (Mapa l ((o,y):xs)) = if validarRio(separarPares((o,y):xs)) == True then True else False

-- | A função carros recebe uma lista de Obstaculos e devolve uma lista de interios em que 1 corresponde aos Carros e o resto dos Obstaculos corresponde a 0.

carros :: [Obstaculo] -> [Int]
carros [] = []
carros [Carro] = [1]
carros (x:xs) = if x == Carro then 1 : carros xs else 0 : carros xs

-- | A função exemplo recebe uma lista de listas de Obstaculos e devolde uma lista de listas baseado na funcionalidade da função carros.

exemplo :: [[Obstaculo]] -> [[Int]]
exemplo [] = []
exemplo ((x:xs):ys) = (carros(x:xs)) : exemplo ys

-- | A função primenta recebe uma lista de listas de inteiros e devolve uma lista de listas de listas de inteiros que estão dividos entre 1's e 0's

primenta :: [[Int]] -> [[[Int]]]
primenta [] = []
primenta ((x:xs):ys) = group(x:xs) : primenta ys

-- | A função somaDeListas recebe uma lista de listas de listas e devolve uma lista de listas com as somas feitas entre os elementos das listas.

somaDeListas :: [[[Int]]] -> [[Int]]
somaDeListas [] = []
somaDeListas ((x:y):zs) = soma2(x:y) : somaDeListas zs

-- | A função quaseFinal recebe uma lista de listas e devolve uma lista de inteiros com o maior número de cada lista.

quaseFinal :: [[Int]] -> [Int]
quaseFinal [] = []
quaseFinal ((x:xs):ys) = maiorNumero(x:xs) : quaseFinal ys

-- | A função maiorNumero recebe uma lista de números e devolve o maior

maiorNumero :: [Int] -> Int
maiorNumero [] = 0
maiorNumero (x:xs) = if x > maiorNumero xs then x else maiorNumero xs

-- | A função group junta elementos iguais e consecutivos numa lista.

group :: [Int] -> [[Int]]
group [] = []
group [x] = [[x]]
group (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    | otherwise = [h] : r
    where r = group t

-- | A função soma2 recebe uma lista de listas e soma os elementos dentro da lista

soma2 :: [[Int]] -> [Int]
soma2 [] = []
soma2 (x:ys)= sum x : soma2 ys

-- | A função validaTamanhoCarro valida se o tamanho do Carro pode ou não existir

validaTamanhoCarro :: Mapa -> Bool
validaTamanhoCarro (Mapa l []) = True
validaTamanhoCarro (Mapa l ((o,y):xs)) = if maiorNumero(quaseFinal(somaDeListas(primenta(exemplo(separarPares2 ((o,y):xs)))))) > 3 then False else True  

-- | A função carros recebe uma lista de Obstaculos e devolve uma lista de 1's e 0's em que 1 corresponde aos Troncos e 0 corresponde aos outros Obstaculos.

troncos :: [Obstaculo] -> [Int] 
troncos [] = []
troncos (x:xs) = if x == Tronco then 1:troncos xs else 0 : troncos xs

-- | A função exemplo recebe uma lista de listas de Obstaculos e devolde uma lista de listas baseado na funcionalidade da função troncos.

exemplo2 :: [[Obstaculo]] -> [[Int]]
exemplo2 [] = []
exemplo2 ((x:xs):ys) = (troncos(x:xs)) : exemplo2 ys

-- | A função validaTamanhoCarro valida se o tamanho do Tronco pode ou não existir

validaTamanhoTronco :: Mapa -> Bool
validaTamanhoTronco (Mapa l []) = True
validaTamanhoTronco (Mapa l ((o,y):xs)) = if maiorNumero(quaseFinal(somaDeListas(primenta(exemplo2(separarPares2 ((o,y):xs)))))) > 5 then False else True  

-- | A função numeroDeRios recebe uma lista de Terrenos e devolve uma lista de interios em que 1 corresponde aos Rios e o resto dos Terrenos corresponde a 0.

numeroDeRios :: [Terreno] -> [Int]
numeroDeRios [] = []
numeroDeRios (Rio x:xs) = 1 : numeroDeRios xs
numeroDeRios (Estrada x:xs) = 0 : numeroDeRios xs
numeroDeRios (Relva:xs) = 0 : numeroDeRios xs 

-- | A função validaTamanhoRio valida se o número de Rios consecutivos pode existir

validaTamanhoRio :: Mapa -> Bool
validaTamanhoRio (Mapa l []) = True
validaTamanhoRio (Mapa l ((o,y):xs)) = if maiorNumero(soma2(group(numeroDeRios(separarPares ((o,y):xs))))) > 4 then False else True

-- | A função numeroDeRios recebe uma lista de Terrenos e devolve uma lista de interios em que 1 corresponde às Relva e o resto dos Terrenos corresponde a 0.

numeroDeRelvas :: [Terreno] -> [Int]
numeroDeRelvas [] = []
numeroDeRelvas (Rio x:xs) = 0 : numeroDeRelvas xs
numeroDeRelvas (Estrada x:xs) = 0 : numeroDeRelvas xs
numeroDeRelvas (Relva:xs) = 1 : numeroDeRelvas xs 

-- | A função validaTamanhoRelva valida se o número de Relvas consecutivos pode existir

validaTamanhoRelva :: Mapa -> Bool
validaTamanhoRelva (Mapa l []) = True
validaTamanhoRelva (Mapa l ((o,y):xs)) = if maiorNumero(soma2(group(numeroDeRelvas(separarPares ((o,y):xs))))) > 5 then False else True

-- | A função numeroDeRios recebe uma lista de Terrenos e devolve uma lista de interios em que 1 corresponde às Estradas e o resto dos Terrenos corresponde a 0.

numerodeEstrada :: [Terreno] -> [Int]
numerodeEstrada [] = []
numerodeEstrada (Rio x:xs) = 0 : numerodeEstrada xs
numerodeEstrada (Relva:xs) = 0 : numerodeEstrada xs
numerodeEstrada (Estrada x:xs) = 1 : numerodeEstrada xs

-- | A função validaTamanhoEstrada valida se o número de Estradas consecutivos pode existir

validaTamanhoEstrada :: Mapa -> Bool
validaTamanhoEstrada (Mapa l []) = True
validaTamanhoEstrada (Mapa l ((o,y):xs)) = if maiorNumero(soma2(group(numeroDeRelvas(separarPares ((o,y):xs))))) > 5 then False else True

-- | A função mapaValido valida se o Mapa recebido é ou não jogável.

mapaValido :: Mapa -> Bool
mapaValido (Mapa l []) = False
mapaValido m = validarObstaculos m && 
                                 contadorDeObstaculosF m && 
                                 existirNenhumF m && 
                                 riosContrarios m && 
                                 validaTamanhoCarro m && 
                                 validaTamanhoTronco m && 
                                 validaTamanhoRio m && 
                                 validaTamanhoRelva m && 
                                 validaTamanhoEstrada m
