{- |
Module      : Tarefa6_2022li1g046
Description : Aplicação gráfica
Copyright   : Miguel Tomás Antunes Pinto <a100815@alunos.uminho.pt>
              Tomás Araújo Santos <a104005@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}
module Tarefa6_2022li1g046 where

import LI12223
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Tarefa1_2022li1g046
import Tarefa2_2022li1g046
import Tarefa3_2022li1g046
import Tarefa4_2022li1g046
import Tarefa5_2022li1g046
import Data.Fixed

-- | Opções que se encontram disponíveis no menu principal
data Opcao = Jogar
            |Instrucoes
            |Sair
-- | Diferentes estados do jogo
data Menu = Opcoes Opcao
            |ModoJogo -- estado durante o jogo
            |PaginaInstrucao -- estado enquanto o jogador se encontra no menu de instruções
            |VenceuJogo -- estado após o jogador perder o jogo

-- | Composição de um mundo/jogo e as suas propriedades/elementos
type World = (Menu, Jogo, Images, Time, Pontuação)

-- | Tipo do elemento da pontuação
type Pontuação = Float

-- | Tipo do elemento de tempo
type Time = Float

-- | Tipo das imagens que compõem o jogo
type Images = [Picture]

-- | Propriedades da janela do jogo

win :: Display 
win = InWindow 
        "Crossy Road" -- nome da janela
        (1100, 11000) -- dimensões da janela
        (180, 0) -- posição da janela


-- | frame rate que vai ser usado na função play
fps :: Int
fps = 50

-- | altura da janela em pixeis
altura :: Float
altura = 1100

-- | comprimento da janela em pixeis
comp :: Float
comp = 1100

-- | estado incial do jogo
initialState :: Images -> World
initialState images = (Opcoes Jogar, undefined, images, 0,0)

-- | jogo inicial que será utilizado para começar um novo jogo
jogoInicial :: Jogo
jogoInicial = (Jogo (Jogador (5,(7))) (Mapa 10 [(Estrada 2,[Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                                                        (Rio (-2),[Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Tronco]),
                                                                        (Relva, [Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                        (Rio 1, [Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco]),
                                                                        (Estrada 2,[Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro]),
                                                                        (Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),
                                                                        (Estrada (-1),[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                        (Relva, [Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore])]))

-- | Função que desenha um certo texto

drawOption option = Translate (-50) 0 $ Scale (0.19) (0.19) $ Text option


-- | Função responsável por converter um estado do jogo para algo gráfico.
desenhaEstado :: World -> Picture
desenhaEstado (VenceuJogo, jogo, images, n,p)  = Pictures [(images !! 19 ),Translate (-250) (-290) $ drawOption "Voltar ao Menu [M].", Translate 290 (-290) (images !! 4), Translate 220 (-200) $ drawOption "Sair do Jogo [Esc].", Translate (-400) (-280) (images !! 17), Translate 0 70 (images !! 20) , Translate (-330) (-75) $ Color red $ scale 0.5 0.5 $ Text  ("Conseguiu " ++ show (round p) ++ " pontos.")]
desenhaEstado (Opcoes Jogar, jogo, images,n,p)  = Pictures [(images !! 19 ),Translate (-20) (300) (images !! 1), Translate (-10) (100) (images !! 2), Translate (-10) (-70) (images !! 3), Translate (-10) (-240) (images !! 4), Translate (300) (110) (images !! 5),Translate (-400) (-300) (images !! 22)]
desenhaEstado (Opcoes Instrucoes, jogo, images,n,p)  = Pictures [(images !! 19 ),Translate (-20) (300) (images !! 1), Translate (-10) (100) (images !! 2), Translate (-10) (-70) (images !! 3), Translate (-10) (-240) (images !! 4), Translate (300) (-60) (images !! 5), Translate (-400) (-300) (images !! 22)]
desenhaEstado (Opcoes Sair, jogo, images,n,p)  = Pictures [(images !! 19 ),Translate (-20) (300) (images !! 1), Translate (-10) (100) (images !! 2), Translate (-10) (-70) (images !! 3), Translate (-10) (-240) (images !! 4), Translate (300) (-230) (images !! 5),Translate (-400) (-300) (images !! 22)]
desenhaEstado (PaginaInstrucao, jogo, images,n,p)  = Pictures [(images !! 19 ),Translate 0 (290) (images !! 18), Translate (-310) (120) $ scale 1.5 1.5 $ drawOption "Este jogo conseguiste na progressao no", Translate (-310) (60) $ scale 1.5 1.5 $ drawOption "mapa com o personagem, de forma a", Translate (-320) (0) $ scale 1.5 1.5 $ drawOption "evitar qualquer tipo de obstaculo, somando",Translate (-330) (-60) $ scale 1.5 1.5 $ drawOption "assim o maior numero de pontos possiveis." ,Translate (-300) (-180) $ scale 1.5 1.5 $ drawOption "Clica na tecla [M] para voltar ao Menu."]
desenhaEstado (ModoJogo, (Jogo (Jogador (x,y)) (Mapa l obs)), images, n,p) = Pictures ((desenhaMapa (Mapa l obs) 350 images)++[desenhaJogador (Jogador (x,y)) images] ++ [scoreboard images] ++ [score p] ++ [Translate 500 0 (images !! 21)] ++ [Translate 480 (-330) (images !! 23)])

-- | Scoreboard que pode ser visível do lado direito durante o decorrer do jogo.
scoreboard :: Images -> Picture
scoreboard images = translate 500 0 (images !! 13)

-- | Função que representa o score do jogador.
score :: Float -> Picture
score p = Translate 464 370 (scale 0.13 0.13 $ Text ("Score:" ++ show (round p)))

-- | Função responsável por converter um jogador e a sua posição para uma pictura.
desenhaJogador :: Jogador -> Images -> Picture
desenhaJogador (Jogador (x,y)) images = translate (a*100) (b*(-100)) (translate (-500) (350) (images !! 6))
                                        where
                                            a = fromIntegral x 
                                            b = fromIntegral y

-- | Função que é utilizada para a conversão de um certo tipo de terreno numa picture.
desenhaTerreno :: Terreno -> Images -> Picture
desenhaTerreno (Rio _) images = (images !! 11)
desenhaTerreno Relva images = (images !! 10)
desenhaTerreno (Estrada _) images = (images !! 12)

-- | Função utilizada para converter um obstáculo para uma picture
desenhaObstaculo :: Obstaculo -> Images -> Picture
desenhaObstaculo o images
        | o == Nenhum = Blank
        | o == Tronco = (images !! 7)
        | o == Carro = (images !! 8)
        | o == Arvore = (images !! 9)
        | o == CarroInv = Rotate 180 (images !! 8)

-- | Função utilizada para converter uma lista de  obstáculos para uma picture
desenhaObstaculos :: [Obstaculo] -> Float -> Images -> [Picture] -- Int remete para a largura do mapa
desenhaObstaculos (h:t) n images = translate (-n) 0 (desenhaObstaculo h images):desenhaObstaculos t (n-100) images
desenhaObstaculos [] _ _ = []

-- | Função auxiliar para desenhar os carros com uma rotação de 180 graus (utilizada quando a velocidade de um terreno do tipo estrada é negativa)
desenhaObstaculosContrario :: [Obstaculo] -> Float -> Images -> [Picture]
desenhaObstaculosContrario [] _ _ = []
desenhaObstaculosContrario (h:t) n images
    | h == Carro = translate (-n) 0 (desenhaObstaculo CarroInv images):desenhaObstaculosContrario t (n-100) images
    | otherwise = translate (-n) 0 (desenhaObstaculo Nenhum images):desenhaObstaculosContrario t (n-100) images


-- | Função que desenha uma linha uma linha do mapa (converte para picture)
desenhaLinha :: (Terreno, [Obstaculo]) -> Float -> Images -> [Picture] -- Int remete para a largura do mapa
desenhaLinha (Estrada v,obs) n images
    | v < 0 = [desenhaTerreno (Estrada v) images] ++ (desenhaObstaculosContrario obs 500 images)
    | otherwise = [desenhaTerreno (Estrada v) images] ++ (desenhaObstaculos obs 500 images)
desenhaLinha (t, obs) n images = [desenhaTerreno t images] ++ (desenhaObstaculos obs 500 images)

-- | Função que converte um dado para mapa para uma lista de pictures.
desenhaMapa :: Mapa -> Float -> Images -> [Picture]
desenhaMapa (Mapa l (h:t)) n images = (Translate 0 n (Pictures (desenhaLinha h n images)):desenhaMapa (Mapa l t) (n-100) images)
desenhaMapa (Mapa l []) n images = []


-- | Função que reage a diversos tipos de eventos (utilização de teclas etc..)
event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo, i, n,p) =
   (ModoJogo, jogoInicial,i,n,0)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Instrucoes, jogo, i, n,p) = 
    (PaginaInstrucao, jogo, i, n,p)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo, i, n,p) =
   error "Fim de Jogo"
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo, i, n,p) =
    (Opcoes Instrucoes, jogo, i, n,p)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo, i, n,p) =
    (Opcoes Sair, jogo, i, n,p)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Instrucoes, jogo, i, n,p) =
    (Opcoes Sair, jogo, i, n,p)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Instrucoes, jogo, i, n,p) =
    (Opcoes Jogar, jogo, i ,n,p)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo, i, n,p) =
    (Opcoes Jogar, jogo, i, n,p)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo, i, n,p) =
    (Opcoes Instrucoes, jogo, i, n,p)
event (EventKey (Char m) Down _ _) (PaginaInstrucao, jogo, i, n,p) = 
    (Opcoes Instrucoes, jogo, i, n,p)
event (EventKey (Char m) Down _ _) (ModoJogo, jogo, i, n,p) = 
    (Opcoes Jogar, jogo, i, n,p)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo j m) ,i,n,p) =
    (ModoJogo, (Jogo (moveJogador (Jogo j m) (Move Cima)) m),i,n,p)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo j m) ,i,n,p) = 
    (ModoJogo, (Jogo (moveJogador (Jogo j m) (Move Baixo)) m),i,n,p)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo j m) ,i,n,p) = 
    (ModoJogo, (Jogo (moveJogador (Jogo j m) (Move Esquerda)) m),i,n,p)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo j m) ,i,n,p) = 
    (ModoJogo, (Jogo (moveJogador (Jogo j m) (Move Direita)) m),i,n,p)
event (EventKey (Char m) Down _ _) (VenceuJogo, jogo, i,n,p) =
    (Opcoes Jogar,jogo,i,n,p)
event _ w = w


-- | Função que reage à passagem do tempo e que vai alterando o estado do jogo
step :: Float -> World -> World
step n (ModoJogo,(Jogo j (Mapa l obs)),imagens,t,p) = if jogoTerminou ((Jogo j (Mapa l obs))) then (VenceuJogo,(Jogo j (Mapa l obs)),imagens,t,p)
    else checkPos n (ModoJogo,(Jogo j (Mapa l obs)),imagens,t,p)
step _ w = w

-- | Função que verifica se o jogador se encontra na posição definida para que o mapa deslize.
checkPos :: Float -> World -> World
checkPos n (ModoJogo,(Jogo (Jogador (x,y)) (Mapa l obs)),imagens,t,p)
    | y == 2 = checkTime n (ModoJogo,(deslizaJogo (round p) (Jogo (Jogador (x,y)) (Mapa l obs))),imagens,t,p)
    | otherwise = checkTime n (ModoJogo,(Jogo (Jogador (x,y)) (Mapa l obs)),imagens,t,p)
        where time = round t


-- | Função que converte uma seed que não esteja contida no intervalo (0,100) para um valor pertencente a este
seedRange :: Int -> Int
seedRange p = if p >= 0 && p <= 100 then p
    else seedRange (div p 2)


-- | Função utilizada para atualizar o estado do jogo com o passar do tempo
checkTime :: Float -> World -> World
checkTime n (ModoJogo,(Jogo (Jogador (x,y)) (Mapa l obs)),imagens,t,p)
    | t == 200 = (ModoJogo,(deslizaJogo (round p) (Jogo (Jogador (x,y)) (Mapa l obs))),imagens,0,p+0.2)
    | (mod time 50 == 0) = (ModoJogo,(Jogo (moveJogador (Jogo (Jogador (x,y)) (Mapa l obs)) Parado) (Mapa l (atualizaMapa obs (Jogador (x,y)) 0))),imagens,t+1,p+0.2)
    | otherwise = (ModoJogo,(Jogo (Jogador (x,y)) (Mapa l obs)),imagens,t+1,p+0.2)
        where time = round t
checkTime _ w = w

{-
-- | Função que verifica se o jogador é atropelado durante o movimento dos obstáculos
verificaAtropelamento :: Jogo -> Bool
verificaAtropelamento (Jogo (Jogador (x,y)) (Mapa l obs)) = if (elem Carro (snd (linhaJogador (Jogo (Jogador (x,y)) (Mapa l obs))))) 
                                                            then jogoTerminou (Jogo (Jogador (x+1,y)) (Mapa l (atualizaMapa obs))) || jogoTerminou (Jogo (Jogador (x-1,y)) (Mapa l (atualizaMapa obs))) 
                                                            else False
-}

-- | Função principal responsável pela execução e compilação de todo o jogo
exec :: IO ()
exec = do
        pac_open <- loadBMP "pac_open.bmp"
        mainMenu <- loadBMP "mainMenuImage.bmp"
        start_Button <- loadBMP "Start-Button-Vector.bmp"
        instruction_Button <- loadBMP "instructions_Button.bmp"
        leave_Button <- loadBMP "leave_Button.bmp"
        arrow_Button <- loadBMP "arrow_Button.bmp"
        chicken <- loadBMP "chicken.bmp"
        tronco <- loadBMP "tronco.bmp"
        carro <- loadBMP "carro.bmp"
        arvore <- loadBMP "arvore.bmp"
        relva <- loadBMP "relva.bmp"
        rio <- loadBMP "rio.bmp"
        estrada <- loadBMP "estrada.bmp"
        scoreboard <- loadBMP "scoreboard.bmp"
        victory <- loadBMP "victory.bmp"
        medal <- loadBMP "first_place_1.bmp"
        trophy <- loadBMP "trophy.bmp"
        backMenu <- loadBMP "backMenu.bmp"
        objective <- loadBMP "objective.bmp"
        backGround <- loadBMP "background.bmp"
        gameOver <- loadBMP "gameOver.bmp"
        sideImage <- loadBMP "sideImage.bmp"
        creditsHorizontal <- loadBMP "creditsHorizontal.bmp"
        creditsVertical <- loadBMP "creditsVertical.bmp"
        let images = [scale 1.5 1.5 pac_open, -- imagem 0
                            scale 1.4 1.4 mainMenu, -- imagem 1
                            scale 0.5 0.5 start_Button, --imagem 2
                            scale 0.97 0.97 instruction_Button, --imagem 3
                            scale 0.503 0.503 leave_Button, --imagem 4
                            scale 0.7 0.7 arrow_Button,--imagem 5
                            scale 0.2 0.2 chicken, -- imagem 6
                            tronco, --imagem 7
                            carro, -- imagem 8
                            arvore, -- imagem 9
                            scale 2.4 1.5 relva, -- imagem 10
                            rio, --imagem 11
                            estrada, -- imagem 12
                            scoreboard, -- imagem 13
                            scale 0.9 0.9 victory, -- imagem 14
                            scale 0.3 0.3 medal, -- imagem 15
                            scale 0.5 0.5 trophy, -- imagem 16 
                            scale 0.4 0.4 backMenu, -- imagem 17
                            objective, --imagem 18
                            backGround, --imagem 19
                            scale 1.2 1.2 gameOver, --image 20
                            sideImage, --imagem 21
                            scale 1.3 1.3 creditsHorizontal, --imagem 22
                            creditsVertical] --imagem 23
        play 
            win (greyN 0.8)
            fps 
            (initialState images) -- define o estado inicial do jogo
            desenhaEstado -- desenha o estado do jogo
            event -- reage a um evento
            step-- reage ao passar do tempo
