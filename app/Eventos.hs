{-|
Module      : Eventos
Description : Este módulo contém a definição de funções para reagir a eventos no jogo.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Este módulo contém a definição de funções para reagir a eventos no jogo.
-}
module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers


import LI12425
import Tarefa3
import Tarefa2
import Nivel5
import MSobrevivencia
import Data.Maybe

import System.Exit 

-- | Função que permite o jogo reagir a eventos
reageEventos :: Event -> ImmutableTowers -> IO ImmutableTowers
-- |Quando se carrega no 'b' dentro dos Niveis bolqueamos o nível atual 
reageEventos (EventKey (Char 'b') Down _ _) im 
    |estadoJogo im == Niveis  = return (im { progressoJogo = (progressoJogo im){niveisDesbloqueados =remove (niveisDesbloqueados (progressoJogo im)) (daN (nivelAtual (progressoJogo im) ) (niveis im)) }})
    |otherwise = return im
-- |Quando se carrega no 'g' ativa-se/desativa-se o boot
reageEventos (EventKey (Char 'g') Down _ _) im = return (im {botOn = not (botOn im)})
-- |Quando se carrega no 'c' dentro dos Niveis desbolqueamos o nível atual 
reageEventos (EventKey (Char 'c') Down _ _) im 
    |estadoJogo im == Niveis = return (im { progressoJogo = (progressoJogo im){niveisDesbloqueados = niveisDesbloqueados (progressoJogo im) ++ [(daN (nivelAtual (progressoJogo im) ) (niveis im)) ]}})
    |otherwise = return im
-- |Quando se carrega no 'p' abrimos e fechamos a loja, apenas dentro do jogo 
reageEventos (EventKey (Char 'p') Down _ _) im 
    |estadoJogo im == Jogando = return ( im {desenhaLoja = not (desenhaLoja im)})
    |otherwise =     return im 
-- | O botão esc serve para sair do jogo e guardar o progresso 
reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) im = 
      do writeFile "record.txt" (show (defineRecorde  (tempoJogo im) (tempoRecorde im)))
         writeFile "save.txt" (show (progressoJogo im))
         exitSuccess  
-- |Quando se carrega na seta da esquerda dentro dos níveis, voltamos para o nível anterior
reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) im 
    |estadoJogo im == Niveis = return  im{progressoJogo = (progressoJogo im) {nivelAtual = nivelAtual (progressoJogo im) -1}}
    |otherwise = return im 
-- |Quando se carrega na seta da direita dentro dos níveis, avançamos para o nível seguinte
reageEventos (EventKey (SpecialKey KeyRight) Down _ _) im 
    |estadoJogo im == Niveis =  return  im{progressoJogo = (progressoJogo im) {nivelAtual = nivelAtual (progressoJogo im) +1}}
    |otherwise = return im 
-- |Quando o rato se move atualizamos e sua posicão e adequamos à escala atual
reageEventos (EventMotion (x,y)) im =return $  im {devolveJogo = (devolveJogo im) {posicaoRato = ((1/(escalaAtual im))*x,(1/(escalaAtual im))*y)}}
-- |Quando se carrega no TAB muda-se a perspetiva/dimensão do jogo
reageEventos (EventKey (SpecialKey KeyTab) Down _ _) im 
    |estadoJogo im == Jogando = return im {devolveJogo = (devolveJogo im) {terceiraDimensao = not (terceiraDimensao (devolveJogo im))}}
    |otherwise = return im
-- |Parte do código que lida com as interações com o botão esquerdo do rato 
reageEventos (EventKey (MouseButton LeftButton) Down _ _) im@(ImmutableTowers {devolveJogo =j@( Jogo {baseJogo = base, lojaJogo= loja,torreSelecionada = torreSel, portaisJogo = portais,torresJogo = torres, mapaJogo = mapa, posicaoRato = mousePos,terceiraDimensao = e3D, torreAMelhorar = torreAMelhorar, pregosJogo = pregos,pregoSelecionado = pregoSel, boostJogo = boost})})  
    |estadoJogo im== Jogando && desenhaLoja im && pregoSel == Nothing && torreSel1 /= Nothing  =  return $ im {devolveJogo = j{baseJogo = base1, torreSelecionada = torreSel1}, desenhaLoja = False}
    |estadoJogo im == Jogando && estaDentroPausa ((posicaoRato.devolveJogo)im) = return im{estadoJogo = MenuInGame}
    |estadoJogo im == Jogando && estaDentroAcelera ((posicaoRato.devolveJogo)im) = return im{devolveJogo = (devolveJogo im) {velocidadeJogo = case velocidadeJogo (devolveJogo im) of 
                                                                                                                                                                        4 -> 4 
                                                                                                                                                                        _ -> velocidadeJogo (devolveJogo im)+ 0.25}}
    |estadoJogo im == Jogando && estaDentroTrava ((posicaoRato.devolveJogo)im) = return im{devolveJogo = (devolveJogo im) {velocidadeJogo = case velocidadeJogo (devolveJogo im) of 
                                                                                                                                                                        1 -> 1 
                                                                                                                                                                        _ -> velocidadeJogo (devolveJogo im)- 0.25}}
                                                                                                                                                                            |estadoJogo im == Jogando && daTorreEmJogo e3D mapa mousePos torres /= Nothing = return im {devolveJogo = j {torreAMelhorar = daTorreEmJogo e3D mapa mousePos torres}}
    |estadoJogo im == Jogando && daTorreEmJogo e3D mapa mousePos torres /= Nothing = return im {devolveJogo = j {torreAMelhorar = daTorreEmJogo e3D mapa mousePos torres}}
    |estadoJogo im == Jogando && torreAMelhorar /= Nothing && estaDentroEvDano mousePos = return im {devolveJogo = j {torresJogo = torresD, baseJogo = baseD,torreAMelhorar = Nothing}}
    |estadoJogo im == Jogando && torreAMelhorar /= Nothing && estaDentroEvAlcance mousePos =  return im {devolveJogo = j {torresJogo = torresA, baseJogo = baseA,torreAMelhorar = Nothing}}
    |estadoJogo im == Jogando && torreAMelhorar /= Nothing && estaDentroEvRajada mousePos =  return im {devolveJogo = j {torresJogo = torresR, baseJogo = baseR,torreAMelhorar = Nothing}}
    |estadoJogo im == Jogando && torreAMelhorar /= Nothing && estaDentroEvCiclo mousePos =  return im {devolveJogo = j {torresJogo = torresC, baseJogo = baseC,torreAMelhorar = Nothing}}
    |estadoJogo im == Jogando && torreAMelhorar /= Nothing && estaDentroEvVender mousePos = return im {devolveJogo = j {torresJogo = torresV, baseJogo = baseV,torreAMelhorar = Nothing}}
    |estadoJogo im == Jogando && torreAMelhorar /= Nothing &&  sairDasEvolucoes mousePos = return im {devolveJogo = j {torreAMelhorar = Nothing}} 
    |estadoJogo im == Jogando && estaDentroPergunta mousePos = return im {estadoJogo = ExplicaJogo}
    |estadoJogo im == Jogando && not (ativoBoost boost) && numeroBoost boost > 0  && estaDentroBoost mousePos = return im {devolveJogo = j {boostJogo = boost {ativoBoost = True, numeroBoost = numeroBoost (boostJogo j) -1, velocidadeBoost = velocidadeCBoost (boostJogo j)}}} 
    |estadoJogo im == Jogando && pregoEscolhido /=Nothing && torreSel == Nothing && desenhaLoja im  = return im {devolveJogo = j {pregoSelecionado  = pregoEscolhido, baseJogo = baseSP}, desenhaLoja = False} 
    |estadoJogo im == Jogando && pregoSel /= Nothing = return im {devolveJogo = j {pregoSelecionado = pregoSelA, pregosJogo = pregosA}}                                                                                                                                                                  
    |estadoJogo im == Jogando && not (desenhaLoja im) && torreSel /= Nothing  = return im {devolveJogo = j{torresJogo = torres1, torreSelecionada = torreSel2}}
    |estadoJogo im == Ganhou && estaDentroButPAW ((posicaoRato.devolveJogo) im) = return im {devolveJogo = daN (nivelAtual (progressoJogo im)) (niveis im) , estadoJogo= Jogando}
    |estadoJogo im == Ganhou && estaDentroButBMW ((posicaoRato.devolveJogo) im) = return im {estadoJogo = MenuInicial}     
    |estadoJogo im == Ganhou && estaDentroNextLevel((posicaoRato.devolveJogo) im) && daN (nivelAtual (progressoJogo im)) (niveis im)/= jogoN5 = return im {estadoJogo = Jogando, progressoJogo = (progressoJogo im) {nivelAtual = nivelSeguinte} , devolveJogo = daN nivelSeguinte (niveis im) }     
    |estadoJogo im == Perdeu && estaDentroButPAD ((posicaoRato.devolveJogo) im) = return im {devolveJogo =daN (nivelAtual (progressoJogo im)) (niveis im), estadoJogo= Jogando}
    |estadoJogo im == Perdeu && estaDentroButBMD ((posicaoRato.devolveJogo) im) = return im {estadoJogo = MenuInicial}     
    |estadoJogo im == TerminouJogo && estaDentroMenuTJ ((posicaoRato.devolveJogo) im) = return im {estadoJogo = MenuInicial, tempoRecorde = defineRecorde (tempoJogo im) (tempoRecorde im)}
    |estadoJogo im == TerminouJogo && estaDentroPA ((posicaoRato.devolveJogo) im) = return im {estadoJogo = Jogando, devolveJogo = jogoS, tempoJogo = 0, tempoRecorde = defineRecorde (tempoJogo im) (tempoRecorde im)}
    |estadoJogo im == MenuInicial && estaDentroExit ((posicaoRato.devolveJogo) im) =
         do writeFile "record.txt" (show (tempoRecorde im))
            writeFile "save.txt" (show (progressoJogo im))
            exitSuccess     
    |estadoJogo im == MenuInicial && estaDentroLevel ((posicaoRato.devolveJogo) im) = return im {estadoJogo = Niveis}  
    |estadoJogo im == MenuInicial && estaDentroMenorR ((posicaoRato.devolveJogo) im) = return im {escalaAtual = 0.75}
    |estadoJogo im == MenuInicial && estaDentroMaiorR ((posicaoRato.devolveJogo) im) = return im {escalaAtual = 1}
    |estadoJogo im == MenuInicial && estaDentroSurvivalMode ((posicaoRato.devolveJogo) im) = return im {estadoJogo = Jogando, devolveJogo = jogoS, tempoJogo = 0, modoJogo = Sobrevivencia}  
    |estadoJogo im == Niveis && estaDentroEsquerda ((posicaoRato.devolveJogo) im) = return im {progressoJogo = (progressoJogo im) {nivelAtual = nivelAnterior}}
    |estadoJogo im == Niveis && estaDentroDireita ((posicaoRato.devolveJogo) im) = return im {progressoJogo = (progressoJogo im) {nivelAtual = nivelSeguinte}}
    |estadoJogo im == Niveis && estaDentroPlayNow ((posicaoRato.devolveJogo) im)= verificaNivel im
    |estadoJogo im == Niveis && estaDentroMenuN ((posicaoRato.devolveJogo) im) = return im {estadoJogo = MenuInicial}
    |estadoJogo im == MenuInGame && estaDentroStart ((posicaoRato.devolveJogo) im) = return im {estadoJogo = Jogando}
    |estadoJogo im == MenuInGame && estaDentroQuit ((posicaoRato.devolveJogo) im) = return im {estadoJogo = Niveis}
    |estadoJogo im == MenuInGame && estaDentroMenu ((posicaoRato.devolveJogo) im) = return im {estadoJogo = MenuInicial}
    |estadoJogo im == ExplicaJogo && estaDentroSairEJ ((posicaoRato.devolveJogo) im) = return im {estadoJogo = Jogando}
    |otherwise = return im 
    where 
        (pregosA,pregoSelA) = colocaPrego base portais e3D mapa (fromJust pregoSel) mousePos pregos 
        (pregoEscolhido,baseSP) = selecionaPrego base mousePos 
        (baseR, torresR) = evoluiTorre ((fromJust torreAMelhorar) {tipoEvolucao = Rajada}) base torres
        (baseC, torresC) = evoluiTorre ((fromJust torreAMelhorar) {tipoEvolucao = Ciclo}) base torres
        (baseA,torresA) = evoluiTorre ((fromJust torreAMelhorar) {tipoEvolucao = Alcance}) base torres
        (baseD,torresD) = evoluiTorre ((fromJust torreAMelhorar) {tipoEvolucao = Dano}) base torres
        (baseV, torresV) = venderTorre (fromJust torreAMelhorar) base torres
        nivelAnterior = nivelAtual (progressoJogo im) -1
        nivelSeguinte = nivelAtual (progressoJogo im) +1
        (torreSel1,base1) = selecionaTorre mousePos loja base 
        (torreSel2,torres1) = colocaTorre (terceiraDimensao (devolveJogo im)) torreSel mousePos mapa torres        
-- |Qualquer outro evento não altera o estado do jogo 
reageEventos _ im =return  im 


-- | Função responsável por definir o recorde do jogo 
defineRecorde :: Float -> Float -> Float
defineRecorde t x = if t > x then t else x

-- | Função que verifica se a posição do rato está dentro do botão de back to menu da vitória
estaDentroButBMW :: Posicao -> Bool 
estaDentroButBMW (x,y) = x >= -15 - buttonsWW  && x<= -15
                    && y <= -475.75 && y >= -475.75  -buttonsHW
                         
-- | Função que verifica se a posição do rato está dentro do botão de play again da vitória 
estaDentroButPAW :: Posicao -> Bool 
estaDentroButPAW (x,y) = x <= 65 + buttonsWW && x>= 65
                    && y <= -475.75 && y >= -475.75  - buttonsHW

-- |Altura dos botões da vitória 
buttonsHW:: Float 
buttonsHW = 208.5

-- |Largura dos botões da vitória
buttonsWW:: Float
buttonsWW = 320

-- | Função que verifica se a posição do rato está dentro do botão de back to menu da derrota
estaDentroButBMD :: Posicao -> Bool 
estaDentroButBMD (x,y) = x >= -70 - buttonsWD  && x<= -70
                    && y <= -320 && y >= -320  -buttonsHD

-- | Função que verifica se a posição do rato está dentro do botão de play again da derrota        
estaDentroButPAD :: Posicao -> Bool 
estaDentroButPAD (x,y) = x <= 86 + buttonsWD && x>= 86
                    && y <= -320 && y >= -320  - buttonsHD

-- |Altura dos botões da derrota
buttonsHD:: Float 
buttonsHD = 208.5

-- |Largura dos botões da derrota
buttonsWD:: Float
buttonsWD = 400

-- | Função que verifica se a posição do rato está dentro do botão de level do menu inicial
estaDentroLevel :: Posicao -> Bool 
estaDentroLevel (x,y) = x >= -buttonsMenuW/2   && x<= buttonsMenuW/2
                    && y <= -150 + buttonsMenuH/2 && y >= -150  -buttonsMenuH/2

-- | Função que verifica se a posição do rato está dentro do botão para entrar no survival mode do menu inicial      
estaDentroSurvivalMode :: Posicao -> Bool 
estaDentroSurvivalMode  (x,y) = x >= -buttonsMenuW/2  && x<= buttonsMenuW/2
                    && y <= -340 + buttonsMenuH/2 && y >= -340  -buttonsMenuH/2

-- | Função que verifica se a posição do rato está dentro do botão de exit do menu inicial      
estaDentroExit :: Posicao -> Bool 
estaDentroExit (x,y) = x >= -buttonsMenuW/2  && x<= buttonsMenuW/2
                    && y <= -530 + buttonsMenuH/2 && y >= -530  -buttonsMenuH/2
    
-- | Função que verifica se a posição do rato está dentro do botão de play now do menu de níveis
estaDentroPlayNow ::Posicao -> Bool 
estaDentroPlayNow (x,y) = x >= -buttonsMenuW/2  && x<= buttonsMenuW/2
                    && y <= -425 && y >= -425  -buttonsMenuH
   
-- |Função que devolve a altura dos botãos do menu             
buttonsMenuH :: Float 
buttonsMenuH = 150

-- |Função que devolve a largura dos botãos do menu
buttonsMenuW :: Float 
buttonsMenuW = 340

-- |Função que devolve o raio dos botãos redondos 
raioBM :: Float 
raioBM = 65

-- |Função que verifica se a posição do rato está dentro do botão de passar os niveis para trás
estaDentroEsquerda :: Posicao -> Bool 
estaDentroEsquerda pos = distancia pos (-1120,0) <= raioBM 

-- |Função que verifica se a posição do rato está dentro do botão de passar os niveis para a frente
estaDentroDireita :: Posicao -> Bool 
estaDentroDireita pos = distancia pos (1120,0) <= raioBM

-- |Função que verifica se a posição do rato está dentro do botão de pausa
estaDentroPausa :: Posicao -> Bool 
estaDentroPausa pos = distancia pos (1080,625) <= raioBM/2

-- |Função que verifica se a posição do rato está dentro do botão de acelerar a velocidade do  jogo
estaDentroAcelera :: Posicao -> Bool 
estaDentroAcelera pos = distancia pos (1010,625) <= raioBM/2

-- |Função que verifica se a posição do rato está dentro do botão de travar a velocidade do jogo
estaDentroTrava :: Posicao -> Bool 
estaDentroTrava pos = distancia pos (940,625) <= raioBM/2

-- |Função que verifica se a posição do rato está dentro do botão de start do menu in game
estaDentroStart :: Posicao -> Bool
estaDentroStart (x,y) = x >= -buttonsMenuW/2  && x<= buttonsMenuW/2
                    && y <= 300+buttonsMenuH/2 && y >=300+buttonsMenuH/2  -buttonsMenuH

-- |Função que verifica se a posição do rato está dentro do botão de quit do menu in game
estaDentroQuit :: Posicao -> Bool
estaDentroQuit (x,y) = x >= -buttonsMenuW/2  && x<= buttonsMenuW/2
                    && y <= buttonsMenuH/2 && y >=buttonsMenuH/2  -buttonsMenuH


-- |Função que verifica se a posição do rato está dentro do botão de menu do menu in game
estaDentroMenu :: Posicao -> Bool
estaDentroMenu (x,y) = x >= -buttonsMenuW/2  && x<= buttonsMenuW/2
                    && y <= -300+buttonsMenuH/2 && y >= -300+buttonsMenuH/2  -buttonsMenuH

-- |Função que verifica se a posicação do rato se enontra dentro do botão de menu na tela dos níves 
estaDentroMenuN :: Posicao -> Bool
estaDentroMenuN (x,y) = x >= 1120 -buttonsMenuW/2 *0.8 && x<= 1120 + buttonsMenuW/2*0.8
                    && y <= -620+buttonsMenuH/2*0.8 && y >= -620-buttonsMenuH/2 *0.8


-- |Função responsável por verificar se o nivel está desbloqueado e se sim, devolver o jogo correspondente
verificaNivel :: ImmutableTowers -> IO ImmutableTowers
verificaNivel im 
    |estaDesbolqueado (niveis im) (progressoJogo im) = return im {estadoJogo = Jogando, devolveJogo = daN  (nivelAtual (progressoJogo im)) (niveis im), modoJogo = Levels} 
    |otherwise = return im
    where estaDesbolqueado :: [Jogo] -> Progresso -> Bool
          estaDesbolqueado niveis progresso = elem (daN (nivelAtual progresso) niveis) (niveisDesbloqueados progresso)



-- |Função que remove um jogo de uma lista de jogos, usada para bloquear níveis
remove :: [Jogo] -> Jogo -> [Jogo]
remove jogos jogo = filter (/=jogo) jogos

-- | Constante que representa a altura dos botões de evolução
buttonsEvH :: Float 
buttonsEvH = 334.6

-- | Constante que representa a largura dos botões de evolução
buttonsEvW :: Float 
buttonsEvW = 371

-- | Função que verifica se uma posição está dentro do botão de evolução de dano
estaDentroEvDano :: Posicao -> Bool 
estaDentroEvDano (x,y)  = x >= -1046- (buttonsEvW/2) && x <= -1046 + (buttonsEvW/2)
                        && y >= -540 - (buttonsEvH/2) && y <= -540 + (buttonsEvH/2)

-- | Função que verifica se uma posição está dentro do botão de evolução de alcance
estaDentroEvAlcance :: Posicao -> Bool 
estaDentroEvAlcance (x,y)  = estaDentroEvDano (x-372-50,y)

-- | Função que verifica se uma posição está dentro do botão de evolução de rajada
estaDentroEvRajada :: Posicao -> Bool 
estaDentroEvRajada (x,y)  = estaDentroEvAlcance (x-372-50,y)

-- | Função que verifica se uma posição está dentro do botão de evolução de ciclo
estaDentroEvCiclo :: Posicao -> Bool 
estaDentroEvCiclo (x,y)  =estaDentroEvRajada (x-372-50,y)

-- | Função que verifica se uma posição está dentro do botão de vender
estaDentroEvVender :: Posicao -> Bool 
estaDentroEvVender (x,y)  = estaDentroEvCiclo (x-372-50,y)

-- | Função que verifica se uma posição está dentro do botão de sair das evoluções ou fora da área de evoluções
sairDasEvolucoes :: Posicao -> Bool 
sairDasEvolucoes (x,y) = y >= -282.5 || distancia (x,y) (1200,-373) <= 34.2

-- | Função que verifica se uma posição está dentro do botão de proximo nivel (Next Level)
estaDentroNextLevel :: Posicao -> Bool 
estaDentroNextLevel (x,y) = x >= 30 -174.15 && x <= 30 + 174.75
                        && y >= -350 -104.25 && y <= -350 +104.35

        
-- | Função que verifica se uma posição está dentro do botão de Boost 
estaDentroBoost :: Posicao -> Bool 
estaDentroBoost pos = distancia (800, 630) pos <= 56.6

-- | Função que verifica se uma posição está dentro do botão de Play Again da tela de terminou o jogo
estaDentroPA :: Posicao -> Bool 
estaDentroPA (x,y) = x >= -buttonsMenuW/2  && x<= buttonsMenuW/2
                    && y <= -300+ buttonsMenuH/2  && y >= -300 -buttonsMenuH/2
                    
-- | Função que verifica se uma posição está dentro do botão de Menu da tela de terminou o jogo
estaDentroMenuTJ :: Posicao -> Bool 
estaDentroMenuTJ (x,y) = x >= -buttonsMenuW/2  && x<= buttonsMenuW/2
                    && y <= -500+ buttonsMenuH/2  && y >= -500 -buttonsMenuH/2

-- | Função que verifica se uma posição está dentro do botão de pergunta durante o jogo 
estaDentroPergunta :: Posicao -> Bool 
estaDentroPergunta (x,y) = x >= 1200 -55.2 && x <= 1200 + 55.2
                        && y >= -650 -44.4 && y <= -650 +44.4

-- |Função que verifica se uma posição está do local que permite sair da tela de ExplicaJogo
estaDentroSairEJ  :: Posicao -> Bool
estaDentroSairEJ (x,y) = y <= -620 || y >= 620  || distancia (550,580) (x,y) <= 34.2 
            || x <= -580 || x >= 580

-- |Função que verifica se uma posição dentro da menor resolucao 
estaDentroMenorR :: Posicao -> Bool
estaDentroMenorR (x,y) = x >= -200 -buttonsMenuW/2  && x<= -200 + buttonsMenuW/2
                    && y <= 50+ buttonsMenuH/2  && y >= 50 -buttonsMenuH/2

-- |Função que verifica se uma posição dentro da maior resolucao 
estaDentroMaiorR :: Posicao -> Bool
estaDentroMaiorR (x,y) = x >= 200 -buttonsMenuW/2  && x<= 200 + buttonsMenuW/2
                    && y <= 50+ buttonsMenuH/2  && y >= 50 -buttonsMenuH/2
