{-|
Module      : Tempo
Description : Este módulo contém a função que reage ao passar do tempo no jogo 
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>

Este módulo contém a função que reage ao passar do tempo no jogo e também altera o estado do jogo em conformidade com a vitoria e a derrotas do jogador
-}
module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3
import Tarefa2
import BotActions
import Eventos

-- | Função que reage ao passar do tempo no jogo e altera o estado do jogo em conformidade com a vitoria e a derrotas do jogador
reageTempo t im  
    | estadoJogo im == Jogando && ganhouJogo (devolveJogo im) && modoJogo im == Levels = do writeFile "save.txt" (show (progressoJogo im){niveisDesbloqueados = niveisDesbloqueados (progressoJogo im) ++ [(daN (nivelAtual (progressoJogo im) +1) (niveis im)) ]})
                                                                                            return (im {estadoJogo = Ganhou, progressoJogo = (progressoJogo im){niveisDesbloqueados = niveisDesbloqueados (progressoJogo im) ++ [(daN (nivelAtual (progressoJogo im) +1) (niveis im)) ]}})                                                               
    | estadoJogo im == Jogando && perdeuJogo (devolveJogo im) && modoJogo im == Levels = return (im {estadoJogo = Perdeu})
    | estadoJogo im == Jogando && modoJogo im == Sobrevivencia && perdeuJogo (devolveJogo im) = do writeFile "record.txt" (show (defineRecorde  (tempoJogo im) (tempoRecorde im)))
                                                                                                   return (im {estadoJogo = TerminouJogo})
    | estadoJogo im == Jogando && modoJogo im == Sobrevivencia  && botOn im ==False = return (im {devolveJogo = atualizaJogo t (devolveJogo im), tempoJogo = tempoJogo im + t*v})
    | estadoJogo im == Jogando && modoJogo im == Sobrevivencia && botOn im == True = return (im {devolveJogo = botAction (atualizaJogo t (devolveJogo im)), tempoJogo = tempoJogo im + t*v})
    | estadoJogo im == Jogando && botOn im == False  = return im {devolveJogo = atualizaJogo t (devolveJogo im)}
    | estadoJogo im == Jogando && botOn im == True  = return im {devolveJogo = botAction (atualizaJogo t (devolveJogo im)) }
    | otherwise = return im 
    where Jogo {velocidadeJogo = v} = devolveJogo im
        

