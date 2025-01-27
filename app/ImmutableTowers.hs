{-|
Module      : immutableTowers
Description : Este módulo contém a definição de um tipo de dados que representa o estado do jogo e as suas funções de acesso.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Este módulo contém a definição de um tipo de dados que representa o estado do jogo e as suas funções de acesso.
-}
module ImmutableTowers where

import LI12425

import Graphics.Gloss


-- | Define ImutableTowers
data ImmutableTowers = ImmutableTowers {
    estadoJogo :: Menu,
    devolveJogo :: Jogo,
    modoJogo :: Modo ,
    imagens :: Imagens,
    desenhaLoja :: Bool,
    niveis :: [Jogo],
    progressoJogo :: Progresso,
    escalaAtual :: Float,
    tempoRecorde  :: Float,
    tempoJogo :: Float,
    botOn :: Bool
    } deriving (Show,Eq)


-- |Modo de Jogo atualmente a ser jogado
data Modo  = Levels
            |Sobrevivencia 
            deriving (Show,Eq)


-- | Tipo da imagem
type Imagens = [(String, Picture)]

-- | Define dimensão
data Dimensao = Dois 
            | Tres deriving Eq

-- | Define Progresso
data Progresso = Progresso
    { -- | Nível que o jogador se encontra atualmente (não coincide com o último nível desbloqueado, mas sim o nível que está escolhido na tela de níveis)
    nivelAtual :: Int,
    -- | Níveis que o jogador já desbloqueou
    niveisDesbloqueados :: [Jogo]
    } deriving (Show,Read,Eq)

-- | Define Menu
data Menu = Jogando 
        |MenuInicial
        |Ganhou
        |Perdeu 
        |Niveis
        |MenuInGame
        |TerminouJogo
        |ExplicaJogo 
        deriving (Show,Eq)    

-- | Tipo de DesenhaLoja, determia se a loja deve ser desenhada ou não 
type DesenhaLoja = Bool
