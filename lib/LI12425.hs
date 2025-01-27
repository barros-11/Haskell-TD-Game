{-|
Module      : LI12425
Description : Definições base do jogo
Copyright   : Nelson Estevão <d12733@di.uminho.pt>
              Olga Pacheco   <omp@di.uminho.pt>
              Pedro Peixoto  <d14110@di.uminho.pt>
              Xavier Pinho   <d12736@di.uminho.pt>

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2024/25.
-}
module LI12425 (
    -- * Tipos de dados
    -- ** Básicos
    Creditos, Direcao(..), Distancia, Duracao(..), Posicao, Semente, Tempo,Velocidade,
    -- ** Mapas
    Mapa, Terreno(..),
    -- ** Entidades
    Base(..), Torre(..), Portal(..), Inimigo(..), TipoProjetil(..), Projetil(..),Tiro, Evolucao (..), Prego (..), Boost (..), 
    -- ** Jogo
    Jogo(..), Onda(..), Loja,
    -- * Funções auxiliares
    geraAleatorios
    ) where

import System.Random (mkStdGen, randoms)

-- | Tipo de terreno do mapa.
data Terreno
  = -- | Torres constroem-se sobre o relvado do mapa.
    Relva
  | -- | A base e os portais constroem-se sobre caminhos de terra do mapa. Além disso, inimigos movem-se sobre estes terrenos.
    Terra
  | -- | Água para efeito decorativo, mas onde não se pode construir, nem os inimigos se podem mover.
    Agua
  |  -- | Terreno que é colocado no mesmo local que a Terra (ou seja, é por onde os inimigos andam, no entanto, os portais, bases e pregos não se podem encontrar sobre este tipo de terreno, eles permanecem na Terra), e serve de camuflagem para os inimigos (não podem ser disparados )
    Cimento 
  deriving (Eq, Show,Read)

-- | Mapa do jogo composto por uma matriz de terrenos.
type Mapa = [[Terreno]]

-- | Coordenada bilateral de uma entidade no jogo, representante do seu centro.
-- O referencial tem origem no canto superior esquerdo, com eixos x e y positivos para a direita e para baixo, respectivamente.
type Posicao = (Float, Float)

-- |Velocidade com componentes x e y
type Velocidade = (Float,Float)

-- | Projétil disparado por uma torre, com posicao atual, posicao final, velocidade e tipo de projétil.
type Tiro = (Posicao,Posicao,Velocidade, TipoProjetil)

-- | Moeda do jogo.
type Creditos = Int

-- | Base de operações do jogador.
data Base = Base
  { -- | Vida da base. Quando esta chega a zero, o jogador perde o jogo.
    vidaBase :: Float,
    -- | Posição da base no mapa. A base deve estar sobre um terreno de terra.
    posicaoBase :: Posicao,
    -- | Balanço de créditos do jogador.
    creditosBase :: Creditos,
    -- |Número de pregos que o jogador tem disponíveis 
    pregosBase :: Int
  }
  deriving (Show, Eq,Read)

-- | Distância entre duas posições.
type Distancia = Float

-- | Tempo em segundos.
type Tempo = Float

-- | Representa uma duração em segundos
data Duracao 
  = -- | Duração em segundos
    Finita Tempo
  | -- | Duração infinita
    Infinita
  deriving (Eq, Show, Ord,Read)

-- | Torre que dispara projéteis contra inimigos.
data Torre = Torre
  { -- | Posição da torre no mapa.
    posicaoTorre :: Posicao,
    -- | Redução de vida no inimigo pelo impacto do projétil.
    danoTorre :: Float,
    -- | Alcance circular da torre.
    alcanceTorre :: Float,
    -- | Número de máximo de inimigos simultaneamente atingidos por uma rajada de tiros.
    rajadaTorre :: Int,
    -- | Ciclo de tempo entre rajadas de tiros.
    cicloTorre :: Tempo,
    -- | Tempo restante para a próxima rajada de tiros.
    tempoTorre :: Tempo,
    -- | Efeito secundário associado ao tipo de projétil da torre.
    projetilTorre :: Projetil,
    -- | Nível em que a torre atualmente se encontra (as torres começam no nível 1 e o nível máximo para uma torre é 3).
    nivelTorre :: Int,
    -- | Tipos de evolucao possível para a torre (o custo da evolução está associado ao tipo pela função associaCusto)
    tipoEvolucao :: Evolucao
  }
  deriving (Show,Eq,Read)

-- | Tipos de evolução possíveis para uma torre
data Evolucao = 
  -- |Evolução que aumenta o alcance da torre
  Alcance 
  | -- |Evolução que aumenta o dano da torre 
  Dano 
  | -- |Evolução que diminui o tempo entre rajadas de tiros
  Rajada 
  | -- |Evolução que diminui o tempo de cada ciclo de tiro
  Ciclo 
  | -- |Quando a torre não tem evolução
  Nada 
  deriving (Show,Eq,Read)

-- | Loja de torres disponíveis para construir por uma quantidade de créditos.
type Loja = [(Creditos, Torre)]

-- |Pregos que são colocados no mapa para atrasar/eliminar os inimigos (eles são gratuitos e o número de pregos é limitado,são uma ferramente que ajuda o jogador, no entanto, os inimigos mortos pelos prego não dão dinheiro. Além disso apenas são colocados na Terra e sem ser nos locais dos portais e da base)
data Prego = Prego
  { -- |Posição do prego no mapa 
    posicaoPrego :: Posicao,
    -- | Vida do prego 
   vidaPrego :: Float
  } deriving (Eq,Show,Read)

-- | Tipo de projétil disparado por uma torre.
data TipoProjetil = Fogo | Gelo | Resina
  deriving (Eq, Show,Read)

-- | Projétil aplicado por uma torre.
data Projetil = Projetil
  { -- | Tipo de projétil.
    tipoProjetil :: TipoProjetil,
    -- | Duração do efeito do projétil no inimigo.
    duracaoProjetil :: Duracao
  }
  deriving (Show,Eq,Read)

-- | Direção de movimento de uma entidade no jogo.
data Direcao
  = Norte
  | Sul
  | Este
  | Oeste
  deriving (Eq, Show,Read)

-- | Inimigo que se move em direção à base do jogador.
data Inimigo = Inimigo
  { -- | Posição do inimigo no mapa.
    posicaoInimigo :: Posicao,
    -- | Direção do último movimento do inimigo.
    direcaoInimigo :: Direcao,
    -- | Vida do inimigo.
    vidaInimigo :: Float,
    -- | Velocidade do inimigo.
    velocidadeInimigo :: Float,
    -- | Dano causado pelo inimigo na base do jogador.
    ataqueInimigo :: Float,
    -- | Créditos que o jogador recebe ao derrotar o inimigo.
    butimInimigo :: Creditos,
    -- | Efeitos secundários ativos no inimigo.
    projeteisInimigo :: [Projetil],
    -- | Estado de visibilidade do inimigo (fica invisível se estiver sobre o cimento)
    visivelInimigo :: Bool
  }
  deriving (Show,Eq,Read)

-- | Onda de inimigos que saem de um portal.
data Onda = Onda
  { -- | Inimigos que compõem a onda.
    inimigosOnda :: [Inimigo],
    -- | Tempo em segundos entre a entrada de cada inimigo.
    cicloOnda :: Tempo,
    -- | Tempo restante, em segundos, para a entrada do próximo inimigo da onda.
    tempoOnda :: Tempo,
    -- | Tempo restante, em segundos, para a entrada da onda.
    entradaOnda :: Tempo
  }
  deriving (Show,Eq,Read)

-- | Portal de entrada de inimigos no mapa.
data Portal = Portal
  { -- | Posição do portal no mapa. O portal deve estar sobre um terreno de terra.
    posicaoPortal :: Posicao,
    -- | Ondas de inimigos que saem do portal.
    ondasPortal :: [Onda]
  }
  deriving (Show,Eq,Read)

-- | Estado do jogo. Um jogo é composto pela base, vários portais, várias torres, um mapa, vários inimigos e a loja.
data Jogo = Jogo
  { -- | Base de operações do jogador.
    baseJogo :: Base,
    -- | Portais de entrada de inimigos no mapa.
    portaisJogo :: [Portal],
    -- | Torres construídas pelo jogador.
    torresJogo :: [Torre],
    -- | Mapa retangular do jogo.
    mapaJogo :: Mapa,
    -- | Inimigos em movimento no mapa.
    inimigosJogo :: [Inimigo],
    -- | Loja de torres disponíveis para construir.
    lojaJogo :: Loja,
    -- |Projeteis que estão a caminho dos inimigos
    tirosJogo :: [Tiro],
    -- | Torre selecionada pelo jogador para a colocar em jogo 
    torreSelecionada :: Maybe Torre,
    -- |Posicao do rato na tela 
    posicaoRato :: Posicao, 
    -- |Velocidade do jogo 
    velocidadeJogo :: Float ,
    -- |Dimesão do jogo
    terceiraDimensao :: Bool,
    -- |Torre selecionada para ser melhorada
    torreAMelhorar :: Maybe Torre,
    -- |Pregos atualmente no mapa 
    pregosJogo :: [Prego],
    -- |Prego selecionado para ser colocado no mapa
    pregoSelecionado :: Maybe Prego,
    -- |Boost que pode ser ativado pelo inimigo que coloca as torres a disparar mais rápido 
    boostJogo :: Boost
  }
  deriving (Show,Eq,Read)

-- |Tipo de dados que representa o Boost (este tipo de dados representa o "impulso" que pode ser utilizado pelo jogar em que as suas torres durante um periodo de tempo ficam a disparar mais rápido)
data Boost = Boost
  {
  -- |Velocidade que está atualmente a ser usada para a velocidade das torres (varia entre a velocidadeCBoost e velocidadeSBoost)
  velocidadeBoost :: Float,
  -- |Velocidade que as torres passam a ter quando ele está ativo
  velocidadeCBoost :: Float,
  -- |Velocidade normal quando o boost não está ativo 
  velocidadeSBoost :: Float,
  -- |Duração do Boost 
  duracaoBoost :: Float, 
  -- |Tempo restante para o boost acabar
  tempoBoost :: Float ,
  -- |Número de boosts restantes 
  numeroBoost :: Int, 
  -- |Valor que diz se o boost está ativo ou não
  ativoBoost :: Bool
  } deriving (Show,Eq,Read)


-- | Valor inicial que determina a sequência de números pseudo-aleatórios.
type Semente = Int

{-| Função que gera uma lista de números aleatórios a partir de uma 'Semente'.

== Exemplos

>>> geraAleatorios 2425 3
[9108974057934916489,3509742222561512871,1534041518507426227]

>>> geraAleatorios 10 1
[3575835729477015470]
-}
geraAleatorios :: Semente -> Int -> [Int]
geraAleatorios s c = take c $ randoms (mkStdGen s)


