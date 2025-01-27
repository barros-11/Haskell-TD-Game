{-|
Module      : Desenhar
Description : Este módulo serve para desenhar o jogo e as suas várias componentes.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Este módulo serve para desenhar o jogo e as suas várias componentes.
-}

module Desenhar where

import Graphics.Gloss
import ImmutableTowers
      

import LI12425
import Tarefa3
import Tarefa2
import Data.List
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Nivel5 (jogoN5)

-- | Função principal que desenha a tela de acordo com o estado atual do jogo.
desenha :: ImmutableTowers -> IO Picture
desenha im 
  |estadoJogo im == Jogando =  return (desenhaJogo im) 
  |estadoJogo im == Ganhou  = return $ Scale escala escala  $ Pictures [vitoria,if daN (nivelAtual (progressoJogo im)) (niveis im)/= jogoN5 then  Translate 30 (-350) nextLevel else Blank ]
  |estadoJogo im == TerminouJogo = return $ Scale escala escala $ desenhaTelaTerminouJogo (converteEmMinutos (tempoRecorde im)) (converteEmMinutos (tempoJogo im)) fundoSobrevivencia playAgain menu um dois tres quatro cinco seis sete oito nove zero doisPontos highScore newRecord currentTime
  |estadoJogo im == Perdeu =  return $ Scale escala escala derrota
  |estadoJogo im == MenuInicial =return $ Scale escala escala $ desenhaMenuInicial menuFundo level exit survivalMode menorResolucao maiorResolucao
  |estadoJogo im == Niveis = return $ Scale escala escala $ desenhaTelaNiveis (daN (nivelAtual (progressoJogo im )) mapas) fundoNiveis esquerda direita  playNow (daN (nivelAtual (progressoJogo im)) [level1,level2,level3,level4,level5])  (daN (nivelAtual (progressoJogo im )) (niveis im))(niveisDesbloqueados (progressoJogo im)) cadeado menu  
  |estadoJogo im == MenuInGame = return $ Pictures [desenhaJogo im, Scale escala escala $ (Pictures [telaEscura,  desenhaMenuInGame fundoMenuInGame quit continue menu] )]
  |estadoJogo im == ExplicaJogo = return $ Pictures [ desenhaJogo im, Scale escala escala $ (pictures [ Scale 1.5 1.5 telaEscura,  desenhaExplicaJogo comoJogar fundoCJ cj sair ])]
  |otherwise = return Blank
  where 
        mapas = [mapa1,mapa2,mapa3,mapa4,mapa5]
        escala = escalaAtual im
        vitoria = getImagem (imagens im) "vitoria"
        derrota = getImagem (imagens im) "derrota"
        esquerda = getImagem (imagens im) "esquerda"
        direita = getImagem (imagens im) "direita"
        playNow = getImagem (imagens im) "playNow"
        exit = getImagem (imagens im) "exit"
        menu = getImagem (imagens im) "menu"
        level = getImagem (imagens im) "level"
        menuFundo = getImagem (imagens im) "menuFundo"
        fundoNiveis = getImagem (imagens im) "fundoNiveis"
        level1 = getImagem (imagens im) "level1"
        level2 = getImagem (imagens im) "level2"
        level3 = getImagem (imagens im) "level3"
        level4 = getImagem (imagens im) "level4"
        level5 = getImagem (imagens im) "level5"
        quit = getImagem (imagens im) "quit"
        fundoMenuInGame = getImagem (imagens im) "fundoMenuInGame"
        mapa1 = getImagem (imagens im) "mapa1"
        telaEscura = Color (makeColorI 50 50 50 200) $ rectangleSolid  2560 1440
        mapa2 = getImagem (imagens im) "mapa2"
        mapa3 = getImagem (imagens im) "mapa3"
        mapa4 = getImagem (imagens im) "mapa4"
        mapa5 = getImagem (imagens im) "mapa5"
        cadeado = getImagem (imagens im) "cadeado"
        nextLevel = getImagem (imagens im) "nextLevel"
        fundoSobrevivencia = getImagem (imagens im) "fundoSobrevivencia"
        survivalMode = getImagem (imagens im) "survivalMode"
        playAgain = getImagem (imagens im) "playAgain"
        doisPontos = getImagem (imagens im) "doisPontos"
        um = getImagem (imagens im) "1"
        zero = getImagem (imagens im) "0"
        nove = getImagem (imagens im) "9"
        oito = getImagem (imagens im) "8"
        sete = getImagem (imagens im) "7"
        seis = getImagem (imagens im) "6"
        cinco= getImagem (imagens im) "5"
        quatro = getImagem (imagens im) "4"
        tres = getImagem (imagens im) "3"
        dois = getImagem (imagens im) "2"
        highScore = getImagem (imagens im) "highScore"
        newRecord = getImagem (imagens im) "newRecord"
        currentTime = getImagem (imagens im) "currentTime"
        comoJogar = getImagem (imagens im) "comoJogar"
        cj = getImagem (imagens im) "cj"
        fundoCJ = getImagem (imagens im) "fundoCJ"
        sair = getImagem (imagens im) "sair"
        menorResolucao = getImagem (imagens im) "menorResolucao"
        maiorResolucao = getImagem (imagens im) "maiorResolucao"
        continue = getImagem (imagens im) "continue"

        


                  


-- | Função que dado um lista de Imagens e uma chave devolve a respetiva picture 
getImagem :: Imagens -> String -> Picture 
getImagem [] _ = Blank
getImagem ((chave,imagem):xs) chaveImagem 
  | chave == chaveImagem = imagem
  | otherwise = getImagem xs chaveImagem

-- | Função responsável por desenhar o jogo
desenhaJogo :: ImmutableTowers -> Picture
desenhaJogo im 
  |not tresD =  Scale escala escala $ Pictures [ desenhaMapa survive mapas (nivelAtual (progressoJogo im)) (modoJogo im) , Translate (-(fromIntegral x*tileSize)/2) ((fromIntegral y *tileSize)/2 ) $ Pictures [
             
             desenhaInimigos inimigos inimigoFN inimigoFS inimigoFO inimigoFE inimigoMN inimigoMS inimigoMO inimigoME inimigoDN inimigoDS inimigoDO inimigoDE inimigoFNC inimigoFSC inimigoFOC inimigoFEC inimigoMNC inimigoMSC inimigoMOC inimigoMEC inimigoDNC inimigoDSC inimigoDOC inimigoDEC gelo fire slime,
             desenhaBase base baseD,
             desenhaPortais portais portal,
             desenhaTorres torres torreFogo torreGelo torreResina,
             desenhaPregosEmJogo pregosI pregos,
             desenhaTiros mapa tresD tiros torreFogoProjetil torreGeloProjetil torreResinaProjetil
           ] , 
           desenhaTempos (modoJogo im) timeToBeat (converteEmMinutos (tempoRecorde im)) (converteEmMinutos  (tempoJogo im)) um dois tres quatro cinco seis sete oito nove zero doisPontos,
           drawLoja loja base madeiraLoja torreFogo torreGelo torreResina torreDefault (desenhaLoja im) um dois tres quatro cinco seis sete oito nove doll zero ponto pregosI vezes, 
           desenhaBoost boostI boostIBW boost um dois tres quatro cinco seis sete oito nove vezes zero ponto,
           desenhaTorreSel torreSel torreFogo torreGelo torreResina, 
           desenhaPregoSel pregoSel pregosI,
           desenhaPergunta pergunta,
           desenhaTelaEvolucoes torreSel torreAEvoluir eDano eAlcance eRajada eCiclo botao madeiraEv sair atualmente vender nMaximo,
           ganhoVendaEAtributos torreAEvoluir um dois tres quatro cinco seis sete oito nove doll zero ponto,
           desenhaCreditosVida 0.5 32 (fromIntegral (creditosBase base)) 1150 650 um dois tres quatro cinco seis sete oito nove doll zero ponto ,
           desenhaCreditosVida 0.5 32 (vidaBase base) 1150 600 um dois tres quatro cinco seis sete oito nove coracao zero ponto,
           Translate 1080 625 $ Scale 0.5 0.5 pausa, Translate 1010 625 $ Scale 0.5 0.5 acelera, Translate 940 625 $ Scale 0.5 0.5 trava
           ]
  |otherwise = Scale escala escala $ Pictures [ Scale 1 contracaoVertical  $ Rotate anguloRotacao (desenhaMapa survive mapas (nivelAtual (progressoJogo im)) (modoJogo im)),            
          desenhaElementos3D (sortByPosicao (criaElementos (devolveJogo im))) mapa torreFogo3D3 torreFogo3D2 torreFogo3D1 torreGelo3D3 torreGelo3D2 torreGelo3D1 torreResina3D3 torreResina3D2 torreResina3D1 carroN carroS carroO carroE carroNC carroSC carroOC carroEC gelo fire slime base3D portal3D pregosI,
          desenhaTiros mapa tresD tiros torreFogoProjetil torreGeloProjetil torreResinaProjetil,
          desenhaDireitaEsquerda mapa,
          desenhaTempos (modoJogo im) timeToBeat (converteEmMinutos (tempoRecorde im)) (converteEmMinutos  (tempoJogo im)) um dois tres quatro cinco seis sete oito nove zero doisPontos,
          drawLoja loja base madeiraLoja torreFogo3D3 torreGelo3D3 torreResina3D3 torreDefault3D (desenhaLoja im) um dois tres quatro cinco seis sete oito nove doll zero ponto pregosI vezes, 
          desenhaBoost boostI boostIBW boost um dois tres quatro cinco seis sete oito nove vezes zero ponto ,
          desenhaTorreSel torreSel torreFogo3D1 torreGelo3D1 torreResina3D1, 
          desenhaPregoSel pregoSel pregosI,
          desenhaPergunta pergunta,
          desenhaTelaEvolucoes torreSel torreAEvoluir eDano eAlcance eRajada eCiclo botao madeiraEv sair atualmente vender nMaximo,
          ganhoVendaEAtributos torreAEvoluir um dois tres quatro cinco seis sete oito nove doll zero ponto,
          desenhaCreditosVida 0.5 32 (fromIntegral (creditosBase base)) 1150 650 um dois tres quatro cinco seis sete oito nove doll zero ponto ,
          desenhaCreditosVida 0.5 32 (vidaBase base) 1150 600 um dois tres quatro cinco seis sete oito nove coracao zero ponto,
          Translate 1080 625 $ Scale 0.5 0.5 pausa, Translate 1010 625 $ Scale 0.5 0.5 acelera, Translate 940 625 $ Scale 0.5 0.5 trava
          ]
    where
        mapas = [mapa1,mapa2,mapa3,mapa4,mapa5]
        escala = escalaAtual im 
        (x,y) = (length (head mapa), length mapa) 
        torreDefault = getImagem (imagens im) "torreDefault"
        torreFogo = getImagem (imagens im) "torreFogo"
        torreFogoProjetil = getImagem (imagens im) "torreFogoProjetil"
        torreGelo = getImagem (imagens im) "torreGelo"
        torreGeloProjetil = getImagem (imagens im) "torreGeloProjetil"
        torreResina = getImagem (imagens im) "torreResina"
        torreResinaProjetil = getImagem (imagens im) "torreResinaProjetil"
        baseD = getImagem (imagens im) "base"
        inimigoFN = getImagem (imagens im) "inimigoFN"
        inimigoFS = getImagem (imagens im) "inimigoFS"
        inimigoFO = getImagem (imagens im) "inimigoFO"
        inimigoFE = getImagem (imagens im) "inimigoFE"
        inimigoMN = getImagem (imagens im) "inimigoMN"
        inimigoMS = getImagem (imagens im) "inimigoMS"
        inimigoMO = getImagem (imagens im) "inimigoMO"
        inimigoME = getImagem (imagens im) "inimigoME" 
        inimigoDN = getImagem (imagens im) "inimigoDN"
        inimigoDS = getImagem (imagens im) "inimigoDS"
        inimigoDO = getImagem (imagens im) "inimigoDO"
        inimigoDE = getImagem (imagens im) "inimigoDE"
        inimigoFNC = getImagem (imagens im) "inimigoFNC"
        inimigoFSC = getImagem (imagens im) "inimigoFSC"
        inimigoFOC = getImagem (imagens im) "inimigoFOC"
        inimigoFEC = getImagem (imagens im) "inimigoFEC"
        inimigoMNC = getImagem (imagens im) "inimigoMNC"
        inimigoMSC = getImagem (imagens im) "inimigoMSC"
        inimigoMOC = getImagem (imagens im) "inimigoMOC"
        inimigoMEC = getImagem (imagens im) "inimigoMEC" 
        inimigoDNC = getImagem (imagens im) "inimigoDNC"
        inimigoDSC = getImagem (imagens im) "inimigoDSC"
        inimigoDOC = getImagem (imagens im) "inimigoDOC"
        inimigoDEC = getImagem (imagens im) "inimigoDEC"
        madeiraLoja = getImagem (imagens im) "madeiraLoja"
        portal = getImagem (imagens im) "portal"
        um = getImagem (imagens im) "1"
        zero = getImagem (imagens im) "0"
        nove = getImagem (imagens im) "9"
        oito = getImagem (imagens im) "8"
        sete = getImagem (imagens im) "7"
        seis = getImagem (imagens im) "6"
        cinco= getImagem (imagens im) "5"
        quatro = getImagem (imagens im) "4"
        tres = getImagem (imagens im) "3"
        dois = getImagem (imagens im) "2"
        doll = getImagem (imagens im) "$"
        coracao = getImagem (imagens im) "coracao"
        mapa1 = getImagem (imagens im) "mapa1"
        trava = getImagem (imagens im) "trava"
        acelera = getImagem (imagens im) "acelera"
        carroN = getImagem (imagens im) "carroN"
        carroS = getImagem (imagens im) "carroS"
        carroO = getImagem (imagens im) "carroO"
        carroE = getImagem (imagens im) "carroE"
        carroNC = getImagem (imagens im) "carroNC"
        carroSC = getImagem (imagens im) "carroSC"
        carroOC = getImagem (imagens im) "carroOC"
        carroEC = getImagem (imagens im) "carroEC"
        (Jogo base portais torres mapa inimigos loja tiros torreSel _ _ tresD torreAEvoluir pregos pregoSel boost) = devolveJogo im 
        pausa = getImagem (imagens im) "pausa"
        torreFogo3D3 = getImagem (imagens im) "torreFogo3D3"
        torreFogo3D2 = getImagem (imagens im) "torreFogo3D2"
        torreFogo3D1 = getImagem (imagens im) "torreFogo3D1"
        torreGelo3D3 = getImagem (imagens im) "torreGelo3D3"
        torreGelo3D2 = getImagem (imagens im) "torreGelo3D2"
        torreGelo3D1 = getImagem (imagens im) "torreGelo3D1"
        torreResina3D3 = getImagem (imagens im) "torreResina3D3"
        torreResina3D2 = getImagem (imagens im) "torreResina3D2"
        torreResina3D1 = getImagem (imagens im) "torreResina3D1"
        portal3D = getImagem (imagens im) "portal3D"
        base3D = getImagem (imagens im) "base3D"
        torreDefault3D = getImagem (imagens im) "torreDefault3D"
        mapa2 = getImagem (imagens im) "mapa2"
        mapa3 = getImagem (imagens im) "mapa3"
        mapa4 = getImagem (imagens im) "mapa4"
        mapa5 = getImagem (imagens im) "mapa5"
        survive = getImagem (imagens im) "survive"
        gelo = getImagem (imagens im) "gelo"
        fire = getImagem (imagens im) "fire"
        slime = getImagem (imagens im) "slime"
        botao = getImagem (imagens im) "botao"
        eDano = getImagem (imagens im) "eDano"
        eAlcance = getImagem (imagens im) "eAlcance"
        eRajada = getImagem (imagens im) "eRajada"
        eCiclo = getImagem (imagens im) "eCiclo"
        madeiraEv = getImagem (imagens im) "madeiraEv"
        sair = getImagem (imagens im) "sair"
        atualmente = getImagem (imagens im) "atualmente"
        vender = getImagem (imagens im) "vender" 
        ponto = getImagem (imagens im) "ponto"
        pregosI = getImagem (imagens im) "pregos"
        vezes = getImagem (imagens im) "vezes"
        boostI = getImagem (imagens im) "boost"
        boostIBW = getImagem (imagens im) "boostBW"
        timeToBeat = getImagem (imagens im) "timeToBeat"
        doisPontos = getImagem (imagens im) "doisPontos"
        pergunta = getImagem (imagens im) "pergunta"
        nMaximo= getImagem (imagens im) "nMaximo"
  
        


-- |Função reponsável por desenhar o menu onde se explica o jogo  
desenhaExplicaJogo :: Picture -> Picture -> Picture -> Picture -> Picture 
desenhaExplicaJogo comoJogar fundo cj sair= 
  Pictures [Translate 0 (-100) (Scale 3.5 2.5 fundo), Translate 0 (-50) comoJogar, Translate 0 500 cj,
            Translate 550 (580) sair]

-- |Função responsável por desenhar a símbolo de pergunta durante o jogo
desenhaPergunta :: Picture -> Picture 
desenhaPergunta p = Translate 1200 (-650) pergunta
  where pergunta = Scale 2.4 2.4 p

-- |Tipo de dados que represantam os elementos do jogo cujo a sua perpetiva no mapa é influêncida pela sobreposição 
type Elemento = (TipoElemento, Posicao)

-- |Tipo de dados que contêm os elementos cuja perpetiva no mapa é influênciada pela sua sobreposição
data TipoElemento  = Torre' Torre|
            Inimigo' Inimigo | 
            Portal' Portal | 
            Base' Base  |
            Prego' Prego deriving (Eq,Show)

-- |Função que cria uma lista de elementos a partir de um jogo
criaElementos :: Jogo -> [Elemento]
criaElementos (Jogo {baseJogo = base, portaisJogo = portais, torresJogo = torres, inimigosJogo = inimigos, pregosJogo =pregos}) = 
  (Base' base,posicaoBase base): 
  map (\portal -> (Portal' portal, posicaoPortal portal)) portais ++
  map (\torre -> (Torre' torre, posicaoTorre torre)) torres ++
  map (\inimigo -> (Inimigo' inimigo, posicaoInimigo inimigo)) inimigos ++ 
  map (\p -> (Prego' p, posicaoPrego p)) pregos

-- |Função resposável por ordenar uma lista de elementos pela sua posição
sortByPosicao :: [Elemento] -> [Elemento] 
sortByPosicao elementos = sortBy (comparing snd) elementos 

-- |Função responsável por desenhar os elementos do jogo em 3D
desenhaElementos3D :: [Elemento] -> Mapa -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture 
desenhaElementos3D elementos mapa torreFogo3D3 torreFogo3D2 torreFogo3D1 torreGelo3D3 torreGelo3D2 torreGelo3D1 torreResina3D3 torreResina3D2 torreResina3D1 carroN carroS carroO carroE carroNC carroSC carroOC carroEC gelo fire slime base3D portal3D pregoI 
  = Pictures $ map desenhaElemento elementos
  where desenhaElemento :: Elemento -> Picture  
        desenhaElemento (Torre' torre, _) = desenhaTorre3D mapa torre torreFogo3D3 torreFogo3D2 torreFogo3D1 torreGelo3D3 torreGelo3D2 torreGelo3D1 torreResina3D3 torreResina3D2 torreResina3D1
        desenhaElemento (Inimigo' inimigo, _) = desenhaInimigo3D mapa inimigo carroN carroS carroO carroE carroNC carroSC carroOC carroEC gelo fire slime
        desenhaElemento (Base' base, _) = desenhaBase3D mapa base base3D
        desenhaElemento (Portal' portal, _) = desenhaPortal3D mapa portal portal3D 
        desenhaElemento (Prego' prego, _) = desenhaPregoEmJogo3D mapa pregoI prego

-- |Função responsável por desenhar o tempo de jogo e o tempo recorde no modo de sobrevivência
desenhaTempos :: Modo -> Picture ->  Float -> Float -> Picture -> Picture -> Picture -> Picture-> Picture -> Picture-> Picture -> Picture-> Picture -> Picture-> Picture -> Picture
desenhaTempos modo timeToBeat recorde tempo  um dois tres quatro cinco seis sete oito nove zero doisPontos = 
  case modo of 
    Sobrevivencia -> Pictures [
                Translate (-1070) 650 timeToBeat, 
                Pictures (map (\((x,y),num)-> desenhaCreditosVida 0.4 32 num x y um dois tres quatro cinco seis sete oito nove Blank zero doisPontos) lista)
                ]
              

    Levels -> Blank 
  where 
    lista = [((-915,634), tempo), ((-905,670), recorde)]

-- |Função responsável por desenhar a tela de fim do jogo de sobrevivência
desenhaTelaTerminouJogo :: Float -> Float -> Picture -> Picture -> Picture -> Picture-> Picture -> Picture -> Picture -> Picture-> Picture -> Picture-> Picture -> Picture-> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
desenhaTelaTerminouJogo  recorde tempo fundoSobrevivencia playAgain menu um dois tres quatro cinco seis sete oito nove zero doisPontos highScore newRecord currentTime = 
  Pictures [fundoSobrevivencia, 
            Translate 0 (-300) playAgain,
            Translate 0 (-500) menu,
            Pictures (map (\((x,y),num)-> desenhaCreditosVida 1.5 100 num x y um dois tres quatro cinco seis sete oito nove Blank zero doisPontos) lista),
            Translate (-650) 300 currentTime, 
            Translate 700 300 highScore,
            Translate 0 500 nR]

  where 
    lista = [((-800,180),  tempo), ((500,180), recorde)]
    nR = if paraSegundo recorde < paraSegundo  tempo then newRecord else Blank
    paraSegundo :: Float -> Int 
    paraSegundo  t =  (floor t) *60 + (floor (t*100) `mod` 100)


-- |Função responsável por passar um número para um formato de tempo (minutos e segundos), em que eles ficam divididos por uma vírgula 
converteEmMinutos::  Float ->Float 
converteEmMinutos t = (minutos + segundos/100)
  where 
      segundos = fromIntegral ((floor  t) `mod` 60)
      minutos = fromIntegral  ((floor t) `div` 60)

-- |Função responsavel por desenhar o mapa correto de acordo com o nível e o modo de jogo
desenhaMapa :: Picture -> [Picture] -> Int -> Modo -> Picture
desenhaMapa survival mapas nivel modo 
  |modo == Levels = daN nivel mapas
  |otherwise = survival  

-- | Função responsável por desenhar  o botão de boost
desenhaBoost :: Picture -> Picture -> Boost -> Picture -> Picture -> Picture-> Picture -> Picture-> Picture -> Picture-> Picture -> Picture-> Picture -> Picture-> Picture -> Picture
desenhaBoost boostI boostIBW boost um dois tres quatro cinco seis sete oito nove vezes zero ponto 
  |ativoBoost boost = Pictures [ Translate 800 630 boostIBW, desenhaCreditosVida 0.5 32 (fromIntegral (numeroBoost boost)) 780 565 um dois tres quatro cinco seis sete oito nove vezes zero ponto]
  |otherwise = Pictures [Translate 800 630 boostI, desenhaCreditosVida 0.5 32 (fromIntegral (numeroBoost boost)) 780 565 um dois tres quatro cinco seis sete oito nove vezes zero ponto]


-- | Função responsável por desenhar o Prego quando selecionado
desenhaPregoSel :: Maybe Prego ->Picture  -> Picture
desenhaPregoSel prego pregoI
  |prego == Nothing = Blank
  |otherwise = Translate x y pregoI
    where (x,y) = posicaoPrego (fromJust prego)

-- | Função responsável por desenhar os pregos em jogo
desenhaPregosEmJogo :: Picture -> [Prego] -> Picture 
desenhaPregosEmJogo pregoI pregos = Pictures $ map desenhaPrego pregos
  where desenhaPrego :: Prego -> Picture 
        desenhaPrego prego = Translate (x*tileSize) (-y*tileSize) pregoI
          where (x,y) = posicaoPrego prego
    
-- | Função responsável por desenhar um prego em 3D
desenhaPregoEmJogo3D :: Mapa -> Picture -> Prego-> Picture 
desenhaPregoEmJogo3D mapa pregoI prego= Translate x y pregoI
          where (x,y) = mudaPara3D mapa (posicaoPrego prego)

-- | Função responsável por desenhar o ganho da venda de uma torre assim como as suas características atuais na tela de evoluções
ganhoVendaEAtributos :: Maybe Torre -> Picture -> Picture ->Picture -> Picture -> Picture -> Picture -> Picture ->Picture -> Picture -> Picture -> Picture ->Picture -> Picture
ganhoVendaEAtributos torre um dois tres quatro cinco seis sete oito nove dollar zero ponto
  |torre == Nothing = Blank
  |otherwise =
  Pictures $ desenhaCreditosVida 0.5 32 (fromIntegral (daValorDeVenda (fromJust torre)))  (-1034+4*(372+50)-50) (-630) um dois tres quatro cinco seis sete oito nove dollar zero ponto:
      map (\((x,y),num) -> desenhaCreditosVida 0.4 32 num x y um dois tres quatro cinco seis sete oito nove Blank zero ponto) lista
  where 
    lista = [ ((-1034+5*(372+50)+55,-560-5),fromIntegral (rajadaTorre (fromJust torre))),
              ((-1034+5*(372+50)+30,-555+15),danoTorre (fromJust torre)), ((-1034+5*(372+50)+75,-550+40), alcanceTorre (fromJust torre)), 
              ((-1034+5*(372+50)+35,-565-30), cicloTorre (fromJust torre)), ((-1034+5*(372+50)+50,-570-57),fromIntegral (nivelTorre (fromJust torre)))]



-- |Função responsável por desenhar as constantes da tela das evoluções 
desenhaTelaEvolucoes :: Maybe Torre -> Maybe Torre -> Picture -> Picture -> Picture -> Picture  -> Picture -> Picture -> Picture ->Picture -> Picture -> Picture -> Picture 
desenhaTelaEvolucoes torreSel torreAEvoluir eDano eAlcance eRajada eCiclo botao madeira sair atualmente vender nMaximo 
  |torreSel == Nothing && torreAEvoluir /= Nothing = 
    Pictures $ ( Translate 0 (-540) madeira : Translate 1200 (-373) sair : map (\((b,e), (x,y)) -> Pictures [Translate x y b , Translate x y e]) listaBotoes ++ [Translate (-420) (-535) $ Scale 1.78 1.6 maximo])
  |otherwise = Blank 
  where 
    botao' = Scale 1.4 1.4 botao 
    listaBotoes = zip [(botao',eDano),(botao',eAlcance),(botao',eRajada),(botao',eCiclo), (botao',vender), (Blank,atualmente)] [(-1046+ x*(368.3+50),-540)| x<- [0 .. 5]]
    maximo = case nivelTorre (fromJust torreAEvoluir) of 
              3 -> nMaximo 
              _ -> Blank

-- | Função responsável por desenhar o menu inicial 
desenhaMenuInicial :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture  
desenhaMenuInicial  menuFundo level exit survivalMode menorResolucao maiorResolucao= 
  Pictures [ menuFundo,Translate 0 (-150) level, Translate 0 (-530) exit, Translate 0 (-340) survivalMode, 
            Translate (-200) 50 menorResolucao, Translate (200) 50 maiorResolucao]


-- | Função responsável por desenhar a tela dos niveis
desenhaTelaNiveis :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Jogo ->[Jogo] -> Picture -> Picture -> Picture 
desenhaTelaNiveis  mapaN  fundoNiveis esquerda direita playNow levelN nAtual nDesbolqueados cadeado menu
  |nAtual `elem` nDesbolqueados = 
                                Pictures [fundoNiveis, Translate (-1120) 0 esquerda, 
                                Translate (1120) 0 direita,
                                Translate 0 (-500) playNow,
                                mapa, level, Translate 1120 (-620) $ Scale 0.8 0.8 menu ]
  |otherwise = Pictures [fundoNiveis, Translate (-1120) 0 esquerda, 
              Translate (1120) 0 direita,
              Translate 0 (-500) playNow,
              mapa, level, cadeado, Translate (1120) (-620) $ Scale 0.8 0.8 menu ]
  where 
    mapa = Scale 0.5 0.5 mapaN
    level = Translate 0 500 levelN
  
-- | Função responsável por desenhar o menu dentro do jogo
desenhaMenuInGame :: Picture -> Picture -> Picture -> Picture -> Picture 
desenhaMenuInGame fundoMenuInGame quit start menu = 
  Pictures [Scale 1.5 1.5 fundoMenuInGame, Translate 0 300 start, Translate 0 0 quit, Translate 0 (-300) menu]

-- | Função responsável por desenhar a torre selecionada
desenhaTorreSel :: Maybe Torre -> Picture -> Picture -> Picture -> Picture 
desenhaTorreSel torre torreFogo torreGelo torreResina = 
    case torre of 
        Nothing -> Blank 
        (Just t) -> Translate x y pic
              where 
                (x,y) = posicaoTorre t
                pic = case (tipoProjetil .projetilTorre) t of 
                              Gelo -> torreGelo
                              Fogo -> torreFogo 
                              Resina -> torreResina

-- | Função responsável por desenhar os tiros
desenhaTiros :: Mapa -> Bool -> [Tiro] -> Picture -> Picture -> Picture -> Picture
desenhaTiros mapa e3D tiros tiroFogo tiroGelo tiroResina = Pictures (map desenhaTiro  tiros)
  where 
    desenhaTiro ::  Tiro -> Picture
    desenhaTiro ((x,y),_,_, tipo) = case e3D of 
                                      False -> Translate (tileSize*x) (-tileSize*y)  pic
                                      True -> Translate xf yf  pic
      where   
        (xf,yf) = mudaPara3D mapa (x,y)
        pic = case tipo of 
          Fogo -> tiroFogo
          Gelo -> tiroGelo
          Resina -> tiroResina




-- | Função responsável por desenhar a loja
drawLoja :: Loja -> Base -> Picture -> Picture -> Picture -> Picture -> Picture -> DesenhaLoja -> Picture-> Picture-> Picture-> Picture-> Picture-> Picture-> Picture-> Picture-> Picture-> Picture-> Picture -> Picture ->Picture -> Picture -> Picture
drawLoja  loja base madeiraLoja torreFogo torreGelo torreResina torreDefault desenhaLoja um dois tres quatro cinco seis sete oito nove doll zero ponto pregos vezes
  | not desenhaLoja = Blank 
  |otherwise = Pictures [
    Translate (-1080) 0 $ Scale 1 1  madeiraLoja,
    if (creditosBase base) >= precoFogo then  Translate posx posyFogo (Scale 1.3 1.3 torreFogo) else Translate  posx posyFogo torreDefault,
    if (creditosBase base) >= precoResina then Translate posx posyResina (Scale 1.3 1.3 torreResina) else Translate posx posyResina torreDefault,
    if (creditosBase base) >= precoGelo  then Translate posx posyGelo (Scale 1.3 1.3 torreGelo) else Translate (posx) posyGelo torreDefault,
    Translate (-20 +posx) posyPrego  pregos,
    Pictures desenhaPrecos
  ]
    where precoFogo = daPreco loja Fogo
          precoGelo = daPreco loja Gelo  
          precoResina = daPreco loja Resina 
          espacoY = 130
          espacoX = -40
          desenhaPrecos = map (\(preco,x,y,acessorio) ->(desenhaCreditosVida 0.5 32 preco (x+ espacoX) (y+ espacoY) um dois tres quatro cinco seis sete oito nove acessorio zero ponto)) [(fromIntegral precoFogo,posx,posyFogo,doll),( fromIntegral precoGelo, posx, posyGelo,doll), (fromIntegral precoResina, posx, posyResina,doll), (fromIntegral (pregosBase base), posx, posyPrego,vezes)]


          
          
-- |Função responsável por dar o preço de uma torre da loja 
daPreco :: Loja -> TipoProjetil -> Creditos 
daPreco []  _ = 0 
daPreco ((preco ,torre):xs) tipo 
  |(tipoProjetil.projetilTorre) torre == tipo = preco 
  |otherwise = daPreco xs tipo 

     


-- |Função responsável por desenhar um inimigo no mapa 
desenhaInimigos:: [Inimigo] -> Picture ->Picture -> Picture ->Picture -> Picture ->Picture -> Picture ->Picture -> Picture ->Picture -> Picture ->Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->Picture -> Picture ->Picture -> Picture ->Picture -> Picture ->Picture -> Picture ->Picture -> Picture ->Picture
desenhaInimigos inims iniFN iniFS iniFO iniFE iniMN iniMS iniMO iniME iniDN iniDS iniDO iniDE iniFNC iniFSC iniFOC iniFEC iniMNC iniMSC iniMOC iniMEC iniDNC iniDSC iniDOC iniDEC gelo fire slime= Pictures (map desenhaInimigo inims)
  where 
      desenhaInimigo :: Inimigo -> Picture
      desenhaInimigo inimigo 
        |efeitos == gelo = Translate (tileSize*x) (-y*tileSize) $ Pictures [Scale 1.3 1.3 $ (case (direcaoInimigo inimigo) of 
                                                                            Norte ->  iniN
                                                                            Sul -> iniS
                                                                            Este ->  iniE
                                                                            Oeste -> iniO  ), efeitos ]
        |otherwise = Translate (tileSize*x) (-y*tileSize) $ Pictures [ efeitos, Scale 1.3 1.3 $ (case (direcaoInimigo inimigo) of 
                                                                            Norte -> iniN
                                                                            Sul ->  iniS
                                                                            Este ->  iniE
                                                                            Oeste ->  iniO  ) ]
        where 
              efeitos :: Picture 
              efeitos | temTipo (projeteisInimigo inimigo) Gelo = gelo
                      | temTipo (projeteisInimigo inimigo) Fogo = fire
                      | temTipo (projeteisInimigo inimigo) Resina = slime
                      | otherwise = Blank
              (x,y) = posicaoInimigo inimigo 
              iniN  | vidaInimigo inimigo <= 100 && visivelInimigo inimigo == False = iniFNC
                    | vidaInimigo inimigo <= 100 && visivelInimigo inimigo == True = iniFN
                    | vidaInimigo inimigo  <= 200 && visivelInimigo inimigo == False  = iniMNC
                    | vidaInimigo inimigo  <= 200  && visivelInimigo inimigo == True  = iniMN
                    | visivelInimigo inimigo == False = iniDNC
                    | otherwise = iniDN         
              iniS | vidaInimigo inimigo <= 100 && visivelInimigo inimigo == False = iniFSC
                    | vidaInimigo inimigo <= 100 && visivelInimigo inimigo == True = iniFS
                    | vidaInimigo inimigo  <= 200 && visivelInimigo inimigo == False  = iniMSC
                    | vidaInimigo inimigo  <= 200  && visivelInimigo inimigo == True  = iniMS
                    | visivelInimigo inimigo == False = iniDSC
                    | otherwise = iniDS 
              iniE | vidaInimigo inimigo <= 100 && visivelInimigo inimigo == False = iniFEC
                    | vidaInimigo inimigo <= 100 && visivelInimigo inimigo == True = iniFE
                    | vidaInimigo inimigo  <= 200 && visivelInimigo inimigo == False  = iniMEC
                    | vidaInimigo inimigo  <= 200  && visivelInimigo inimigo == True  = iniME
                    | visivelInimigo inimigo == False = iniDEC
                    | otherwise = iniDE   
              iniO | vidaInimigo inimigo <= 100 && visivelInimigo inimigo == False = iniFOC
                    | vidaInimigo inimigo <= 100 && visivelInimigo inimigo == True = iniFO
                    | vidaInimigo inimigo  <= 200 && visivelInimigo inimigo == False  = iniMOC
                    | vidaInimigo inimigo  <= 200  && visivelInimigo inimigo == True  = iniMO
                    | visivelInimigo inimigo == False = iniDOC
                    | otherwise = iniDO

                        
-- |Função que desenha um inimigo no mapa 3D
desenhaInimigo3D :: Mapa -> Inimigo -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture  ->Picture -> Picture -> Picture -> Picture  ->Picture -> Picture
desenhaInimigo3D mapa inimigo carroN carroS carroO carroE carroNC carroSC carroOC carroEC gelo fire slime = 
  inimigof
  where 
    inimigof 
      |efeitos == gelo = Translate xf yf $ Pictures [(case direcaoInimigo inimigo of 
                                                          Norte -> if visivelInimigo inimigo == True then Scale escalaC escalaC carroN else Scale escalaC escalaC carroNC
                                                          Sul -> if visivelInimigo inimigo == True then Scale escalaC escalaC carroS else Scale escalaC escalaC carroSC
                                                          Oeste -> if visivelInimigo inimigo == True then Scale escalaC escalaC carroO else Scale escalaC escalaC carroOC
                                                          Este -> if visivelInimigo inimigo == True then Scale escalaC escalaC carroE else Scale escalaC escalaC carroEC),
                                                          efeitos]
      |otherwise = Translate xf yf $ Pictures [efeitos, case direcaoInimigo inimigo of 
                                                          Norte -> if visivelInimigo inimigo == True then Scale escalaC escalaC carroN else Scale escalaC escalaC carroNC
                                                          Sul -> if visivelInimigo inimigo == True then Scale escalaC escalaC carroS else Scale escalaC escalaC carroSC
                                                          Oeste -> if visivelInimigo inimigo == True then Scale escalaC escalaC carroO else Scale escalaC escalaC carroOC
                                                          Este -> if visivelInimigo inimigo == True then Scale escalaC escalaC carroE else Scale escalaC escalaC carroEC]
       
    efeitos :: Picture 
    efeitos | temTipo (projeteisInimigo inimigo) Gelo = gelo
                | temTipo (projeteisInimigo inimigo) Fogo = fire
                | temTipo (projeteisInimigo inimigo) Resina = slime
                | otherwise = Blank
    escalaC = 0.5
    (x,y) = mudaPara3D mapa (posicaoInimigo inimigo)
    (xf,yf) = (x,y+ 15)

-- |Função auxiliar que passa uma posicao para a sua posicao no mapa 3D
mudaPara3D :: Mapa -> Posicao -> Posicao 
mudaPara3D mapa (x,y) = (xf,yf)
      where 
        (largura, altura) = (tileSize * fromIntegral (length (head mapa)), tileSize * fromIntegral (length mapa))
        (x', y') = (tileSize * x- largura/2 , -tileSize * y + altura/2)
        (x'', y'') = (cos a *x' +sin a * y', cos a *y'- sin a * x')
        (xf, yf) = (x'', contracaoVertical * y'')
        a = converteEmRadianos anguloRotacao 

-- |Função responsável por desenhar as torres no mapa 
desenhaTorres:: [Torre] -> Picture -> Picture -> Picture -> Picture 
desenhaTorres torres torreFogo torreGelo torreResina = Pictures (map desenhaTorre torres)
  where 
    desenhaTorre ::  Torre -> Picture
    desenhaTorre  torre = Translate (tileSize*x) (-tileSize*y) $ Scale (daN (nivelTorre torre -1) [1,1.2,1.4]) (daN (nivelTorre torre -1) [1,1.2,1.4]) pic
      where 
        (x,y) = posicaoTorre torre 
        pic = case (tipoProjetil . projetilTorre) torre of 
          Fogo -> torreFogo
          Gelo -> torreGelo
          Resina -> torreResina

-- |Função que desenha uma torre no mapa 3D
desenhaTorre3D :: Mapa -> Torre -> Picture -> Picture -> Picture -> Picture-> Picture -> Picture -> Picture-> Picture -> Picture -> Picture
desenhaTorre3D mapa torre torreFogo3 torreFogo2 torreFogo1 torreGelo3 torreGelo2 torreGelo1 torreResina3 torreResina2 torreResina1 =
  Translate xf yf $ case (tipoProjetil . projetilTorre) torre of 
                                                Fogo ->  case (nivelTorre torre) of 
                                                            1 -> torreFogo1
                                                            2 -> torreFogo2
                                                            3 -> torreFogo3
                                                            _ -> Blank
                                                Gelo ->  case (nivelTorre torre) of 
                                                            1 -> torreGelo1
                                                            2 -> torreGelo2
                                                            3 -> torreGelo3
                                                            _ -> Blank
                                                Resina -> case (nivelTorre torre) of 
                                                            1 -> torreResina1
                                                            2 -> torreResina2
                                                            3 -> torreResina3
                                                            _ -> Blank
  where 
    (xf,yf) = mudaPara3D mapa (posicaoTorre torre)


-- |Função que desenha a base no mapa
desenhaBase :: Base -> Picture -> Picture 
desenhaBase base pic = Translate (tileSize*x) (-tileSize*y) pic
  where (x,y) = posicaoBase base

-- |Função que desenha a base no mapa 3D
desenhaBase3D :: Mapa -> Base -> Picture -> Picture
desenhaBase3D mapa base pic = Translate xf yf $ Scale 1.4 1.4 pic
  where 
    (x,y) = mudaPara3D mapa (posicaoBase base)
    (xf,yf) = (x+10,y+30)

-- |Função que desenha os portais no mapa 
desenhaPortais :: [Portal] ->  Picture -> Picture 
desenhaPortais portais pic = Pictures (map (desenhaPortal pic) portais) 
  where 
    desenhaPortal :: Picture -> Portal -> Picture 
    desenhaPortal pict portal = Translate (x*tileSize) (-y*tileSize) pict  
      where (x,y) = posicaoPortal portal

-- |Função que desenha um portal no mapa 3D
desenhaPortal3D ::Mapa -> Portal -> Picture -> Picture
desenhaPortal3D mapa portal pict = Translate xf yf $ Scale 1.2 1.2 pict
      where 
        (x,y) = mudaPara3D mapa (posicaoPortal portal)
        (xf,yf) = (x,y+20)
        
-- |Função que permite desenhar um número no tela
desenhaCreditosVida ::  Float -> Float-> Float ->Float -> Float -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture 
desenhaCreditosVida escala espaco creditos xInicial yInicial um dois tres quatro cinco seis sete oito nove doll zero ponto
    |creditos == fromIntegral (floor creditos) = Pictures (dollarSign :digitPicturesInt )
    |otherwise = Pictures (dollarSign : digitPicturesFloat ++ [desenhaPonto])
  where
   
        -- Lista de imagens traduzidas para os dígitos
    digitPicturesInt = zipWith (\i d -> Translate (xInicial + i * espaco) yInicial (Scale escala escala (imagemDigito d))) [1..] (digitos (floor creditos) ) 
    digitPicturesFloat = zipWith (\i d -> Translate (xInicial + i * espaco) yInicial (Scale escala escala (imagemDigito d))) [1..] (digitos (floor creditos) ++ digitos (floor (creditos*100) - ((floor creditos)*100))) 
    desenhaPonto = Translate (xInicial + (fromIntegral (length (digitos (floor creditos)))+0.5)* espaco) yInicial (Scale escala escala ponto)
    -- adereço à esquerda
    dollarSign = Translate xInicial yInicial (Scale 0.5 0.5 doll)


    -- Imagem de cada dígito
    imagemDigito :: Int -> Picture
    imagemDigito n = case n of
        0 -> zero
        1 -> um
        2 -> dois
        3 -> tres
        4 -> quatro
        5 -> cinco
        6 -> seis
        7 -> sete
        8 -> oito
        9 -> nove
        _ -> Blank 

      -- Converte os créditos para uma lista de dígitos
    digitos :: Int -> [Int] 
    digitos n 
      |n<10 = [n]
      |otherwise = digitos (n `div` 10) ++ [n `mod` 10]



-- |Função que dá o efeito de sombra a uma cor 
dark' :: Color -> Color
dark' col = mixColors 0.6 0.4 black col

-- |Função que dá o efeito de luz a uma cor
light' :: Color -> Color
light' col = mixColors 0.7 0.1 col white

-- |Função que desenha a parte lateral direira e esquerda do mapa 
desenhaDireitaEsquerda ::Mapa -> Picture 
desenhaDireitaEsquerda mapa  = 
   Pictures [Color (dark' verde) $ Polygon [(xl,yl), (xl, yl-m), (xp, yp-m), (xp,yp)],
            Color (light' verde) $ Polygon [(xl,yl), (xl, yl-m), (xr, yr-m), (xr, yr)]]
   where 
      verde :: Color 
      verde = makeColor (46/256) (202/256) (112/256) 1
      altura = fromIntegral $ length mapa
      largura = fromIntegral $ length (head mapa)
      (xp,yp) = mudaPara3D mapa ( largura ,0)
      (xl,yl) = mudaPara3D mapa (largura,altura)
      (xr,yr) = mudaPara3D mapa (0,altura)
      m= 100 :: Float 
      


