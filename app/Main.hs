{-|
Module      : Main
Description : Este módulo é responsável por iniciar o jogo Immutable Towers, carregando as imagens necessárias e definindo a janela, o fundo e o frame rate do jogo.
              É também responsável por ler o estado do jogo guardado no ficheiro save.txt e iniciar o jogo com o estado guardado.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo que inicia o jogo Immutable Towers, carregando as imagens necessárias e definindo a janela, o fundo e o frame rate do jogo.
-}
module Main where

import Desenhar
import Eventos
import Graphics.Gloss 
import ImmutableTowers
import Tempo
import Nivel1
import Nivel2 
import Nivel3
import Nivel4
import Nivel5
import Tarefa3
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.Directory (doesFileExist)


-- | Janela do jogo
janela :: Display
janela = -- InWindow "Immutable Towers" (1920, 1080) (0, 0)
  FullScreen

-- | Fundo do jogo
fundo :: Color
fundo = black

-- | frame rate
fr :: Int
fr = 240

-- | Função responsável pelo Upload das imagens
upLoadImages :: ImmutableTowers -> IO ImmutableTowers 
upLoadImages im = 
  do 
    torreDefault' <- loadBMP "Imagens/Imagens2D/Torres/torreDefault.bmp" 
    let torreDefault = Scale 1.5 1.5 torreDefault'
    torreFogo <- loadBMP "Imagens/Imagens2D/Torres/torreFogo.bmp"
    torreFogoProjetil' <- loadBMP "Imagens/Imagens2D/Torres/torreFogoProjetil.bmp"
    let torreFogoProjetil  = Scale 0.13 0.13 torreFogoProjetil'
    torreGelo <- loadBMP "Imagens/Imagens2D/Torres/torreGelo.bmp"
    torreGeloProjetil' <- loadBMP "Imagens/Imagens2D/Torres/torreGeloProjetil.bmp"
    let torreGeloProjetil = Scale 0.005 0.005 torreGeloProjetil'
    torreResina <- loadBMP "Imagens/Imagens2D/Torres/torreResina.bmp"
    torreResinaProjetil' <- loadBMP "Imagens/Imagens2D/Torres/torreResinaProjetil.bmp"
    let torreResinaProjetil = Scale 0.06 0.06 torreResinaProjetil'
    baseP <- loadBMP "Imagens/Imagens2D/Bases/base.bmp"
    inimigoFEC <- loadBMP "Imagens/Imagens2D/Inimigos/inimigoFEC.bmp"
    let inimigoFNC = rotate 270 inimigoFEC
    let inimigoFOC = rotate 180 inimigoFEC 
    let inimigoFSC = rotate 90 inimigoFEC
    inimigoMEC <- loadBMP "Imagens/Imagens2D/Inimigos/inimigoMEC.bmp"
    let inimigoMNC = rotate 270 inimigoMEC
    let inimigoMOC = rotate 180 inimigoMEC 
    let inimigoMSC = rotate 90 inimigoMEC
    inimigoDEC <- loadBMP "Imagens/Imagens2D/Inimigos/inimigoDEC.bmp"
    let inimigoDNC = rotate 270 inimigoDEC
    let inimigoDOC = rotate 180 inimigoDEC 
    let inimigoDSC = rotate 90 inimigoDEC
    inimigoFE <- loadBMP "Imagens/Imagens2D/Inimigos/inimigoFE.bmp"
    let inimigoFN = rotate 270 inimigoFE
    let inimigoFO = rotate 180 inimigoFE 
    let inimigoFS = rotate 90 inimigoFE
    inimigoME <- loadBMP "Imagens/Imagens2D/Inimigos/inimigoME.bmp"
    let inimigoMN = rotate 270 inimigoME
    let inimigoMO = rotate 180 inimigoME 
    let inimigoMS = rotate 90 inimigoME
    inimigoDE <- loadBMP "Imagens/Imagens2D/Inimigos/inimigoDE.bmp"
    let inimigoDN = rotate 270 inimigoDE
    let inimigoDO = rotate 180 inimigoDE 
    let inimigoDS = rotate 90 inimigoDE
    madeiraLoja' <- loadBMP "Imagens/Imagens2D/Loja/madeiraLoja.bmp"
    let madeiraLoja = Scale 2.72 2.72 madeiraLoja' 
    portal <- loadBMP "Imagens/Imagens2D/Portais/portal.bmp"
    um <- loadBMP "Imagens/Imagens2D/Extras/1.bmp"
    zero <- loadBMP "Imagens/Imagens2D/Extras/0.bmp"
    dois <- loadBMP "Imagens/Imagens2D/Extras/2.bmp"
    tres <- loadBMP "Imagens/Imagens2D/Extras/3.bmp"
    quatro <- loadBMP "Imagens/Imagens2D/Extras/4.bmp"
    cinco <- loadBMP "Imagens/Imagens2D/Extras/5.bmp"
    seis <- loadBMP "Imagens/Imagens2D/Extras/6.bmp"
    sete <- loadBMP "Imagens/Imagens2D/Extras/7.bmp"
    oito <- loadBMP "Imagens/Imagens2D/Extras/8.bmp"
    nove <- loadBMP "Imagens/Imagens2D/Extras/9.bmp"
    doll <- loadBMP "Imagens/Imagens2D/Extras/$.bmp"
    coracao' <- loadBMP "Imagens/Imagens2D/Extras/coracao.bmp"
    let coracao =  Scale 0.19 0.19 coracao'
    mapa1' <- loadBMP "Imagens/Imagens2D/Mapas/mapa1.bmp"
    let mapa1 = Scale 2 2 mapa1' 
    vitoria' <- loadBMP "Imagens/Imagens2D/Menus/vitoria.bmp"
    let vitoria = Scale 1.5 1.5 vitoria' 
    derrota' <- loadBMP "Imagens/Imagens2D/Menus/derrota.bmp"
    let derrota = Scale 1.33 1.33 derrota' 
    esquerda <- loadBMP "Imagens/Imagens2D/Menus/esquerda.bmp"
    direita <- loadBMP "Imagens/Imagens2D/Menus/direita.bmp"
    exit <- loadBMP "Imagens/Imagens2D/Menus/exit.bmp"
    menu <- loadBMP "Imagens/Imagens2D/Menus/menu.bmp"
    playNow <- loadBMP "Imagens/Imagens2D/Menus/playNow.bmp"
    level <-loadBMP "Imagens/Imagens2D/Menus/level.bmp"
    menuFundo <- loadBMP "Imagens/Imagens2D/Menus/menuFundo.bmp"
    fundoNiveis <- loadBMP "Imagens/Imagens2D/Menus/fundoNiveis.bmp"
    level1 <- loadBMP "Imagens/Imagens2D/Menus/level1.bmp"
    level2 <- loadBMP "Imagens/Imagens2D/Menus/level2.bmp"
    level3 <- loadBMP "Imagens/Imagens2D/Menus/level3.bmp"
    level4 <- loadBMP "Imagens/Imagens2D/Menus/level4.bmp"
    level5 <- loadBMP "Imagens/Imagens2D/Menus/level5.bmp"
    acelera <- loadBMP "Imagens/Imagens2D/Menus/acelera.bmp"
    trava <- loadBMP "Imagens/Imagens2D/Menus/trava.bmp"
    pausa <- loadBMP "Imagens/Imagens2D/Menus/pausa.bmp"
    quit <- loadBMP "Imagens/Imagens2D/Menus/quit.bmp"
    fundoMenuInGame <- loadBMP "Imagens/Imagens2D/Menus/fundoMenuInGame.bmp"
    carroN <- loadBMP "Imagens/Imagens3D/Inimigos/carroN.bmp"
    carroS <- loadBMP "Imagens/Imagens3D/Inimigos/carroS.bmp"
    carroO <- loadBMP "Imagens/Imagens3D/Inimigos/carroO.bmp"
    carroE <- loadBMP "Imagens/Imagens3D/Inimigos/carroE.bmp"
    carroNC <- loadBMP "Imagens/Imagens3D/Inimigos/carroNC.bmp"
    carroSC <- loadBMP "Imagens/Imagens3D/Inimigos/carroSC.bmp"
    carroOC <- loadBMP "Imagens/Imagens3D/Inimigos/carroOC.bmp"
    carroEC <- loadBMP "Imagens/Imagens3D/Inimigos/carroEC.bmp"
    baseFogo' <- loadBMP "Imagens/Imagens3D/Torres/baseFogo.bmp"
    let baseFogo = Scale 1.4 1.4 baseFogo'
    meioFogo' <- loadBMP "Imagens/Imagens3D/Torres/meioFogo.bmp"
    let meioFogo = Scale 1.4 1.4 meioFogo'
    topoFogo' <- loadBMP "Imagens/Imagens3D/Torres/topoFogo.bmp"
    let topoFogo = Scale 1.4 1.4 topoFogo'
    let torreFogo3D1 = topoFogo
    let torreFogo3D2 = Pictures $ [baseFogo, Translate 0 20 topoFogo]
    let torreFogo3D3 =  Pictures $ [baseFogo,Translate 0 20 meioFogo, Translate 0 40 topoFogo]
    baseGelo' <- loadBMP "Imagens/Imagens3D/Torres/baseGelo.bmp"
    let baseGelo = Scale 1.4 1.4 baseGelo'
    meioGelo' <- loadBMP "Imagens/Imagens3D/Torres/meioGelo.bmp"
    let meioGelo = Scale 1.4 1.4 meioGelo'
    topoGelo' <- loadBMP "Imagens/Imagens3D/Torres/topoGelo.bmp"
    let topoGelo = Scale 1.4 1.4 topoGelo'
    let torreGelo3D1 = topoGelo
    let torreGelo3D2 = Pictures $ [baseGelo, Translate 0 20 topoGelo]
    let torreGelo3D3 = Pictures $ [baseGelo, Translate 0 20 meioGelo,Translate 0 40 topoGelo]
    baseResina' <- loadBMP "Imagens/Imagens3D/Torres/baseResina.bmp"
    let baseResina = Scale 1.4 1.4 baseResina'
    meioResina' <- loadBMP "Imagens/Imagens3D/Torres/meioResina.bmp"
    let meioResina = Scale 1.4 1.4 meioResina'
    topoResina' <- loadBMP "Imagens/Imagens3D/Torres/topoResina.bmp"
    let topoResina = Scale 1.4 1.4 topoResina'
    let torreResina3D1 = topoResina
    let torreResina3D2 = Pictures $ [baseResina, Translate 0 20 topoResina]
    let torreResina3D3 = Pictures $ [baseResina, Translate 0 20 meioResina, Translate 0 40 topoResina]
    basePortal <- loadBMP "Imagens/Imagens3D/Portais/basePortal.bmp"
    topoPortal <- loadBMP "Imagens/Imagens3D/Portais/topoPortal.bmp"
    let portal3D = Pictures $ [basePortal, Translate 0 20 topoPortal]
    baseBase <- loadBMP "Imagens/Imagens3D/Bases/baseBase.bmp"
    topoBase <- loadBMP "Imagens/Imagens3D/Bases/topoBase.bmp"
    let base3D = Pictures $ [baseBase, Translate 0 20 topoBase]
    torreDefault3D' <- loadBMP "Imagens/Imagens3D/Torres/torreDefault3D.bmp"
    let torreDefault3D = Scale 0.5 0.5 torreDefault3D'
    mapa2' <- loadBMP "Imagens/Imagens2D/Mapas/mapa2.bmp"
    let mapa2 = Scale 2 2 mapa2'
    mapa3' <- loadBMP "Imagens/Imagens2D/Mapas/mapa3.bmp"
    let mapa3 = Scale 2 2 mapa3'
    mapa4' <- loadBMP "Imagens/Imagens2D/Mapas/mapa4.bmp"
    let mapa4 = Scale 2 2 mapa4'
    mapa5' <- loadBMP "Imagens/Imagens2D/Mapas/mapa5.bmp"
    let mapa5 = Scale 2 2 mapa5' 
    gelo' <- loadBMP "Imagens/Imagens2D/Extras/gelo.bmp"
    let gelo = Scale 0.15 0.15 gelo'
    fire' <- loadBMP "Imagens/Imagens2D/Extras/fire.bmp"
    let fire = Scale 0.20 0.20 fire'
    slime' <- loadBMP "Imagens/Imagens2D/Extras/slime.bmp"
    let slime = Scale 0.13 0.13 slime'
    cadeado <- loadBMP "Imagens/Imagens2D/Extras/cadeado.bmp"
    botao <- loadBMP "Imagens/Imagens2D/Menus/botao.bmp"
    eDano' <- loadBMP "Imagens/Imagens2D/Menus/eDano.bmp"
    let eDano = Scale 0.38 0.38 eDano'
    eAlcance' <- loadBMP "Imagens/Imagens2D/Menus/eAlcance.bmp"
    let eAlcance = Scale 0.38 0.38 eAlcance'
    eRajada' <- loadBMP "Imagens/Imagens2D/Menus/eRajada.bmp"
    let eRajada = Scale 0.38 0.38 eRajada'
    eCiclo' <- loadBMP "Imagens/Imagens2D/Menus/eCiclo.bmp"
    let eCiclo = Scale 0.38 0.38 eCiclo'
    madeiraEv' <- loadBMP "Imagens/Imagens2D/Menus/madeiraEv.bmp"
    let madeiraEv = Scale 4.6 2.5 madeiraEv'
    sair' <- loadBMP "Imagens/Imagens2D/Menus/sair.bmp"
    let sair = Scale 0.3 0.3 sair'
    atualmente' <- loadBMP "Imagens/Imagens2D/Menus/atualmente.bmp"
    let atualmente = Scale 0.38 0.38 atualmente'
    vender' <- loadBMP "Imagens/Imagens2D/Menus/vender.bmp"
    let vender = Scale 0.28 0.28 vender'
    ponto <- loadBMP "Imagens/Imagens2D/Extras/ponto.bmp"
    pregos' <- loadBMP "Imagens/Imagens2D/Extras/pregos.bmp"
    let pregos = Scale 0.27 0.27 pregos'
    soma <- loadBMP "Imagens/Imagens2D/Extras/soma.bmp"
    nextLevel' <- loadBMP "Imagens/Imagens2D/Menus/nextLevel.bmp"
    let nextLevel = Scale 1.5 1.5 nextLevel'
    let vezes = Rotate 45 soma
    boost' <- loadBMP "Imagens/Imagens2D/Extras/boost.bmp" 
    let boost = Scale 0.4 0.4 boost'
    boostBW' <- loadBMP "Imagens/Imagens2D/Extras/boostBW.bmp" 
    let boostBW = Scale 0.4 0.4 boostBW'
    fundoSobrevivencia <- loadBMP "Imagens/Imagens2D/Menus/fundoSobrevivencia.bmp"
    survivalMode <- loadBMP "Imagens/Imagens2D/Menus/survivalMode.bmp"
    playAgain <- loadBMP "Imagens/Imagens2D/Menus/playAgain.bmp"
    doisPontos' <- loadBMP "Imagens/Imagens2D/Extras/doisPontos.bmp"
    let doisPontos = Scale 0.3 0.4 doisPontos'
    newRecord <- loadBMP "Imagens/Imagens2D/Menus/newRecord.bmp"
    highScore <- loadBMP "Imagens/Imagens2D/Menus/highScore.bmp"
    currentTime <- loadBMP "Imagens/Imagens2D/Menus/currentTime.bmp"
    timeToBeat' <- loadBMP "Imagens/Imagens2D/Menus/timeToBeat.bmp"
    let timeToBeat = Scale 0.4 0.4 timeToBeat'
    survive' <- loadBMP "Imagens/Imagens2D/Mapas/survive.bmp"
    let survive = Scale 2 2 survive'
    comoJogar <- loadBMP "Imagens/Imagens2D/Menus/comoJogar.bmp"
    pergunta <- loadBMP "Imagens/Imagens2D/Menus/pergunta.bmp"
    cj <- loadBMP "Imagens/Imagens2D/Menus/cj.bmp"
    fundoCJ <- loadBMP "Imagens/Imagens2D/Menus/fundoCJ.bmp"
    nMaximo <- loadBMP "Imagens/Imagens2D/Extras/nMaximo.bmp"
    menorResolucao <- loadBMP "Imagens/Imagens2D/Menus/1920.bmp"
    maiorResolucao <- loadBMP "Imagens/Imagens2D/Menus/2560.bmp"
    continue <- loadBMP "Imagens/Imagens2D/Menus/continue.bmp"
    return (im { imagens = 
      [ ("torreDefault", torreDefault),
        ("torreFogo", torreFogo),
        ("torreFogoProjetil", torreFogoProjetil),
        ("torreGelo", torreGelo),
        ("torreGeloProjetil", torreGeloProjetil),
        ("torreResina", torreResina),
        ("torreResinaProjetil", torreResinaProjetil),
        ("base", baseP),
        ("inimigoFN", inimigoFN),
        ("inimigoFS", inimigoFS),
        ("inimigoFO", inimigoFO),
        ("inimigoFE", inimigoFE),
        ("inimigoMN", inimigoMN),
        ("inimigoMS", inimigoMS),
        ("inimigoMO", inimigoMO),
        ("inimigoME", inimigoME),
        ("inimigoDN", inimigoDN),
        ("inimigoDS", inimigoDS),
        ("inimigoDO", inimigoDO),
        ("inimigoDE", inimigoDE),
        ("inimigoFNC", inimigoFNC),
        ("inimigoFSC", inimigoFSC),
        ("inimigoFOC", inimigoFOC),
        ("inimigoFEC", inimigoFEC),
        ("inimigoMNC", inimigoMNC),
        ("inimigoMSC", inimigoMSC),
        ("inimigoMOC", inimigoMOC),
        ("inimigoMEC", inimigoMEC),
        ("inimigoDNC", inimigoDNC),
        ("inimigoDSC", inimigoDSC),
        ("inimigoDOC", inimigoDOC),
        ("inimigoDEC", inimigoDEC),
        ("madeiraLoja", madeiraLoja),
        ("portal", portal),
        ("1", um),
        ("0",zero),
        ("2", dois),
        ("3", tres),
        ("4", quatro),
        ("5", cinco),
        ("6", seis),
        ("7", sete),
        ("8", oito),
        ("9", nove),
        ("$", doll),
        ("coracao", coracao),
        ("mapa1", mapa1),
        ("vitoria",vitoria),
        ("derrota", derrota),
        ("trava", trava ),
        ("acelera",acelera),
        ("esquerda",esquerda),
        ("direita", direita),
        ("playNow", playNow),
        ("menu",menu),
        ("exit", exit),
        ("level",level),
        ("menuFundo", menuFundo),
        ("fundoNiveis", fundoNiveis),
        ("level1",level1),
        ("level2",level2),
        ("level3",level3),
        ("level4",level4),
        ("level5",level5), 
        ("pausa", pausa),
        ("quit", quit),
        ("fundoMenuInGame", fundoMenuInGame),
        ("carroN", carroN),
        ("carroS", carroS),
        ("carroO", carroO),
        ("carroE", carroE),
        ("carroNC", carroNC),
        ("carroSC", carroSC),
        ("carroOC", carroOC),
        ("carroEC", carroEC),
        ("torreFogo3D3", torreFogo3D3),
        ("torreFogo3D2", torreFogo3D2),
        ("torreFogo3D1", torreFogo3D1),
        ("torreGelo3D3", torreGelo3D3),
        ("torreGelo3D2", torreGelo3D2),
        ("torreGelo3D1", torreGelo3D1),
        ("torreResina3D3", torreResina3D3),
        ("torreResina3D2", torreResina3D2),
        ("torreResina3D1", torreResina3D1),
        ("portal3D", portal3D),
        ("base3D", base3D),
        ("torreDefault3D", torreDefault3D), 
        ("mapa2", mapa2),
        ("mapa3", mapa3),
        ("mapa4", mapa4),
        ("mapa5", mapa5),
        ("gelo", gelo),
        ("fire", fire),
        ("slime", slime),
        ("cadeado", cadeado),
        ("botao", botao),
        ("eDano", eDano),
        ("eAlcance", eAlcance),
        ("eRajada", eRajada),
        ("eCiclo", eCiclo),
        ("madeiraEv", madeiraEv),
        ("sair", sair),
        ("atualmente", atualmente),
        ("vender", vender),
        ("ponto", ponto),
        ("pregos", pregos),
        ("vezes", vezes),
        ("nextLevel", nextLevel),
        ("boost", boost),
        ("boostBW",boostBW),
        ("fundoSobrevivencia", fundoSobrevivencia),
        ("survivalMode", survivalMode),
        ("playAgain", playAgain),
        ("doisPontos", doisPontos),
        ("newRecord", newRecord),
        ("highScore", highScore),
        ("currentTime", currentTime),
        ("timeToBeat", timeToBeat),
        ("survive", survive),
        ("comoJogar", comoJogar),
        ("pergunta", pergunta),
        ("cj", cj),
        ("fundoCJ", fundoCJ),
        ("nMaximo", nMaximo),
        ("menorResolucao", menorResolucao),
        ("maiorResolucao", maiorResolucao),
        ("continue", continue)
      ]
    })

-- | Definição do immutableTowers quando o jogo é iniciado
immutableTowersInit :: ImmutableTowers
immutableTowersInit = ImmutableTowers 
    {
    estadoJogo = MenuInicial,
    devolveJogo = jogoN1, 
    imagens = [] ,
    desenhaLoja = False,
    progressoJogo = Progresso {nivelAtual = 0, niveisDesbloqueados = [jogoN1]},
    niveis= [jogoN1, jogoN2, jogoN3, jogoN4, jogoN5],
    escalaAtual = escalaInicial,
    modoJogo = Levels,
    tempoRecorde = 0,
    tempoJogo = 0,
    botOn = False
    }




-- | Função responsável por ler o estado do progresso do jogo e guardá-lo no ficheiro save.txt
leProgresso :: IO Progresso
leProgresso = 
    do exists <- doesFileExist "save.txt"
       if exists then do
           progresso <- readFile "save.txt"
           return (read progresso)
       else return $ Progresso {nivelAtual = 0, niveisDesbloqueados = [jogoN1]}
  
-- | Função responsável por ler tempo recordo do jogo e guardá-lo no ficheiro record.txt
leTempoRecorde :: IO  Float
leTempoRecorde = 
    do exists <- doesFileExist "record.txt"
       if exists then do
           tempoRecorde <- readFile "record.txt"
           return  (read tempoRecorde)
        else return 0


-- | Função main
main :: IO ()
main = do
  
  progresso <- leProgresso
  tempoR <- leTempoRecorde
  immutableTowersI' <- upLoadImages immutableTowersInit
  let immutableTowersI = immutableTowersI' {progressoJogo = progresso, tempoRecorde = tempoR} 
  playIO janela fundo fr immutableTowersI desenha reageEventos reageTempo

