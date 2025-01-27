{-|
Module      : Nivel5
Description : Este módulo contém a definição do jogo para o Nível 5.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo que contém a definição do jogo para o Nível 5.
-}
module Nivel5 where


import LI12425
import Tarefa3
  
-- | Base do nivel 5
baseN5 :: Base 
baseN5 = Base
      { vidaBase = 100,
        posicaoBase = (19.5, 2.5),
        creditosBase = 75,
        pregosBase = 3
      }

-- | Portais do nivel 5
portaisN5 :: [Portal]
portaisN5 =
      [ Portal
          { posicaoPortal = (2.5, 0.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                       repete 15 (Inimigo
                          { posicaoInimigo = (2.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 90,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 6,
                            butimInimigo = 3,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          })
                    ,
                    cicloOnda = 3,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  },
                Onda
                  { inimigosOnda =
                    repete 25 (Inimigo
                          { posicaoInimigo = (2.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 125,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 6,
                            butimInimigo = 2,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }) 
                    ,
                    cicloOnda = 2.5,
                    tempoOnda = 0,
                    entradaOnda = 5.0
                  },
                Onda
                  { inimigosOnda =
                    repete 40 (Inimigo
                          { posicaoInimigo = (2.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 180,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 9,
                            butimInimigo = 2,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }) 
                    ,
                    cicloOnda = 2,
                    tempoOnda = 0,
                    entradaOnda = 5.0
                  },
                Onda
                  { inimigosOnda =
                    repete 50 (Inimigo
                          { posicaoInimigo = (2.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 280,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 10,
                            butimInimigo = 1,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }) 
                    ,
                    cicloOnda = 1.5,
                    tempoOnda = 0,
                    entradaOnda = 5.0
                  }
              ]
          },
        Portal
          { posicaoPortal = (9.5, 1.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                      repete 15 (Inimigo
                          { posicaoInimigo = (9.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 100,
                            velocidadeInimigo = 2,
                            ataqueInimigo = 7,
                            butimInimigo = 2,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 5.0,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  },
                  Onda
                  { inimigosOnda =
                      repete 20 (Inimigo
                          { posicaoInimigo = (9.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 200,
                            velocidadeInimigo = 2,
                            ataqueInimigo = 8,
                            butimInimigo = 2,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 4.0,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  },
                  Onda
                  { inimigosOnda =
                      repete 30 (Inimigo
                          { posicaoInimigo = (9.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 280,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 9,
                            butimInimigo = 1,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 3.5,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  }
              ]
          }
      ]

-- | Torres do nivel 5
torresN5 :: [Torre]
torresN5 = []

-- | Mapa 5 do nivel 5
mapaN5 :: Mapa    
mapaN5 =  
      [ [r,r,t,r,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,r],
        [a,a,t,a,a,a,a,a,t,t,t,t,t,t,t,t,t,r,r,r,r],
        [r,r,t,r,r,r,r,r,t,r,r,a,r,r,r,r,t,r,r,t,r],
        [r,r,t,r,r,r,r,r,t,r,r,a,r,r,r,r,t,r,r,t,r],
        [a,a,t,t,t,a,a,a,t,r,a,a,a,a,a,a,t,a,a,t,a],
        [r,r,r,r,t,r,r,r,t,r,r,a,r,r,r,r,t,r,r,t,r],
        [r,r,r,r,t,r,r,r,c,r,r,a,r,r,r,r,c,r,r,t,r],
        [r,t,t,t,t,r,r,r,c,r,r,a,r,r,c,c,c,r,r,t,r],
        [r,t,r,r,r,r,r,r,c,r,r,a,r,r,c,r,r,r,r,t,r],
        [r,t,r,r,r,r,r,r,c,r,r,a,r,r,c,r,r,r,r,t,r],
        [r,t,t,t,t,c,c,c,c,r,r,a,r,r,c,c,t,t,t,t,r],
        [r,r,r,r,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,r]
      ]
      where 
        t = Terra 
        a = Agua 
        r = Relva 
        c = Cimento

-- | Inimigos (inicialmente vazios)
inimigosN5 :: [Inimigo]
inimigosN5 = []

-- | Loja do nivel 5
lojaN5 :: Loja
lojaN5  =
      [ (40, Torre
          { posicaoTorre = (0, 0),
            danoTorre = 25,
            alcanceTorre = 3.5,
            rajadaTorre = 1,
            cicloTorre = 2.0,
            tempoTorre = 0,
            projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},
            nivelTorre = 1,
            tipoEvolucao = Nada 
          }),
        (35, Torre
          { posicaoTorre = (0, 0),
            danoTorre = 30,
            alcanceTorre = 3.5,
            rajadaTorre = 1,
            cicloTorre = 1.5,
            tempoTorre = 0,
            projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
            nivelTorre = 1,
            tipoEvolucao = Nada  
          }),
        (35, Torre
          { posicaoTorre = (0, 0),
            danoTorre = 35,
            alcanceTorre = 3.5,
            rajadaTorre = 1,
            cicloTorre = 1.5,
            tempoTorre = 0,
            projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2 },
            nivelTorre = 1,
            tipoEvolucao = Nada  
          })
      ]
      
-- |Boosts inicias e as suas caracteristicas
boostN5 :: Boost
boostN5 = Boost {
  velocidadeBoost = 1, 
  velocidadeCBoost = 2, 
  velocidadeSBoost = 1, 
  duracaoBoost = 7, 
  tempoBoost = 7, 
  numeroBoost = 2, 
  ativoBoost = False 
}

-- | Tiros (inicialmente vazios)
tirosN5 :: [Tiro]
tirosN5 = []

-- | Definição do jogo para o Nível 5
jogoN5 :: Jogo
jogoN5 = Jogo { 
  baseJogo = baseN5, 
  portaisJogo = portaisN5, 
  torresJogo = torresN5, 
  mapaJogo = mapaN5, 
  inimigosJogo = inimigosN5, 
  lojaJogo = lojaN5, 
  tirosJogo = tirosN5,
  torreSelecionada = Nothing,
  posicaoRato = (0,0),
  velocidadeJogo = 1,
  terceiraDimensao = False ,
  torreAMelhorar = Nothing,
  pregosJogo = [],
  pregoSelecionado = Nothing,
  boostJogo = boostN5
}
         



