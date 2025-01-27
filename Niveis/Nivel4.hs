{-|
Module      : Nivel4
Description : Este módulo contém a definição do quarto nível do jogo.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo que contém a definição do quarto nível do jogo.
-}
module Nivel4 where


import LI12425
import Tarefa3

-- | Base do nivel 4
baseN4 :: Base 
baseN4 = Base
      { vidaBase = 100,
        posicaoBase = (1.5, 4.5),
        creditosBase = 75,
        pregosBase = 3
      }

-- | Portais do nivel 4
portaisN4 :: [Portal]
portaisN4 =
      [ Portal
          { posicaoPortal = (3.5, 0.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                       repete 15 (Inimigo
                          { posicaoInimigo = (3.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 90,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 6,
                            butimInimigo = 3,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          })
                    ,
                    cicloOnda = 3.5,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  },
                Onda
                  { inimigosOnda =
                    repete 25 (Inimigo
                          { posicaoInimigo = (3.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 150,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 7,
                            butimInimigo = 2,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          })
                    ,
                    cicloOnda = 3.0,
                    tempoOnda = 0,
                    entradaOnda = 5.0
                  },
                Onda
                  { inimigosOnda =
                    repete 35 (Inimigo
                          { posicaoInimigo = (3.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 190,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 8,
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
                          { posicaoInimigo = (3.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 220,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 9,
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
          { posicaoPortal = (12.5, 4.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                      repete 20 (Inimigo
                          { posicaoInimigo = (12.5, 4.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 80,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 6,
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
                      repete 40 (Inimigo
                          { posicaoInimigo = (12.5, 4.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 110,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 7,
                            butimInimigo = 2,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 3.0,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  },
                  Onda
                  { inimigosOnda =
                      repete 25 (Inimigo
                          { posicaoInimigo = (12.5, 4.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 220,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 8,
                            butimInimigo = 1,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 2.0,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  }
              ]
          }
      ]

-- | torres do nivel 4
torresN4 :: [Torre]
torresN4 = []

-- |  mapa do nivel 4
mapaN4 :: Mapa    
mapaN4 =  
      [ [r,r,r,t,r,r,r,r,r,a,r,r,r,r,r,r,r,r,r,r,r],
        [r,r,r,t,t,t,t,t,t,t,t,t,t,c,c,c,c,t,t,r,r],
        [r,r,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,t,r,r],
        [r,r,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,t,r,r],
        [r,t,r,r,r,r,t,t,c,c,c,t,t,r,r,r,r,r,t,r,r],
        [r,t,r,r,r,r,t,r,r,a,r,r,t,r,r,r,r,r,t,r,r],
        [r,t,r,r,r,r,t,r,r,a,r,r,t,r,r,r,r,r,t,r,r],
        [a,t,a,a,a,a,t,a,a,a,a,a,t,a,a,a,a,a,t,a,a],
        [r,t,r,r,r,r,t,r,r,a,r,r,t,r,r,r,r,r,t,r,r],
        [r,t,r,r,t,t,t,r,r,a,r,r,t,r,r,r,r,r,t,r,r],
        [r,t,t,t,t,r,r,r,r,a,r,r,t,t,c,c,c,t,t,r,r],
        [r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r]
      ]
      where 
        t = Terra 
        a = Agua 
        r = Relva 
        c = Cimento

-- | inimigos do nivel 4
inimigosN4 :: [Inimigo]
inimigosN4 = []

-- | loja do nivel 4
lojaN4 :: Loja
lojaN4  =
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

-- | Boosts inicias e as suas caracteristicas
boostN4 :: Boost
boostN4 = Boost {
  velocidadeBoost = 1, 
  velocidadeCBoost = 2, 
  velocidadeSBoost = 1, 
  duracaoBoost = 7, 
  tempoBoost = 7, 
  numeroBoost = 2, 
  ativoBoost = False 
}

-- | Tiros (inicialmente vazios)
tirosN4 :: [Tiro]
tirosN4 = []

-- | jogo do nivel 4
jogoN4 :: Jogo
jogoN4 = Jogo { 
  baseJogo = baseN4, 
  portaisJogo = portaisN4, 
  torresJogo = torresN4, 
  mapaJogo = mapaN4, 
  inimigosJogo = inimigosN4, 
  lojaJogo = lojaN4, 
  tirosJogo = tirosN4,
  torreSelecionada = Nothing,
  posicaoRato = (0,0),
  velocidadeJogo = 1,
  terceiraDimensao = False,
  torreAMelhorar = Nothing,
  pregosJogo = [],
  pregoSelecionado = Nothing,
  boostJogo = boostN4
}