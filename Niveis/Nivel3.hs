{-|
Module      : Nivel3
Description : Este módulo contém a definição do estado inicial do jogo no nível 3.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo que contém a definição do estado inicial do jogo no nível 3.
-}
module Nivel3 where 

import LI12425
import Tarefa3

-- | Base do nivel 3
baseN3 :: Base 
baseN3 = Base
      { vidaBase = 100,
        posicaoBase = (19.5, 2.5),
        creditosBase = 55,
        pregosBase = 2
      }

-- | Portais do nivel 3
portaisN3 :: [Portal]
portaisN3 =
      [ Portal
          { posicaoPortal = (2.5, 0.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                       repete 25 (Inimigo
                          { posicaoInimigo = (2.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 120,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 7,
                            butimInimigo = 5,
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
                    repete 35 (Inimigo
                          { posicaoInimigo = (2.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 140,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 8,
                            butimInimigo = 3,
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
                    repete 45 (Inimigo
                          { posicaoInimigo = (2.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 160,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 9,
                            butimInimigo = 3,
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
                    repete 55 (Inimigo
                          { posicaoInimigo = (2.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 220,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 10,
                            butimInimigo = 2,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }) 
                    ,
                    cicloOnda = 1,
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
                            vidaInimigo = 90,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 8,
                            butimInimigo = 4,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 5.0,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  },
                  Onda
                  { inimigosOnda =
                      repete 30 (Inimigo
                          { posicaoInimigo = (9.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 150,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 9,
                            butimInimigo = 3,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 4.0,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  },
                  Onda
                  { inimigosOnda =
                      repete 60 (Inimigo
                          { posicaoInimigo = (9.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 250,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 10,
                            butimInimigo = 2,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 1.5,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  }
              ]
          }
      ]

-- | Torres do nivel 3
torresN3 :: [Torre]
torresN3 = []

-- | mapas do nivel 3
mapaN3 :: Mapa    
mapaN3 =  
      [ [r,r,t,r,r,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r],
        [a,a,t,a,a,a,a,a,a,t,t,t,t,t,t,t,t,r,r,r,r],
        [r,r,t,r,r,r,r,r,r,t,r,r,a,r,r,r,t,r,r,t,r],
        [r,r,t,r,r,r,r,r,r,t,r,r,a,r,r,r,c,r,r,c,r],
        [r,r,t,t,t,r,r,r,r,c,r,r,a,r,r,r,c,r,r,c,r],
        [r,r,r,r,t,r,r,r,r,c,r,r,a,r,r,r,c,r,r,c,r],
        [r,r,r,r,t,r,r,r,r,c,r,r,r,r,r,r,t,r,r,t,r],
        [r,t,t,t,t,r,r,r,r,c,r,r,r,r,t,t,t,r,r,t,r],
        [r,t,r,r,r,r,r,r,r,t,r,r,r,r,t,r,r,r,r,t,r],
        [r,t,r,r,r,r,r,r,r,t,r,r,r,r,t,r,r,r,r,t,r],
        [r,t,t,t,t,t,t,t,t,t,r,r,r,r,t,t,t,t,t,t,r],
        [r,r,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,r,r,r]
      ]
      where 
        t = Terra 
        a = Agua 
        r = Relva 
        c = Cimento

-- | inimigos do nivel 3
inimigosN3 :: [Inimigo]
inimigosN3 = []


-- | loja do nivel 3
lojaN3 :: Loja
lojaN3  =
      [ (35, Torre
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
        (30, Torre
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
        (30, Torre
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
boostN3 :: Boost
boostN3 = Boost {
  velocidadeBoost = 1, 
  velocidadeCBoost = 2, 
  velocidadeSBoost = 1, 
  duracaoBoost = 7, 
  tempoBoost = 7, 
  numeroBoost = 2, 
  ativoBoost = False 
}

-- | Tiros (inicialmente vazios)
tirosN3 :: [Tiro]
tirosN3 = []

-- | jogo do nivel 3
jogoN3 :: Jogo
jogoN3 = Jogo { 
  baseJogo = baseN3, 
  portaisJogo = portaisN3, 
  torresJogo = torresN3, 
  mapaJogo = mapaN3, 
  inimigosJogo = inimigosN3, 
  lojaJogo = lojaN3, 
  tirosJogo = tirosN3,
  torreSelecionada = Nothing,
  posicaoRato = (0,0),
  velocidadeJogo = 1,
  terceiraDimensao = False,
  torreAMelhorar = Nothing,
  pregosJogo = [],
  pregoSelecionado = Nothing, 
  boostJogo = boostN3
}
