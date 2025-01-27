{-|
Module      : nivel1
Description : Este módulo contém a definição do primeiro nível do jogo.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo que contém a definição do primeiro nível do jogo.
-}
module Nivel1 where 

import LI12425
import Tarefa3
  
-- | Base do nivel 1 
baseN1 :: Base 
baseN1 = Base
      { vidaBase = 100,
        posicaoBase = (12.5, 1.5),
        creditosBase = 55,
        pregosBase = 3 
      }

-- | Portais do nivel 1 
portaisN1 :: [Portal]
portaisN1 =
      [ Portal
          { posicaoPortal = (1.5, 1.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                       repete 15 (Inimigo
                          { posicaoInimigo = (1.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 75,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 5,
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
                    repete 25 (Inimigo
                          { posicaoInimigo = (1.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 5,
                            butimInimigo = 4,
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
                    repete 30 (Inimigo
                          { posicaoInimigo = (1.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 125,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 6,
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
                    repete 40 (Inimigo
                          { posicaoInimigo = (1.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 150,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 7,
                            butimInimigo = 2,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }) 
                    ,
                    cicloOnda = 1.5,
                    tempoOnda = 0,
                    entradaOnda = 5.0
                  },
                Onda
                  { inimigosOnda =
                    repete 50 (Inimigo
                          { posicaoInimigo = (1.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 200,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 8,
                            butimInimigo = 1,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }) 
                    ,
                    cicloOnda = 1,
                    tempoOnda = 0,
                    entradaOnda = 5.0
                  },
                Onda
                  { inimigosOnda =
                    repete 60 (Inimigo
                          { posicaoInimigo = (1.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 250,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 10,
                            butimInimigo = 1,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }) 
                    ,
                    cicloOnda = 0.8,
                    tempoOnda = 0,
                    entradaOnda = 5.0
                  }
              ]
          },
        Portal
          { posicaoPortal = (9.5, 9.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                      repete 5 (Inimigo
                          { posicaoInimigo = (9.5, 9.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 50,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 6,
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
                      repete 8 (Inimigo
                          { posicaoInimigo = (9.5, 9.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 75,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 7,
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
                      repete 10 (Inimigo
                          { posicaoInimigo = (9.5, 9.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 125,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 8,
                            butimInimigo = 2,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 3.5,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  },
                  Onda
                  { inimigosOnda =
                      repete 12 (Inimigo
                          { posicaoInimigo = (9.5,9.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 125,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 9,
                            butimInimigo = 1,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 3.0,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  },
                  Onda
                  { inimigosOnda =
                      repete 15 (Inimigo
                          { posicaoInimigo = (9.5, 9.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 150,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 10,
                            butimInimigo = 1,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 2.5,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  },
                  Onda
                  { inimigosOnda =
                      repete 20 (Inimigo
                          { posicaoInimigo = (9.5, 9.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 200,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 12,
                            butimInimigo = 1,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 2,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  }
              ]
          }
      ]

-- | Torres do nivel 1 
torresN1 :: [Torre]
torresN1 = []

-- | Mapa do nivel 1 
mapaN1 :: Mapa    
mapaN1 =  
      [ [r,r,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,r,r,r],
        [t,t,t,t,t,t,t,r,r,a,r,r,t,r,r,r,r,r,r,r,r],
        [r,r,r,r,r,r,t,r,r,a,r,r,c,r,r,r,r,r,r,r,r],
        [r,r,r,r,r,r,t,r,r,a,a,a,c,a,a,a,a,a,a,a,a],
        [r,r,r,r,r,r,t,r,r,a,r,r,c,r,r,r,r,r,r,r,r],
        [r,t,t,t,t,t,t,r,r,a,r,r,t,r,r,r,r,r,r,r,r],
        [r,t,r,r,r,r,r,r,r,a,r,r,t,t,t,t,t,t,t,r,r],
        [r,t,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,t,r,r],
        [r,t,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,t,r,r],
        [r,t,r,r,t,t,t,t,t,t,t,t,t,r,r,r,r,r,t,r,r],
        [r,t,t,t,t,r,r,r,r,a,r,r,t,t,t,t,t,t,t,r,r],
        [r,r,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,r,r,r]
      ]
      where 
        t = Terra 
        a = Agua 
        r = Relva 
        c = Cimento

-- | inimigos do nivel 1 
inimigosN1 :: [Inimigo]
inimigosN1 = []

-- | loja do nivel 1
lojaN1 :: Loja
lojaN1 =
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
boostN1 :: Boost
boostN1 = Boost {
  velocidadeBoost = 1, 
  velocidadeCBoost = 2, 
  velocidadeSBoost = 1, 
  duracaoBoost = 7, 
  tempoBoost = 7, 
  numeroBoost = 2, 
  ativoBoost = False 
}

-- | tiros associados ao nivel 1
tirosN1 :: [Tiro]
tirosN1 = []


-- | jogo nivel 1

jogoN1 :: Jogo
jogoN1 = Jogo { 
  baseJogo = baseN1, 
  portaisJogo = portaisN1, 
  torresJogo = torresN1, 
  mapaJogo = mapaN1, 
  inimigosJogo = inimigosN1, 
  lojaJogo = lojaN1, 
  tirosJogo = tirosN1,
  torreSelecionada = Nothing,
  posicaoRato = (0,0),
  velocidadeJogo = 1,
  terceiraDimensao = False,
  torreAMelhorar = Nothing,
  pregosJogo = [],
  pregoSelecionado = Nothing,
  boostJogo = boostN1
}
