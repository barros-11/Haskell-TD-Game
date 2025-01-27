{-|
Module      : Nivel2
Description : Este módulo contém a definição do jogo para o Nível 2.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo que contém a definição do jogo para o Nível 2.
-}
module Nivel2 where 


import LI12425
import Tarefa3
  
-- | Base (mesma do Nível 1)
baseN2 :: Base 
baseN2 = Base
      { vidaBase = 100,
        posicaoBase = (1.5, 5.5),
        creditosBase = 55, 
        pregosBase = 3
      }

-- | Portais (ajustes para mais dificuldade)
portaisN2 :: [Portal]
portaisN2 =
      [ Portal
          { posicaoPortal = (1.5, 1.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                       repete 20 (Inimigo
                          { posicaoInimigo = (1.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 6,
                            butimInimigo = 4,
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
                    repete 35 (Inimigo
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
                    repete 45 (Inimigo
                          { posicaoInimigo = (1.5, 1.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 170,
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
                  }
              ]
          },
        Portal
          { posicaoPortal = (18.5, 6.5),
            ondasPortal =
              [ Onda
                  { inimigosOnda =
                      repete 20 (Inimigo
                          { posicaoInimigo = (18.5, 6.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 7,
                            butimInimigo = 3,
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
                          { posicaoInimigo = (18.5, 6.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 120,
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
                      repete 35 (Inimigo
                          { posicaoInimigo = (18.5, 6.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 230,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 9,
                            butimInimigo = 1,
                            projeteisInimigo = [],
                            visivelInimigo = True
                          }),
                    cicloOnda = 1.8,
                    tempoOnda = 0.0,
                    entradaOnda = 5.0
                  }
              ]
          }
      ]

-- | Torres (mesmas do Nível 1)
torresN2 :: [Torre]
torresN2 = []

-- | Novo Mapa 2
mapaN2 :: Mapa    
mapaN2 =  
      [ [r,r,r,r,r,r,r,r,r,a,r,r,a,r,r,r,r,r,r,r,r],
        [t,t,t,t,t,t,t,r,r,a,r,r,a,a,a,a,a,a,a,a,a],
        [r,r,r,r,r,r,t,r,r,a,r,r,r,r,r,r,r,r,r,r,r],
        [r,r,r,r,r,r,t,r,r,a,r,r,r,r,r,r,r,r,r,r,r],
        [r,r,r,r,r,r,t,t,c,c,c,t,t,r,r,r,r,r,r,r,r],
        [r,t,r,r,r,r,r,r,r,a,r,r,t,r,r,r,r,r,r,r,r],
        [r,t,r,r,r,r,r,r,r,a,r,r,t,t,t,t,t,t,t,r,r],
        [r,t,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,t,r,r],
        [r,t,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,c,r,r],
        [r,t,r,r,t,t,t,t,c,c,c,t,t,r,r,r,r,r,c,r,r],
        [r,t,t,t,t,r,r,r,r,a,r,r,t,t,t,t,t,c,c,r,r],
        [r,r,r,r,r,r,r,r,r,a,r,r,r,r,r,r,r,r,r,r,r]
      ]
      where 
        t = Terra 
        a = Agua 
        r = Relva 
        c = Cimento

-- | Inimigos (inicialmente vazios)
inimigosN2 :: [Inimigo]
inimigosN2 = []

-- | Loja (mesma do Nível 1)
lojaN2 :: Loja
lojaN2  =
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
boostN2 :: Boost
boostN2 = Boost {
  velocidadeBoost = 1, 
  velocidadeCBoost = 2, 
  velocidadeSBoost = 1, 
  duracaoBoost = 7, 
  tempoBoost = 7, 
  numeroBoost = 2, 
  ativoBoost = False 
}

-- | Tiros (inicialmente vazios)
tirosN2 :: [Tiro]
tirosN2 = []

-- | Definição do jogo para o Nível 2
jogoN2 :: Jogo
jogoN2 = Jogo { 
  baseJogo = baseN2, 
  portaisJogo = portaisN2, 
  torresJogo = torresN2, 
  mapaJogo = mapaN2, 
  inimigosJogo = inimigosN2, 
  lojaJogo = lojaN2, 
  tirosJogo = tirosN2,
  torreSelecionada = Nothing,
  posicaoRato = (0,0),
  velocidadeJogo = 1,
  terceiraDimensao = False,
  torreAMelhorar = Nothing ,
  pregosJogo = [],
  pregoSelecionado = Nothing,
  boostJogo = boostN2
  }
         
