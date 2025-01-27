{-|
Module      : Tarefa3Spec
Description : Este módulo contém definições inerentes à realização da Tarefa 3 de LI1 em 2024/25.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo qye contém definições inerentes à realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import LI12425
import Tarefa3

-- | Exemplos base
baseTeste :: Base
baseTeste = Base
  { vidaBase = 100,
    posicaoBase = (3.5, 1.5),
    creditosBase = 50, 
    pregosBase = 2
  }

-- | Exemplos de portais
portalTeste :: [Portal] 
portalTeste =
  [ 
    Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal =
          [ Onda
              { inimigosOnda =
                  [ Inimigo
                      { posicaoInimigo = (0.5, 0.5),
                        direcaoInimigo = Este,
                        vidaInimigo = 10,
                        velocidadeInimigo = 1.0,
                        ataqueInimigo = 5,
                        butimInimigo = 10,
                        projeteisInimigo = [],
                        visivelInimigo = True
                      }
                  ],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 0
              }
          ]
      },
    Portal
      { posicaoPortal = (1.5, 2.5),
        ondasPortal =
          [ Onda
              { inimigosOnda =
                  [ Inimigo
                      { posicaoInimigo = (1.5, 2.5),
                        direcaoInimigo = Este,
                        vidaInimigo = 10,
                        velocidadeInimigo = 1.0,
                        ataqueInimigo = 5,
                        butimInimigo = 10,
                        projeteisInimigo = [],
                        visivelInimigo = True
                      }
                  ],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 10
              }
          ]
      }
  ]

-- | Exemplos de torres
torreTeste :: [Torre]
torreTeste =
  [ Torre
      { posicaoTorre = (2.5, 1.5),
        danoTorre = 2.5,
        alcanceTorre = 3.0,
        rajadaTorre = 1,
        cicloTorre = 1.5,
        tempoTorre = 0,
        projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},
        nivelTorre = 1, 
        tipoEvolucao = Nada
        
      }
  ]
-- |Exemplos de mapa
mapaTeste :: Mapa
mapaTeste =
    [ [Terra, Terra, Agua,Agua],
      [Relva, Terra, Relva, Terra],
      [Terra, Terra, Agua, Terra],
      [Relva, Terra, Terra, Terra]  
    ]

-- | Exemplos de inimigos
inimigoTeste :: [Inimigo]
inimigoTeste =
  [ Inimigo
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
                            Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
      }
  ]


      

-- | Exemplos de loja
lojaTeste :: Loja
lojaTeste =
  [ (15, Torre
      { posicaoTorre = (0, 0),
        danoTorre = 2,
        alcanceTorre = 2.5,
        rajadaTorre = 2,
        cicloTorre = 1.0,
        tempoTorre = 0,
        projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
        nivelTorre = 1,
        tipoEvolucao = Nada
      })
  ]

-- | Exemplos de tiros
tirosTeste :: [Tiro]
tirosTeste = []

-- |Boost utilizado para testes
boostTeste:: Boost 
boostTeste = Boost 
  {velocidadeBoost = 1, 
  velocidadeCBoost = 2, 
  velocidadeSBoost = 1, 
  duracaoBoost = 5,
  tempoBoost = 5, 
  numeroBoost = 2, 
  ativoBoost = False
  }

-- | Exemplos jogo
jogoTeste :: Jogo
jogoTeste = Jogo
  { baseJogo = baseTeste,
    portaisJogo = portalTeste,
    torresJogo = torreTeste,
    mapaJogo = mapaTeste,
    inimigosJogo = inimigoTeste,
    lojaJogo = lojaTeste,
    tirosJogo = tirosTeste,
    posicaoRato = (0,0),
    torreSelecionada = Nothing,
    velocidadeJogo =1, 
    terceiraDimensao = False, 
    torreAMelhorar = Nothing, 
    pregosJogo = [], 
    pregoSelecionado = Nothing, 
    boostJogo = boostTeste
  }

-- | Exemplos resultadoTestes 
resultadoTesteJogo :: Jogo 
resultadoTesteJogo = 
   Jogo {
    baseJogo = Base 
    {vidaBase = 100.0, 
    posicaoBase = (3.5,1.5), 
    creditosBase = 50, 
    pregosBase = 2}
    ,
    portaisJogo = [
        Portal {posicaoPortal = (0.5,0.5), ondasPortal = []},
        Portal {posicaoPortal = (1.5,2.5), ondasPortal = [
            Onda {inimigosOnda = [Inimigo 
            {posicaoInimigo = (1.5,2.5), 
            direcaoInimigo = Este, 
            vidaInimigo = 10.0, 
            velocidadeInimigo = 1.0,
             ataqueInimigo = 5.0, 
             butimInimigo = 10, 
             projeteisInimigo = [], 
             visivelInimigo = True}], 
             cicloOnda = 2.0, 
             tempoOnda = 0.0, 
             entradaOnda = 9.0}
        ]}
    ],
    torresJogo = [
        Torre {posicaoTorre = (2.5,1.5), 
        danoTorre = 2.5, 
        alcanceTorre = 3.0, 
        rajadaTorre = 1, 
        cicloTorre = 1.5, 
        tempoTorre = 1.5,
         projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}, nivelTorre = 1, tipoEvolucao = Nada}
    ],
    mapaJogo = [
        [Terra, Terra, Agua, Agua],
        [Relva, Terra, Relva, Terra],
        [Terra, Terra, Agua, Terra],
        [Relva, Terra, Terra, Terra]
    ],
    inimigosJogo = [
        Inimigo 
        {posicaoInimigo = (1.5,0.5),
         direcaoInimigo = Este, 
         vidaInimigo = 10.0, 
         velocidadeInimigo = 0.8,
          ataqueInimigo = 5.0, 
          butimInimigo = 10, 
          projeteisInimigo = [], 
          visivelInimigo = True}
          ,
        Inimigo 
        {posicaoInimigo = (1.5,2.5),
         direcaoInimigo = Sul,
          vidaInimigo = 5.5, 
          velocidadeInimigo = 0.5, 
          ataqueInimigo = 5.0, 
          butimInimigo = 20, 
          projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
           visivelInimigo = True}
    ],
    lojaJogo = [
        (15, Torre {posicaoTorre = (0.0,0.0), danoTorre = 2.0, alcanceTorre = 2.5, rajadaTorre = 2, cicloTorre = 1.0, tempoTorre = 0.0, projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}, nivelTorre = 1, tipoEvolucao = Nada})
    ],
    tirosJogo = [
        ((-2.5,1.5), (1.5,1.5), (-5.0,0.0), Fogo)
    ],
    torreSelecionada = Nothing,
    posicaoRato = (0.0, 0.0),
    velocidadeJogo = 1.0,
    terceiraDimensao = False,
    torreAMelhorar = Nothing,
    pregosJogo = [],
    pregoSelecionado = Nothing,
    boostJogo = Boost {velocidadeBoost = 1.0, velocidadeCBoost = 2.0, velocidadeSBoost = 1.0, duracaoBoost = 5.0, tempoBoost = 5.0, numeroBoost = 2, ativoBoost = False}
}

-- | Exemplos da função jogo
tAtualizaJogo :: Test 
tAtualizaJogo = "Teste da função jogo" ~: resultadoTesteJogo ~=? atualizaJogo 1 jogoTeste


-- | Exemplos da torre
torreTeste1 :: [Torre]
torreTeste1 =
  [ Torre
      { posicaoTorre = (2.5, 1.5),
        danoTorre = 10,
        alcanceTorre = 1.5,
        rajadaTorre = 2,
        cicloTorre = 1.5,
        tempoTorre = 0,
        projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},
        nivelTorre = 1,
        tipoEvolucao = Nada
      }
  ]

-- | Exemplos da torre
torreTeste1' :: [Torre]
torreTeste1' =
  [ Torre
      { posicaoTorre = (3.5, 3.5),
        danoTorre = 10,
        alcanceTorre = 1.5,
        rajadaTorre = 2,
        cicloTorre = 1.5,
        tempoTorre = 0,
        projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},
        nivelTorre = 1,
        tipoEvolucao = Nada
      }
  ]

-- | Exemplos de tiros
tirosTeste1 :: [Tiro] 
tirosTeste1 = []

-- | Exemplos de inimigos
inimigoTeste1 :: [Inimigo]
inimigoTeste1 =
  [ Inimigo
      { posicaoInimigo = (1.9, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 100,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 100,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
                            Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
      }
  ]

-- | Resultado do teste 1 
resultadoTeste1 :: ([Inimigo], [Torre], [Tiro]) 
resultadoTeste1= 
  ( [ Inimigo
      { posicaoInimigo = (1.9, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 90.0,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5.0,
        butimInimigo = 20,
        projeteisInimigo =
          [ Projetil
              { tipoProjetil = Fogo,
                duracaoProjetil = Finita 8.0
              }
          ],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 90.0,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5.0,
        butimInimigo = 20,
        projeteisInimigo =
          [ Projetil
              { tipoProjetil = Resina,
                duracaoProjetil = Infinita
              }
          ],
        visivelInimigo = True
      }
  ],
  [ Torre
      { posicaoTorre = (2.5, 1.5),
        danoTorre = 10.0,
        alcanceTorre = 1.5,
        rajadaTorre = 2,
        cicloTorre = 1.5,
        tempoTorre = 1.5,
        projetilTorre = Projetil
          { tipoProjetil = Fogo,
            duracaoProjetil = Finita 5.0
          },
        nivelTorre = 1,
        tipoEvolucao = Nada
      }
  ],
  [ ((2.5, 1.5), (1.9, 1.5), (-3, 0.0), Fogo),
    ((2.5, 1.5), (1.5, 1.5), (-5.0, 0.0), Fogo)
  ]
  )

-- | Exemplos da função atualizaInimigosTorres
tAtualizaInimigosTorres :: Test
tAtualizaInimigosTorres = test ["Teste 1 da função atualizaInimigosTorres" ~: resultadoTeste1 ~=? atualizaInimigosTorres boostTeste 1  1 torreTeste1 inimigoTeste1 tirosTeste1,
                                 "Teste 2 da função atualizaInimigosTorres" ~: (inimigoTeste1, [], tirosTeste1) ~=? atualizaInimigosTorres boostTeste 1 1 [] inimigoTeste1 tirosTeste1,
                                 "Teste 3 da função atualizaInimgosTorres" ~: (inimigoTeste1, torreTeste1', []) ~=? atualizaInimigosTorres boostTeste 1 1 torreTeste1' inimigoTeste1 tirosTeste1]




-- | Exemplos de inimigos
inimigoTeste2 :: [Inimigo]
inimigoTeste2 =
  [ Inimigo
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3},
                            Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }
  ]

-- | Exemplos de inimigos
inimigoTeste2' :: [Inimigo]
inimigoTeste2' =
  [ Inimigo
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 8,
        velocidadeInimigo = 0.8,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 8,
        velocidadeInimigo = 0.8,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [],
        visivelInimigo = True
      }
  ]

-- | Exemplos de resultado
resultadoTeste2 :: [Inimigo]
resultadoTeste2 =
  [ Inimigo
    { posicaoInimigo = (1.5, 1.5),
      direcaoInimigo = Sul,
      vidaInimigo = 8.0,
      velocidadeInimigo = 0,
      ataqueInimigo = 5.0,
      butimInimigo = 20,
      projeteisInimigo =
        [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 2.0 },
          Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita }
        ],
      visivelInimigo = True      
    },
  Inimigo
    { posicaoInimigo = (1.5, 1.5),
      direcaoInimigo = Sul,
      vidaInimigo = 8.0- danoFogo,
      velocidadeInimigo = velocidadeDefault,
      ataqueInimigo = 5.0,
      butimInimigo = 20,
      projeteisInimigo =
        [ Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 2.0 }
        ],
      visivelInimigo = True
    }
  ]


-- | Testes da função atualizaInimigos
tAtualizaInimigos :: Test
tAtualizaInimigos = test ["Teste 1 da função atualizaInimigos" ~: resultadoTeste2 ~=? atualizaInimigos boostTeste 1 1 inimigoTeste2,
                          "Teste 2 da função atualizaInimigos" ~: inimigoTeste2' ~=? atualizaInimigos boostTeste 1 1 inimigoTeste2']


-- | Testes mapa
mapaTeste3 :: Mapa
mapaTeste3 =
    [ [Terra, Terra, Agua,Agua],
      [Relva, Terra, Relva, Terra],
      [Terra, Terra, Agua, Terra],
      [Relva, Terra, Terra, Terra]  
    ]

-- | Testes de base
baseTeste3 :: Base
baseTeste3 = Base
  { vidaBase = 100,
    posicaoBase = (3.5, 1.5),
    creditosBase = 50,
    pregosBase = 2
  }

-- | Testes de inimigos
inimigoTeste3 :: [Inimigo]
inimigoTeste3 =
  [ Inimigo
      { posicaoInimigo = (0.5, 0.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3},
                            Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (3.5, 2.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }, 
      Inimigo
      { posicaoInimigo = (3.2, 2.9),
        direcaoInimigo = Oeste,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }
      
  ]

-- | Testes de resultado
resultadoTeste3 :: [Inimigo]
resultadoTeste3 = [  Inimigo {    posicaoInimigo = (0.5, 0.5),    direcaoInimigo = Este,    vidaInimigo = 8.0,    velocidadeInimigo = 1.0,    ataqueInimigo = 5.0,    butimInimigo = 20,    projeteisInimigo = [      Projetil {        tipoProjetil = Gelo,        duracaoProjetil = Finita 3.0      },      Projetil {        tipoProjetil = Resina,        duracaoProjetil = Infinita      }    ],
    visivelInimigo = True
  },
  Inimigo {
    posicaoInimigo = (3.5, 2.5),
    direcaoInimigo = Norte,
    vidaInimigo = 8.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 5.0,
    butimInimigo = 20,
    projeteisInimigo = [      Projetil {        tipoProjetil = Fogo,        duracaoProjetil = Finita 3.0      }    ],
    visivelInimigo = True
  },
  Inimigo {
    posicaoInimigo = (3.2, 2.9),
    direcaoInimigo = Oeste,
    vidaInimigo = 8.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 5.0,
    butimInimigo = 20,
    projeteisInimigo = [      Projetil {        tipoProjetil = Fogo,        duracaoProjetil = Finita 3.0      }    ],
    visivelInimigo = True
  },
  Inimigo {
    posicaoInimigo = (1.5, 1.5),
    direcaoInimigo = Sul,
    vidaInimigo = 8.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 5.0,
    butimInimigo = 20,
    projeteisInimigo = [      Projetil {        tipoProjetil = Fogo,        duracaoProjetil = Finita 3.0      }    ],
    visivelInimigo = True
  }
 ]

-- | Testes da função atualizaDirecaoInimigos
tAtualizaDirecaoInimigos :: Test
tAtualizaDirecaoInimigos = "Teste da função atualizaDirecaoInimigos" ~: resultadoTeste3 ~=? atualizaDirecaoInimigos baseTeste3 mapaTeste3 inimigoTeste3


-- | Testes de inimigos
inimigoTeste4 :: [Inimigo]
inimigoTeste4 =
  [ Inimigo
      { posicaoInimigo = (0.5, 0.5),
        direcaoInimigo = Este,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3},
                            Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (3.5, 2.5),
        direcaoInimigo = Norte,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (3.5, 2.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (3.5, 2.5),
        direcaoInimigo = Sul,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }
  ]

-- | Testes de resultado
resultadoTeste4 :: [Inimigo]
resultadoTeste4 =
  [  Inimigo {    posicaoInimigo = (1.5, 0.5),    direcaoInimigo = Este,    vidaInimigo = 8.0,    velocidadeInimigo = 1.0,    ataqueInimigo = 5.0,    butimInimigo = 20,    projeteisInimigo = [      Projetil {        tipoProjetil = Gelo,        duracaoProjetil = Finita 3.0      },      Projetil {        tipoProjetil = Resina,        duracaoProjetil = Infinita      }    ],
    visivelInimigo = True
  },
  Inimigo {
    posicaoInimigo = (3.5, 1.5),
    direcaoInimigo = Norte,
    vidaInimigo = 8.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 5.0,
    butimInimigo = 20,
    projeteisInimigo = [      Projetil {        tipoProjetil = Fogo,        duracaoProjetil = Finita 3.0      }    ],
    visivelInimigo = True
  },
  Inimigo {
    posicaoInimigo = (2.5, 2.5),
    direcaoInimigo = Oeste,
    vidaInimigo = 8.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 5.0,
    butimInimigo = 20,
    projeteisInimigo = [      Projetil {        tipoProjetil = Fogo,        duracaoProjetil = Finita 3.0      }    ],
    visivelInimigo = True
  },
  Inimigo {
    posicaoInimigo = (3.5, 3.5),
    direcaoInimigo = Sul,
    vidaInimigo = 8.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 5.0,
    butimInimigo = 20,
    projeteisInimigo = [      Projetil {        tipoProjetil = Fogo,        duracaoProjetil = Finita 3.0      }    ],
    visivelInimigo = True
  }
 ]


-- | Testes da função atualizaPosicaoInimigos
tAtualizaPosicaoInimigos :: Test 
tAtualizaPosicaoInimigos = "Teste da função atualizaPosicaoInimigos" ~: resultadoTeste4 ~=? atualizaPosicaoInimigos 1 1 inimigoTeste4



-- | Testes de base
baseTeste5 :: Base
baseTeste5 = Base
  { vidaBase = 100,
    posicaoBase = (3.5, 1.5),
    creditosBase = 50,
    pregosBase = 2
  }

-- | Testes de inimigos
inimigoTeste5 :: [Inimigo]
inimigoTeste5 =
  [ Inimigo
      { posicaoInimigo = (0.5, 0.5),
        direcaoInimigo = Oeste,
        vidaInimigo = -8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 10,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3},
                            Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
        
      },
      Inimigo
      { posicaoInimigo = (3.5, 2.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }
  ]

-- | Testes de resultado
resultadosTeste5 :: ([Inimigo], Base) 
resultadosTeste5 = 
  ( [ Inimigo
      { posicaoInimigo = (3.5, 2.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8.0,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5.0,
        butimInimigo = 20,
        projeteisInimigo =
          [ Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 3.0 }
          ],
        visivelInimigo = True
      }
  ],
  Base
    { vidaBase = 100.0,
      posicaoBase = (3.5, 1.5),
      creditosBase = 60,
      pregosBase = 2
    }
  )

-- | Testes da função retiraInimigosMortos
tRetiraInimigosMortos :: Test
tRetiraInimigosMortos = "Teste da função retiraInimigosMortos" ~: resultadosTeste5 ~=? retiraInimigosMortos baseTeste5 inimigoTeste5 

-- | Testes de base
baseTeste6 :: Base
baseTeste6 = Base
  { vidaBase = 100,
    posicaoBase = (3.5, 1.5),
    creditosBase = 50,
    pregosBase = 2
  }

-- | Testes de inimigos
inimigoTeste6 :: [Inimigo]
inimigoTeste6 =
  [ Inimigo
      { posicaoInimigo = (0.5, 0.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 50,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 10,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3},
                            Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (3.5, 1.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 18,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }
  ]

  -- | Testes de resultado
resultadoTeste6 :: (Base, [Inimigo])
resultadoTeste6 = 
    ( Base
    { vidaBase = 95.0,
      posicaoBase = (3.5, 1.5),
      creditosBase = 50,
      pregosBase = 2
    },
  [ Inimigo
      { posicaoInimigo = (0.5, 0.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 50.0,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5.0,
        butimInimigo = 10,
        projeteisInimigo =
          [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 },
            Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita }
          ],
        visivelInimigo = True
      }
  ]
  ) 

-- | Testes da função atualizaVidaBase
tAtualizaVidaBase :: Test
tAtualizaVidaBase = "Teste da função atualizaVidaBase" ~: resultadoTeste6 ~=? atualizaVidaBase baseTeste6 inimigoTeste6



-- | Testes de portais
portalTeste7 :: [Portal]
portalTeste7 =
  [ 
    Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal =
          [ Onda
              { inimigosOnda =
                  [ Inimigo
                      { posicaoInimigo = (0.5, 0.5),
                        direcaoInimigo = Este,
                        vidaInimigo = 10,
                        velocidadeInimigo = 1.0,
                        ataqueInimigo = 5,
                        butimInimigo = 10,
                        projeteisInimigo = [],
                        visivelInimigo = True
                      }
                  ],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 0
              }, 
              Onda
              { inimigosOnda =
                  [ Inimigo
                      { posicaoInimigo = (1.5, 2.5),
                        direcaoInimigo = Este,
                        vidaInimigo = 10,
                        velocidadeInimigo = 1.0,
                        ataqueInimigo = 5,
                        butimInimigo = 10,
                        projeteisInimigo = [],
                        visivelInimigo = True
                      }
                  ],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 10
              }
          ]
      },
    Portal
      { posicaoPortal = (1.5, 2.5),
        ondasPortal =
          [ Onda
              { inimigosOnda =
                  [ Inimigo
                      { posicaoInimigo = (1.5, 2.5),
                        direcaoInimigo = Este,
                        vidaInimigo = 10,
                        velocidadeInimigo = 1.0,
                        ataqueInimigo = 5,
                        butimInimigo = 10,
                        projeteisInimigo = [],
                        visivelInimigo = True
                      }
                  ],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 10
              }
          ]
      }
  ]

-- | Exemplo de portais
portalTeste7' :: [Portal]
portalTeste7' =
  [ 
    Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal =
          []
      },
    Portal
      { posicaoPortal = (1.5, 2.5),
        ondasPortal =
          [ Onda
              { inimigosOnda =
                  [ ],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 10
              }
          ]
      },
      Portal
      { posicaoPortal = (1.5, 2.5),
        ondasPortal =
          [ Onda
              { inimigosOnda =
                  [ ],
                cicloOnda = 2,
                tempoOnda = 6,
                entradaOnda = 10
              }
          ]
      }
  ]

-- | Testes de inimigos
inimigoTeste7 :: [Inimigo]
inimigoTeste7 =
  [ Inimigo
      { posicaoInimigo = (0.5, 0.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 48,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 10,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3},
                            Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (3.5, 2.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }
  ]

-- | Testes de resultado
resultadoTeste7 :: ([Portal], [Inimigo])
resultadoTeste7 = 
  ( [ Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal =
          [ Onda
              { inimigosOnda =
                  [ Inimigo
                      { posicaoInimigo = (1.5, 2.5),
                        direcaoInimigo = Este,
                        vidaInimigo = 10.0,
                        velocidadeInimigo = 1.0,
                        ataqueInimigo = 5.0,
                        butimInimigo = 10,
                        projeteisInimigo = [],
                        visivelInimigo = True
                      }
                  ],
                cicloOnda = 2.0,
                tempoOnda = 0.0,
                entradaOnda = 10.0
              }
          ]
      },
    Portal
      { posicaoPortal = (1.5, 2.5),
        ondasPortal =
          [ Onda
              { inimigosOnda =
                  [ Inimigo
                      { posicaoInimigo = (1.5, 2.5),
                        direcaoInimigo = Este,
                        vidaInimigo = 10.0,
                        velocidadeInimigo = 1.0,
                        ataqueInimigo = 5.0,
                        butimInimigo = 10,
                        projeteisInimigo = [],
                        visivelInimigo = True
                      }
                  ],
                cicloOnda = 2.0,
                tempoOnda = 0.0,
                entradaOnda = 9.0
              }
          ]
      }
  ],
  [ Inimigo
      { posicaoInimigo = (0.5, 0.5),
        direcaoInimigo = Este,
        vidaInimigo = 10.0,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5.0,
        butimInimigo = 10,
        projeteisInimigo = [],
        visivelInimigo = True
      },
    Inimigo
      { posicaoInimigo = (0.5, 0.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 48.0,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5.0,
        butimInimigo = 10,
        projeteisInimigo =
          [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 },
            Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita }
          ],
        visivelInimigo = True
      },
    Inimigo
      { posicaoInimigo = (3.5, 2.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8.0,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5.0,
        butimInimigo = 20,
        projeteisInimigo =
          [ Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 3.0 }
          ],
        visivelInimigo = True
      }
  ]
  )  

resultadoTeste7' :: ([Portal], [Inimigo])
resultadoTeste7' = 
  (
  [
    Portal {
      posicaoPortal = (0.5, 0.5),
      ondasPortal = []
    },
    Portal {
      posicaoPortal = (1.5, 2.5),
      ondasPortal = [        Onda {          inimigosOnda = [],
          cicloOnda = 2.0,
          tempoOnda = 0.0,
          entradaOnda = 9.0
        }
      ]
    },
    Portal {
      posicaoPortal = (1.5, 2.5),
      ondasPortal = [        Onda {          inimigosOnda = [],
          cicloOnda = 2.0,
          tempoOnda = 6.0,
          entradaOnda = 9.0
        }
      ]
    }
  ],
  [    Inimigo {      posicaoInimigo = (0.5, 0.5),      direcaoInimigo = Oeste,      vidaInimigo = 48.0,      velocidadeInimigo = 1.0,      ataqueInimigo = 5.0,      butimInimigo = 10,      projeteisInimigo = [        Projetil {          tipoProjetil = Gelo,          duracaoProjetil = Finita 3.0        },        Projetil {          tipoProjetil = Resina,          duracaoProjetil = Infinita        }      ],
      visivelInimigo = True
    },
    Inimigo {
      posicaoInimigo = (3.5, 2.5),
      direcaoInimigo = Oeste,
      vidaInimigo = 8.0,
      velocidadeInimigo = 1.0,
      ataqueInimigo = 5.0,
      butimInimigo = 20,
      projeteisInimigo = [        Projetil {          tipoProjetil = Fogo,          duracaoProjetil = Finita 3.0        }      ],
      visivelInimigo = True
    }
  ]
 )



-- | Testes da função atualizaPortais
tAtualizaPortais :: Test 
tAtualizaPortais = test ["Teste 1 da função atualizaPortais" ~: resultadoTeste7 ~=? atualizaPortais 1 1 portalTeste7 inimigoTeste7,
                        "Teste 2 da função atualizaPortais" ~: resultadoTeste7' ~=? atualizaPortais 1 1 portalTeste7' inimigoTeste7]



-- | Testes de torre
torreTeste8 :: Maybe Torre
torreTeste8 = 
  Just $ Torre
      { posicaoTorre = (2.5, 1.5),
        danoTorre = 2.5,
        alcanceTorre = 3.0,
        rajadaTorre = 1,
        cicloTorre = 1.5,
        tempoTorre = 0,
        projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},
        nivelTorre = 1,
        tipoEvolucao = Nada
      }

-- | Testes de posicao
posicaoTeste8 :: Posicao 
posicaoTeste8 =  (-tileSize*2+1, - tileSize*2+1)

-- | Testes de mapa
mapaTeste8 :: Mapa
mapaTeste8 =
    [ [Terra, Terra, Agua,Agua],
      [Relva, Terra, Relva, Terra],
      [Terra, Terra, Agua, Terra],
      [Relva, Terra, Terra, Terra]  
    ]

-- | Testes de torres
torresTeste8 :: [Torre]
torresTeste8 = []

-- | Testes de resultado
resultadoTeste8 :: (Maybe Torre, [Torre])
resultadoTeste8 = (Nothing, [Torre {
    posicaoTorre = (0.5, 3.5),
    danoTorre = 2.5,
    alcanceTorre = 3.0,
    rajadaTorre = 1,
    cicloTorre = 1.5,
    tempoTorre = 0.0,
    projetilTorre = Projetil {
        tipoProjetil = Fogo,
        duracaoProjetil = Finita 5.0
    },
    nivelTorre = 1,
    tipoEvolucao = Nada
}])

-- | Teste da função colocaTorre
tColocaTorre :: Test
tColocaTorre =test ["Teste 1 da função colocaTorre" ~:  resultadoTeste8 ~=? colocaTorre False torreTeste8 posicaoTeste8 mapaTeste8 torresTeste8,
                    "Teste 2 da função colocaTorre" ~:  (torreTeste8, []) ~=? colocaTorre False torreTeste8 (10000,10000) mapaTeste8 torresTeste8]

-- | Teste de torre
tTransformaGlossMat :: Test
tTransformaGlossMat =test ["Teste 1 da função transformaGlossMat" ~: Just (2.5,1.5) ~=? transformaGlossMat False  (tileSize-1,tileSize-1) mapaTeste8,
                           "Teste 2 da função transformaGlossMat" ~: Just (0.5,1.5) ~=? transformaGlossMat True (-tileSize/2+1,tileSize/2-1) mapaTeste8,
                           "Teste 3 da função transformaGlossMat" ~: Nothing ~=? transformaGlossMat False (10000, 1000000) mapaTeste8 ]

-- | Exemplo de torre selecionada
resultadoTesteSelecionaTorre :: (Maybe Torre, Base) 
resultadoTesteSelecionaTorre = (
    Just (Torre {
        posicaoTorre = (-1080.0, -170.0),
        danoTorre = 2.0,
        alcanceTorre = 2.5,
        rajadaTorre = 2,
        cicloTorre = 1.0,
        tempoTorre = 0.0,
        projetilTorre = Projetil {
            tipoProjetil = Resina,
            duracaoProjetil = Infinita
        },
        nivelTorre = 1,
        tipoEvolucao = Nada
    }),
    Base {
        vidaBase = 100.0,
        posicaoBase = (3.5, 1.5),
        creditosBase = 35,
        pregosBase = 2
    }
  )
  -- | Testes da função selecionaTorre
tSelecionaTorre :: Test
tSelecionaTorre =test ["Teste 1 da função selecionaTorre" ~: resultadoTesteSelecionaTorre ~=? selecionaTorre (posx,posyResina) lojaTeste baseTeste6 ,
                       "Teste 2 da função selecionaTorre" ~:  (Nothing, baseTeste6) ~=? selecionaTorre (500,230) lojaTeste baseTeste6]


-- | Testes da função atualizaPosicaoTorre
resultadoTesteAtualizaPosicaoTorre:: Maybe Torre 
resultadoTesteAtualizaPosicaoTorre = Just (Torre {
    posicaoTorre = (200.0, -400.0),
    danoTorre = 2.5,
    alcanceTorre = 3.0,
    rajadaTorre = 1,
    cicloTorre = 1.5,
    tempoTorre = 0.0,
    projetilTorre = Projetil {
        tipoProjetil = Fogo,
        duracaoProjetil = Finita 5.0
    },
    nivelTorre = 1,
    tipoEvolucao = Nada
})

-- | Testes da função atualizaPosicaoTorre
tAtualizaPosicaoTorre :: Test
tAtualizaPosicaoTorre = test ["Teste 1 da função atualizaPosicaoTorre" ~: resultadoTesteAtualizaPosicaoTorre ~=? atualizaPosicaoTorre torreTeste8 (200,-400),
                              "Teste 2 da função atualizaPosicaoTorre" ~: Nothing ~=? atualizaPosicaoTorre Nothing (200,-400)]

-- | Testes da função repete
tRepete ::Test 
tRepete = "Teste da função repete" ~: [1,1,1,1,1] ~=? repete 5 1

-- | Testes da função daN
tDaN :: Test
tDaN = "Teste da função daN" ~: 2 ~=? daN 7 [1,2,3]

-- | Testes da função torres
torreTeste9 :: Torre
torreTeste9 =
  Torre
      { posicaoTorre = (2.5, 1.5),
        danoTorre = 10,
        alcanceTorre = 3.0,
        rajadaTorre = 2,
        cicloTorre = 1.5,
        tempoTorre = 0,
        projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},
        nivelTorre = 1,
        tipoEvolucao = Ciclo 
      }
    
-- | Testes da função torres
torreTeste9' :: Torre
torreTeste9' =
  Torre
      { posicaoTorre = (2.5, 1.5),
        danoTorre = 10,
        alcanceTorre = 3.0,
        rajadaTorre = 2,
        cicloTorre = 1.5,
        tempoTorre = 0,
        projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},
        nivelTorre = 1,
        tipoEvolucao = Rajada 
      }
    -- | Exemplo de evolução torres
resultadoTesteEvoluiTorre :: (Base,[Torre])
resultadoTesteEvoluiTorre = (
    Base {
        vidaBase = 100.0,
        posicaoBase = (3.5, 1.5),
        creditosBase = 25,
        pregosBase = 2
    },
    [
        Torre {
            posicaoTorre = (2.5, 1.5),
            danoTorre = 10.0,
            alcanceTorre = 3.0,
            rajadaTorre = 2,
            cicloTorre = 1.4,
            tempoTorre = 0.0,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 5.0
            },
            nivelTorre = 2,
            tipoEvolucao = Ciclo
        }
    ]
 )

    -- | Testes de evolução torres
resultadoTesteEvoluiTorre' :: (Base,[Torre])
resultadoTesteEvoluiTorre' = 
  (
  Base {
    vidaBase = 100.0,
    posicaoBase = (3.5, 1.5),
    creditosBase = 20,
    pregosBase = 2
  },
  [    Torre {      posicaoTorre = (2.5, 1.5),      danoTorre = 10.0,      alcanceTorre = 3.0,      rajadaTorre = 3,      cicloTorre = 1.5,      tempoTorre = 0.0,      projetilTorre = Projetil {        tipoProjetil = Fogo,        duracaoProjetil = Finita 5.0      },      nivelTorre = 2,      tipoEvolucao = Rajada    }  ] 
 )



-- | Testes da função evoluiTorre
tEvoluiTorre :: Test
tEvoluiTorre = test ["Teste 1 da função evoluiTorre" ~: resultadoTesteEvoluiTorre ~=? evoluiTorre torreTeste9 baseTeste6 [torreTeste9],
                    "Teste 2 da função evoluiTorre" ~: resultadoTesteEvoluiTorre' ~=? evoluiTorre torreTeste9' baseTeste6 [torreTeste9']]

-- | Testes de vender torres
resultadoTesteVenderTorre :: (Base,[Torre])
resultadoTesteVenderTorre = (
    Base {
        vidaBase = 100.0,
        posicaoBase = (3.5, 1.5),
        creditosBase = 80,
        pregosBase = 2
    },
    []
  )

-- | Testes da função venderTorre
tVenderTorre :: Test
tVenderTorre = "Teste da função venderTorre" ~: resultadoTesteVenderTorre ~=? venderTorre torreTeste9 baseTeste6 [torreTeste9]

-- | Testes de inimigos
inimigoTeste10 :: [Inimigo]
inimigoTeste10 =
  [ Inimigo
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 48,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 10,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3},
                            Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (3.5, 2.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }
  ]

-- | Testes de pregos
pregosTeste10 :: [Prego]
pregosTeste10  = [Prego (1.5, 1.5) 30, Prego (0.5, 2.5) 10]


-- | Testes de resultado
resultadoTesteAtualizaPregosInimigos :: ([Inimigo],[Prego])
resultadoTesteAtualizaPregosInimigos = 
  (
    [ Inimigo {
        posicaoInimigo = (3.5, 2.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8.0,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5.0,
        butimInimigo = 20,
        projeteisInimigo = [
            Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 3.0
            }
        ],
        visivelInimigo = True
    }],
    [ Prego {
        posicaoPrego = (1.5, 1.5),
        vidaPrego = 25.0
    },
    Prego {
        posicaoPrego = (0.5, 2.5),
        vidaPrego = 10.0
    }]
 )

-- | Testes da função atualizaPregosInimigos
tAtualizaPregosInimigos :: Test
tAtualizaPregosInimigos = test ["Teste 1 da função atualizaPregosInimigos" ~: resultadoTesteAtualizaPregosInimigos ~=? atualizaPregosInimigos inimigoTeste10 pregosTeste10,
                                "Teste 2 da função atualizaPregosInimigos" ~: ([], pregosTeste10) ~=? atualizaPregosInimigos [] pregosTeste10]

-- | Testes de pregos
pregosTeste11 :: [Prego]
pregosTeste11  = [Prego (1.5, 1.5) 25, Prego (0.5, 2.5) 0]

-- | Testes de remove pregos
tRemovePregosMortos :: Test
tRemovePregosMortos = "Teste da função removePregosMortos" ~: [Prego (1.5, 1.5) 25] ~=? removePregosMortos pregosTeste11

-- | Testes se prego selecionado
resultadoSelecionaPrego :: (Maybe Prego, Base )
resultadoSelecionaPrego = 
  (
    Just (Prego {
        posicaoPrego = (-1080.0, -510.0),
        vidaPrego = 20.0
    }),
    Base {
        vidaBase = 100.0,
        posicaoBase = (3.5, 1.5),
        creditosBase = 50,
        pregosBase = 1
    }
 )

-- | Testes da função selecionaPregos
tSelecionaPregos :: Test
tSelecionaPregos = test ["Teste 1 da função selecionaPregos" ~: resultadoSelecionaPrego ~=? selecionaPrego baseTeste (posx,posyPrego),
                         "Teste 2 da função selecionaPregos" ~: (Nothing, baseTeste {pregosBase = 0}) ~=? selecionaPrego (baseTeste {pregosBase = 0}) (posx,posyPrego)]

-- | Testes de base
baseTeste11 :: Base
baseTeste11 = Base
  { vidaBase = 100,
    posicaoBase = (3.5, 1.5),
    creditosBase = 50, 
    pregosBase = 2
  }

-- | Testes de portais
portalTeste11 :: [Portal] 
portalTeste11 =
  [ 
    Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal =
          [ Onda
              { inimigosOnda =
                  [ Inimigo
                      { posicaoInimigo = (0.5, 0.5),
                        direcaoInimigo = Este,
                        vidaInimigo = 10,
                        velocidadeInimigo = 1.0,
                        ataqueInimigo = 5,
                        butimInimigo = 10,
                        projeteisInimigo = [],
                        visivelInimigo = True
                      }
                  ],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 0
              }
          ]
      },
    Portal
      { posicaoPortal = (1.5, 2.5),
        ondasPortal =
          [ Onda
              { inimigosOnda =
                  [ Inimigo
                      { posicaoInimigo = (0.5, 2.5),
                        direcaoInimigo = Este,
                        vidaInimigo = 10,
                        velocidadeInimigo = 1.0,
                        ataqueInimigo = 5,
                        butimInimigo = 10,
                        projeteisInimigo = [],
                        visivelInimigo = True
                      }
                  ],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 10
              }
          ]
      }
  ]

-- | Testes de mapa
mapaTeste11 :: Mapa
mapaTeste11 =
    [ [Terra, Terra, Agua,Agua],
      [Relva, Terra, Relva, Terra],
      [Terra, Terra, Agua, Terra],
      [Relva, Terra, Terra, Terra]  
    ]
  
  -- | Testes de prego
pregoTeste11 :: Prego 
pregoTeste11 = Prego (tileSize-1,- tileSize -1) 20 

-- | Testes de resultado colocar prego
resultadoTesteColocaPrego :: ([Prego], Maybe Prego)
resultadoTesteColocaPrego  = 
  (
    [ Prego {
        posicaoPrego = (2.5, 3.5),
        vidaPrego = 20.0
    }],
    Nothing
 )

  -- | Testes da função colocaPrego
tColocaPrego :: Test
tColocaPrego =test [ "Teste da função colocaPrego" ~: resultadoTesteColocaPrego ~=? colocaPrego baseTeste11 portalTeste11 False mapaTeste11 pregoTeste11 (tileSize -1,-tileSize-1) [],
                      "Teste da função colocaPrego" ~: ([],Just pregoTeste11) ~=? colocaPrego baseTeste11 portalTeste11 False mapaTeste11 pregoTeste11 (10000,10000) [] ]

-- | Testes de prego
tAtualizaPosicaoPrego :: Test 
tAtualizaPosicaoPrego = test ["Teste da função atualizaPosicaoPrego" ~: Just (Prego (1200,500) 50) ~=? atualizaPosicaoPrego (Just (Prego (1200,500) 50)) (1200,500) ,
                              "Teste da função atualizaPosicaoPrego" ~: Nothing ~=? atualizaPosicaoPrego Nothing (1200,500) ]

-- | Testes de mapa
mapaTeste12 :: Mapa
mapaTeste12 =
    [ [Terra, Cimento, Agua,Agua],
      [Relva, Cimento, Relva, Terra],
      [Terra, Terra, Agua, Terra],
      [Relva, Terra, Terra, Terra]  
    ]

-- | Testes de inimigos
inimigoTeste12 :: [Inimigo]
inimigoTeste12 =
  [ Inimigo
      { posicaoInimigo = (1.5, 0.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 48,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 10,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3},
                            Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
      },
      Inimigo
      { posicaoInimigo = (3.5, 3.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }
  ]

  -- | Testes de resultado
resultadoTesteAlteraVisibilidade :: [Inimigo]
resultadoTesteAlteraVisibilidade = 
  [
    Inimigo {
        posicaoInimigo = (1.5, 0.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 48.0,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5.0,
        butimInimigo = 10,
        projeteisInimigo = [
            Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 3.0
            },
            Projetil {
                tipoProjetil = Resina,
                duracaoProjetil = Infinita
            }
        ],
        visivelInimigo = False
    },
    Inimigo {
        posicaoInimigo = (3.5, 3.5),
        direcaoInimigo = Oeste,
        vidaInimigo = 8.0,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5.0,
        butimInimigo = 20,
        projeteisInimigo = [
            Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 3.0
            }
        ],
        visivelInimigo = True
    }
 ]

-- | Testes da função alteraVisibilidade
tAlteraVisibilidade :: Test
tAlteraVisibilidade = "Teste da função alteraVisibilidade" ~: resultadoTesteAlteraVisibilidade ~=? alteraVisibilidade mapaTeste12 inimigoTeste12

-- | Testes de torre
torreTeste13 :: Torre
torreTeste13  =
   Torre
      { posicaoTorre = (2.5, 1.5),
        danoTorre = 2.5,
        alcanceTorre = 3.0,
        rajadaTorre = 1,
        cicloTorre = 1.5,
        tempoTorre = 0,
        projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},
        nivelTorre = 1, 
        tipoEvolucao = Nada
        
      }

-- | Testes de resultado
resultadoTesteCiraTiros :: [Tiro]
resultadoTesteCiraTiros = 
 [
    ((2.5, 1.5), (1.5, 0.5), (-5.0, -5.0), Fogo),
    ((2.5, 1.5), (3.5, 3.5), (5.0, 10.0), Fogo)
 ]
  
-- | Testes da função criaTiros
tCriaTiros :: Test 
tCriaTiros = "Teste da função criaTiros" ~: resultadoTesteCiraTiros ~=? criaTiros torreTeste13 inimigoTeste12 

-- | Testes de tiros
tirosTeste14 :: [Tiro]
tirosTeste14= 
 [
    ((2.5, 1.5), (2.5, 1.5), (-5.0, -5.0), Fogo),
    ((2.5, 1.5), (3.5, 3.5), (5.0, 10.0), Fogo)
 ]

-- | Testes de resultado
tRemoveTiros :: Test
tRemoveTiros = "Teste da função removeTiros" ~: [((2.5, 1.5), (3.5, 3.5), (5.0, 10.0), Fogo)] ~=? removeTiros tirosTeste14 

-- | Testes de atualiza posicao tiros
resultadoTesteAtualizaPosicaoTiros :: [Tiro]
resultadoTesteAtualizaPosicaoTiros = [
    ((-2.5, -3.5), (2.5, 1.5), (-5.0, -5.0), Fogo),
    ((7.5, 11.5), (3.5, 3.5), (5.0, 10.0), Fogo)
 ]

-- | Testes da função atualizaPosicaoTiros
tAtualizaPosicaoTiros :: Test
tAtualizaPosicaoTiros = "Teste da função atualizaPosicaoTiros" ~:  resultadoTesteAtualizaPosicaoTiros ~=? atualizaPosicaoTiros boostTeste 1 1 tirosTeste14 

-- | Testes de boost
boostTeste15:: Boost 
boostTeste15 = Boost 
  {velocidadeBoost = 1, 
  velocidadeCBoost = 2, 
  velocidadeSBoost = 1, 
  duracaoBoost = 5,
  tempoBoost = 5, 
  numeroBoost = 2, 
  ativoBoost = True
  }

-- | Testes de resultado
resultadoTesteAtualizaBoost :: Boost
resultadoTesteAtualizaBoost =
  Boost {
    velocidadeBoost = 1.0,
    velocidadeCBoost = 2.0,
    velocidadeSBoost = 1.0,
    duracaoBoost = 5.0,
    tempoBoost = 4.0,
    numeroBoost = 2,
    ativoBoost = True
}
  
  -- | Testes da função atualizaBoost
tAtualizaBoost :: Test
tAtualizaBoost = "Teste da função atualizaBoost" ~: resultadoTesteAtualizaBoost ~=? atualizaBoost 1 1 boostTeste15 

-- | Testes da tarefa 3
testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ tAtualizaInimigosTorres,
        tAtualizaInimigos,
        tAtualizaDirecaoInimigos,
        tAtualizaPosicaoInimigos,
        tRetiraInimigosMortos,
        tAtualizaVidaBase,
        tAtualizaPortais,
        tAtualizaJogo,
        tColocaTorre,
        tTransformaGlossMat,
        tSelecionaTorre,
        tAtualizaPosicaoTorre,
        tRepete,
        tDaN,
        tEvoluiTorre,
        tVenderTorre,
        tAtualizaPregosInimigos,
        tRemovePregosMortos,
        tSelecionaPregos,
        tColocaPrego,
        tAtualizaPosicaoPrego,
        tAlteraVisibilidade,
        tCriaTiros,
        tRemoveTiros,
        tAtualizaPosicaoTiros,
        tAtualizaBoost

      ]

