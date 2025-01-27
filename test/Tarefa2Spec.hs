{-|
Module      : Tarefa2Spec
Description : Este módulo contém definições Haskell para a realização da Tarefa 2 de LI1 em 2024/25.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo que ontém definições Haskell para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2Spec (testesTarefa2) where

import Test.HUnit
import LI12425
import Tarefa2 



-- | Testes portal
portalTeste1 :: Portal
portalTeste1 = Portal
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
                entradaOnda = 10
              }
          ]
      }

-- | Testes portal
portalTeste3 :: Portal
portalTeste3 = Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal = []
      }
    
-- | Testes portal
portalTeste2 :: Portal
portalTeste2 = Portal
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
      }

-- | Testes torre
torreTeste1 :: Torre 
torreTeste1 = Torre
      { posicaoTorre = (2.5, 1.5),
        danoTorre = 2.5,
        alcanceTorre = 1.0,
        rajadaTorre = 1,
        cicloTorre = 1.5,
        tempoTorre = 3,
        projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},
        nivelTorre = 1, 
        tipoEvolucao = Nada
      }
    
-- | Testes torre
torreTeste2 :: Torre 
torreTeste2 = Torre
      { posicaoTorre = (2.5, 1.5),
        danoTorre = 2.5,
        alcanceTorre = 1.0,
        rajadaTorre = 1,
        cicloTorre = 1.5,
        tempoTorre = 3,
        projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 5.0},
        nivelTorre = 1, 
        tipoEvolucao = Nada
      }

-- | Exemplo torre
torreTeste3 :: Torre 
torreTeste3 = Torre
      { posicaoTorre = (2.5, 1.5),
        danoTorre = 2.5,
        alcanceTorre = 1.0,
        rajadaTorre = 1,
        cicloTorre = 1.5,
        tempoTorre = 3,
        projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 5.0},
        nivelTorre = 1, 
        tipoEvolucao = Nada
      }



-- | Testes inimigo
inimigoTesteLista1 :: [Inimigo]
inimigoTesteLista1 =
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
      },
    Inimigo
      { posicaoInimigo = (1.5, 3.5),
        direcaoInimigo = Sul,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
                            Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }
  ]

-- | Testes inimigo
inimigoTesteLista2 :: [Inimigo]
inimigoTesteLista2 =
  [ Inimigo
      { posicaoInimigo = (0.5, 0.5),
        direcaoInimigo = Este,
        vidaInimigo = 10,                    
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 10,
        projeteisInimigo = [],
        visivelInimigo = True
      },
            Inimigo
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
                            Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
      },
    Inimigo
      { posicaoInimigo = (1.5, 3.5),
        direcaoInimigo = Sul,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
                            Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }
  ]

-- | Testes inimigo
inimigoTeste1 :: Inimigo
inimigoTeste1 = Inimigo
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

-- | Testes inimigo
inimigoTeste2 :: Inimigo 
inimigoTeste2 = Inimigo 
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 5.5,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}],
        visivelInimigo = True
      }

-- | Testes inimigo 
inimigoTeste3 :: Inimigo 
inimigoTeste3 = Inimigo 
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 5.5,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 8.0},
                          Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita }],
        visivelInimigo = True
      }
      
-- | Testes inimigo
inimigoTeste4 :: Inimigo 
inimigoTeste4 = Inimigo 
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 3,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil =Finita 10}],
        visivelInimigo = True
      }

-- | Testes inimigo
inimigoTeste5 :: Inimigo 
inimigoTeste5 = Inimigo 
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 3,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil =Finita 10}],
        visivelInimigo = False
      }
      

-- | Testes base
baseTeste :: Base
baseTeste = Base
  { vidaBase = 1,
    posicaoBase = (3.5, 1.5),
    creditosBase = 50,
    pregosBase = 2
  }

-- | Exemplo portais
portalTeste :: [Portal]
portalTeste =
  [ 
    Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal =
          [ Onda
              { inimigosOnda =
                  [],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 0
              }
          ]
      }
  ]

-- | Exemplo portais
portalTeste4 :: Portal
portalTeste4 =
 
    Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal = []
      }
  

-- | Exemplo portais
portalTeste5 :: Portal
portalTeste5 =
  
    Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal = [
          Onda
              { inimigosOnda =
                  [],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 3
              }
        ]
      }

  


-- | Exemplo portais
portalTeste6 :: Portal
portalTeste6 =
  
    Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal = [
          Onda
              { inimigosOnda =
                  [inimigoTeste5],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 3
              }
        ]
      }
  

-- | Exemplo portais
portalTeste7 :: Portal
portalTeste7 =
   
    Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal = [
          Onda
              { inimigosOnda =
                  [inimigoTeste5],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 0
              }
        ]
      }
  

-- | Exemplo portais
portalTeste8 :: Portal
portalTeste8 =
   
    Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal = [
          Onda
              { inimigosOnda =
                  [inimigoTeste5,inimigoTeste4],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 3
              },
          Onda
              { inimigosOnda =
                  [inimigoTeste5],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 3
              }
        ]
      }
  

-- | Exemplo portais
portalTeste9 :: Portal
portalTeste9 =
  
    Portal
      { posicaoPortal = (0.5, 0.5),
        ondasPortal = [
          Onda
              { inimigosOnda =
                  [inimigoTeste5, inimigoTeste4],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 0
              },
          Onda
              { inimigosOnda =
                  [inimigoTeste5],
                cicloOnda = 2,
                tempoOnda = 0,
                entradaOnda = 3
              }
        ]
      }
  

-- | Testes torres
torreTeste :: [Torre]
torreTeste =
  [ Torre
      { posicaoTorre = (2.5, 1.5),
        danoTorre = 2.5,
        alcanceTorre = 3.0,
        rajadaTorre = 1,
        cicloTorre = 1.5,
        tempoTorre = 3,
        projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},
        nivelTorre = 1,
        tipoEvolucao = Nada
      }
  ]

-- | Testes mapa
mapaTeste :: Mapa
mapaTeste =
  [ [Terra, Terra, Agua,Agua],
    [Relva, Terra, Relva, Terra],
    [Terra, Terra, Agua, Terra],
    [Relva, Terra, Terra, Terra]  
  ]

-- | Testes inimigo
inimigoTeste :: [Inimigo]
inimigoTeste = []

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

-- | Testes loja
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

-- | Testes jogo
jogoTeste :: Jogo
jogoTeste = Jogo
  { baseJogo = baseTeste,
    portaisJogo = portalTeste,
    torresJogo = torreTeste,
    mapaJogo = mapaTeste,
    inimigosJogo = inimigoTeste,
    lojaJogo = lojaTeste,
    tirosJogo = [] ,
    torreSelecionada = Nothing,
    posicaoRato = (0,0),
    velocidadeJogo =1, 
    terceiraDimensao = False, 
    torreAMelhorar = Nothing, 
    pregosJogo = [], 
    pregoSelecionado = Nothing, 
    boostJogo = boostTeste
    
  }

-- | Testes base
baseTesteP :: Base
baseTesteP = Base
  { vidaBase = -1,
    posicaoBase = (3.5, 1.5),
    creditosBase = 50,
    pregosBase = 2
  }

-- | Testes jogo
jogoTesteP :: Jogo
jogoTesteP = Jogo
  { baseJogo = baseTesteP,
    portaisJogo = portalTeste,
    torresJogo = torreTeste,
    mapaJogo = mapaTeste,
    inimigosJogo = inimigoTeste,
    lojaJogo = lojaTeste,
    tirosJogo = [],
    torreSelecionada = Nothing,
    posicaoRato = (0,0),
    velocidadeJogo =1, 
    terceiraDimensao = False, 
    torreAMelhorar = Nothing, 
    pregosJogo = [], 
    pregoSelecionado = Nothing, 
    boostJogo = boostTeste
  }

-- |Inimigo de Teste
inimigoTeste6 :: Inimigo 
inimigoTeste6 = 
  Inimigo {
  posicaoInimigo = (1.5, 1.5),
  direcaoInimigo = Sul,
  vidaInimigo = 3.0,
  velocidadeInimigo = 1.0,
  ataqueInimigo = 5.0,
  butimInimigo = 20,
  projeteisInimigo = [    Projetil {      tipoProjetil = Gelo,      duracaoProjetil = Finita 5.0    },    Projetil {      tipoProjetil = Resina,      duracaoProjetil = Infinita    }  ],
  visivelInimigo = True
 }


-- |Inimigo de Teste
inimigoTeste7 :: Inimigo 
inimigoTeste7 =
 Inimigo {
  posicaoInimigo = (1.5, 1.5),
  direcaoInimigo = Sul,
  vidaInimigo = 0.5,
  velocidadeInimigo = 1.0,
  ataqueInimigo = 5.0,
  butimInimigo = 20,
  projeteisInimigo = [    Projetil {      tipoProjetil = Fogo,      duracaoProjetil = Finita 20.0    }  ],
  visivelInimigo = True
 }

-- |Inimigo de Teste
inimigoTeste8 :: Inimigo 
inimigoTeste8 =
  Inimigo {
  posicaoInimigo = (1.5, 1.5),
  direcaoInimigo = Sul,
  vidaInimigo = 0.5,
  velocidadeInimigo = 1.0,
  ataqueInimigo = 5.0,
  butimInimigo = 20,
  projeteisInimigo = [Projetil {tipoProjetil = Fogo,      duracaoProjetil = Finita 15.0    }  ],
  visivelInimigo = True
 }

-- |Inimigo de Teste
inimigoTeste9 :: Inimigo 
inimigoTeste9 =
 Inimigo {
  posicaoInimigo = (1.5, 1.5),
  direcaoInimigo = Sul,
  vidaInimigo = 0.5,
  velocidadeInimigo = 1.0,
  ataqueInimigo = 5.0,
  butimInimigo = 20,
  projeteisInimigo = [],
  visivelInimigo = True
 }

-- |Inimigo de Teste
inimigoTeste10 :: Inimigo 
inimigoTeste10 =
  Inimigo {
  posicaoInimigo = (1.5, 1.5),
  direcaoInimigo = Sul,
  vidaInimigo = 3.0,
  velocidadeInimigo = 1.0,
  ataqueInimigo = 5.0,
  butimInimigo = 20,
  projeteisInimigo = [    Projetil {      tipoProjetil = Resina,      duracaoProjetil = Infinita    }  ],
  visivelInimigo = True
}


-- |Inimigo de Teste
inimigoTeste11 :: Inimigo 
inimigoTeste11 =
  Inimigo {
  posicaoInimigo = (1.5, 1.5),
  direcaoInimigo = Sul,
  vidaInimigo = 5.5,
  velocidadeInimigo = 1.0,
  ataqueInimigo = 5.0,
  butimInimigo = 20,
  projeteisInimigo = [    Projetil {      tipoProjetil = Gelo,      duracaoProjetil = Finita 3.0    },    Projetil {      tipoProjetil = Resina,      duracaoProjetil = Infinita    }  ],
  visivelInimigo = True
 }

-- |Resultado do teste 2 da função ativaInimigo
resultadoT2 :: (Portal, [Inimigo])
resultadoT2  =
  ( Portal
  { posicaoPortal = (0.5, 0.5)
  , ondasPortal =
      [ Onda
          { inimigosOnda =
              [ Inimigo
                  { posicaoInimigo = (0.5, 0.5)
                  , direcaoInimigo = Este
                  , vidaInimigo = 10.0
                  , velocidadeInimigo = 1.0
                  , ataqueInimigo = 5.0
                  , butimInimigo = 10
                  , projeteisInimigo = []
                  , visivelInimigo = True
                  }
              ]
          , cicloOnda = 2.0
          , tempoOnda = 0.0
          , entradaOnda = 10.0
          }
      ]
  }
 , [ Inimigo
      { posicaoInimigo = (1.5, 1.5)
      , direcaoInimigo = Sul
      , vidaInimigo = 8.0
      , velocidadeInimigo = 1.0
      , ataqueInimigo = 5.0
      , butimInimigo = 20
      , projeteisInimigo =
          [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 }
          , Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita }
          ]
      , visivelInimigo = True
      }
  , Inimigo
      { posicaoInimigo = (1.5, 3.5)
      , direcaoInimigo = Sul
      , vidaInimigo = 8.0
      , velocidadeInimigo = 1.0
      , ataqueInimigo = 5.0
      , butimInimigo = 20
      , projeteisInimigo =
          [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 }
          , Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 3.0 }
          ]
      , visivelInimigo = True
      }
  ]
 )


-- |Resultado do teste 3 da função ativaInimigo
resultadoT3 :: (Portal, [Inimigo])
resultadoT3  =
  ( Portal { posicaoPortal = (0.5, 0.5), ondasPortal = [] }
 , [ Inimigo
      { posicaoInimigo = (1.5, 1.5)
      , direcaoInimigo = Sul
      , vidaInimigo = 8.0
      , velocidadeInimigo = 1.0
      , ataqueInimigo = 5.0
      , butimInimigo = 20
      , projeteisInimigo =
          [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 }
          , Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita }
          ]
      , visivelInimigo = True
      }
  , Inimigo
      { posicaoInimigo = (1.5, 3.5)
      , direcaoInimigo = Sul
      , vidaInimigo = 8.0
      , velocidadeInimigo = 1.0
      , ataqueInimigo = 5.0
      , butimInimigo = 20
      , projeteisInimigo =
          [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 }
          , Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 3.0 }
          ]
      , visivelInimigo = True
      }
  ]
 )

-- |Resultado do teste 4 da função ativaInimigo
resultadoT4 :: (Portal, [Inimigo])
resultadoT4  =
  ( Portal
  { posicaoPortal = (0.5, 0.5)
  , ondasPortal =
      [ Onda
          { inimigosOnda = []
          , cicloOnda = 2.0
          , tempoOnda = 0.0
          , entradaOnda = 3.0
          }
      ]
  }
 , [ Inimigo
      { posicaoInimigo = (1.5, 1.5)
      , direcaoInimigo = Sul
      , vidaInimigo = 8.0
      , velocidadeInimigo = 1.0
      , ataqueInimigo = 5.0
      , butimInimigo = 20
      , projeteisInimigo =
          [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 }
          , Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita }
          ]
      , visivelInimigo = True
      }
  , Inimigo
      { posicaoInimigo = (1.5, 3.5)
      , direcaoInimigo = Sul
      , vidaInimigo = 8.0
      , velocidadeInimigo = 1.0
      , ataqueInimigo = 5.0
      , butimInimigo = 20
      , projeteisInimigo =
          [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 }
          , Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 3.0 }
          ]
      , visivelInimigo = True
      }
  ]
 )

-- |Resultado do teste 5 da função ativaInimigo
resultadoT5 :: (Portal, [Inimigo])
resultadoT5  =
 ( Portal 
  { posicaoPortal = (0.5, 0.5)
  , ondasPortal = 
      [ Onda 
          { inimigosOnda = 
              [ Inimigo 
                  { posicaoInimigo = (1.5, 1.5)
                  , direcaoInimigo = Sul
                  , vidaInimigo = 3.0
                  , velocidadeInimigo = 1.0
                  , ataqueInimigo = 5.0
                  , butimInimigo = 20
                  , projeteisInimigo = 
                      [ Projetil 
                          { tipoProjetil = Fogo
                          , duracaoProjetil = Finita 10.0 
                          }
                      ]
                  , visivelInimigo = False
                  }
              ]
          , cicloOnda = 2.0
          , tempoOnda = 0.0
          , entradaOnda = 3.0
          }
      ]
  }
 , [ Inimigo 
      { posicaoInimigo = (1.5, 1.5)
      , direcaoInimigo = Sul
      , vidaInimigo = 8.0
      , velocidadeInimigo = 1.0
      , ataqueInimigo = 5.0
      , butimInimigo = 20
      , projeteisInimigo = 
          [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 }
          , Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita }
          ]
      , visivelInimigo = True
      }
  , Inimigo 
      { posicaoInimigo = (1.5, 3.5)
      , direcaoInimigo = Sul
      , vidaInimigo = 8.0
      , velocidadeInimigo = 1.0
      , ataqueInimigo = 5.0
      , butimInimigo = 20
      , projeteisInimigo = 
          [ Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 3.0 }
          , Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 3.0 }
          ]
      , visivelInimigo = True
      }
  ]
 )

-- |Resultado do teste 6 da função ativaInimigo
resultadoT6 :: (Portal, [Inimigo])
resultadoT6  =
 (Portal
 {
  posicaoPortal = (0.5, 0.5),
  ondasPortal = []
 }
 ,
 [Inimigo  {    posicaoInimigo = (1.5, 1.5),    direcaoInimigo = Sul,    vidaInimigo = 3.0,    velocidadeInimigo = 1.0,    ataqueInimigo = 5.0,    butimInimigo = 20,    projeteisInimigo =    [      Projetil      {        tipoProjetil = Fogo,        duracaoProjetil = Finita 10.0      }    ],
    visivelInimigo = False
  }
 , Inimigo
  {
    posicaoInimigo = (1.5, 1.5),
    direcaoInimigo = Sul,
    vidaInimigo = 8.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 5.0,
    butimInimigo = 20,
    projeteisInimigo =
    [      Projetil      {        tipoProjetil = Gelo,        duracaoProjetil = Finita 3.0      }      , Projetil      {        tipoProjetil = Resina,        duracaoProjetil = Infinita      }    ],
    visivelInimigo = True
  }
 , Inimigo
  {
    posicaoInimigo = (1.5, 3.5),
    direcaoInimigo = Sul,
    vidaInimigo = 8.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 5.0,
    butimInimigo = 20,
    projeteisInimigo =
    [      Projetil      {        tipoProjetil = Gelo,        duracaoProjetil = Finita 3.0      }      , Projetil      {        tipoProjetil = Fogo,        duracaoProjetil = Finita 3.0      }    ],
    visivelInimigo = True
  }
 ])

-- |Resultado do teste 7 da função ativaInimigo
resultadoT7 :: (Portal, [Inimigo])
resultadoT7  =
 (Portal
 {
  posicaoPortal = (0.5, 0.5),
  ondasPortal =
  [    Onda    {      inimigosOnda =      [        Inimigo        {          posicaoInimigo = (1.5, 1.5),          direcaoInimigo = Sul,          vidaInimigo = 3.0,          velocidadeInimigo = 1.0,          ataqueInimigo = 5.0,          butimInimigo = 20,          projeteisInimigo =          [            Projetil            {              tipoProjetil = Fogo,              duracaoProjetil = Finita 10.0            }          ],
          visivelInimigo = False
        }
        , Inimigo
        {
          posicaoInimigo = (1.5, 1.5),
          direcaoInimigo = Sul,
          vidaInimigo = 3.0,
          velocidadeInimigo = 1.0,
          ataqueInimigo = 5.0,
          butimInimigo = 20,
          projeteisInimigo =
          [            Projetil            {              tipoProjetil = Fogo,              duracaoProjetil = Finita 10.0            }          ],
          visivelInimigo = True
        }
      ],
      cicloOnda = 2.0,
      tempoOnda = 0.0,
      entradaOnda = 3.0
    }
    , Onda
    {
      inimigosOnda =
      [        Inimigo        {          posicaoInimigo = (1.5, 1.5),          direcaoInimigo = Sul,          vidaInimigo = 3.0,          velocidadeInimigo = 1.0,          ataqueInimigo = 5.0,          butimInimigo = 20,          projeteisInimigo =          [            Projetil            {              tipoProjetil = Fogo,              duracaoProjetil = Finita 10.0            }          ],
          visivelInimigo = False
        }
      ],
      cicloOnda = 2.0,
      tempoOnda = 0.0,
      entradaOnda = 3.0
    }
  ]
 } 
 , [  Inimigo  {    posicaoInimigo = (1.5, 1.5),    direcaoInimigo = Sul,    vidaInimigo = 8.0,    velocidadeInimigo = 1.0,    ataqueInimigo = 5.0,    butimInimigo = 20,    projeteisInimigo =    [      Projetil      {        tipoProjetil = Gelo,        duracaoProjetil = Finita 3.0      }      , Projetil      {        tipoProjetil = Resina,        duracaoProjetil = Infinita      }    ],
    visivelInimigo = True
  }
  , Inimigo
  {
    posicaoInimigo = (1.5, 3.5),
    direcaoInimigo = Sul,
    vidaInimigo = 8.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 5.0,
    butimInimigo = 20,
    projeteisInimigo =
    [      Projetil      {        tipoProjetil = Gelo,        duracaoProjetil = Finita 3.0      }      , Projetil      {        tipoProjetil = Fogo,        duracaoProjetil = Finita 3.0      }    ],
    visivelInimigo = True
  }
 ]
 )


-- |Resultado do teste 8 da função ativaInimigo
resultadoT8 :: (Portal, [Inimigo])
resultadoT8  =
  (Portal
 {
  posicaoPortal = (0.5, 0.5),
  ondasPortal =
  [    Onda    {      inimigosOnda =      [        Inimigo        {          posicaoInimigo = (1.5, 1.5),          direcaoInimigo = Sul,          vidaInimigo = 3.0,          velocidadeInimigo = 1.0,          ataqueInimigo = 5.0,          butimInimigo = 20,          projeteisInimigo =          [            Projetil            {              tipoProjetil = Fogo,              duracaoProjetil = Finita 10.0            }          ],
          visivelInimigo = True
        }
      ],
      cicloOnda = 2.0,
      tempoOnda = 0.0,
      entradaOnda = 2.0
    },
    Onda
    {
      inimigosOnda =
      [        Inimigo        {          posicaoInimigo = (1.5, 1.5),          direcaoInimigo = Sul,          vidaInimigo = 3.0,          velocidadeInimigo = 1.0,          ataqueInimigo = 5.0,          butimInimigo = 20,          projeteisInimigo =          [            Projetil            {              tipoProjetil = Fogo,              duracaoProjetil = Finita 10.0            }          ],
          visivelInimigo = False
        }
      ],
      cicloOnda = 2.0,
      tempoOnda = 0.0,
      entradaOnda = 3.0
    }
  ]
 },
 [  Inimigo  {    posicaoInimigo = (1.5, 1.5),    direcaoInimigo = Sul,    vidaInimigo = 3.0,    velocidadeInimigo = 1.0,    ataqueInimigo = 5.0,    butimInimigo = 20,    projeteisInimigo =    [      Projetil      {        tipoProjetil = Fogo,        duracaoProjetil = Finita 10.0      }    ],
    visivelInimigo = False
  },
  Inimigo
  {
    posicaoInimigo = (1.5, 1.5),
    direcaoInimigo = Sul,
    vidaInimigo = 8.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 5.0,
    butimInimigo = 20,
    projeteisInimigo =
    [      Projetil      {        tipoProjetil = Gelo,        duracaoProjetil = Finita 3.0      },      Projetil      {        tipoProjetil = Resina,        duracaoProjetil = Infinita      }    ],
    visivelInimigo = True
  },
  Inimigo
  {
    posicaoInimigo = (1.5, 3.5),
    direcaoInimigo = Sul,
    vidaInimigo = 8.0,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 5.0,
    butimInimigo = 20,
    projeteisInimigo =
    [      Projetil      {        tipoProjetil = Gelo,        duracaoProjetil = Finita 3.0      },      Projetil      {        tipoProjetil = Fogo,        duracaoProjetil = Finita 3.0      }    ],
    visivelInimigo = True
  } 
 ]
 )




-- | Testes tem tipo
tTemTipo :: Test
tTemTipo = test ["Test1 temTipo" ~: True ~=? temTipo (projeteisInimigo inimigoTeste1) Gelo,
                "Test2 temTipo" ~: False ~=? temTipo (projeteisInimigo inimigoTeste1) Fogo]

-- | Testes inimigos alcance 
tInimigosNoAlcance :: Test
tInimigosNoAlcance =test ["Test1 inimigosNoAlcance" ~: [inimigoTeste1] ~=? inimigosNoAlcance torreTeste1 inimigoTesteLista1,
                          "Test2 inimigosNoAlcance" ~: [] ~=? inimigosNoAlcance torreTeste1 [inimigoTeste5]]

-- | Atinge inimigo
tAtingeInimigo :: Test 
tAtingeInimigo = test ["Test1 atingeInimigo" ~: inimigoTeste2 ~=? atingeInimigo torreTeste1 inimigoTeste1,
                      "Test2 atingeInimigo" ~: inimigoTeste4 ~=? atingeInimigo torreTeste1 inimigoTeste2,
                      "Test3 atingeInimigo" ~: inimigoTeste3 ~=? atingeInimigo torreTeste2 inimigoTeste1,
                      "Test4 atingeInimigo" ~: inimigoTeste6 ~=? atingeInimigo torreTeste2 inimigoTeste2,
                      "Test5 atingeInimigo" ~: inimigoTeste7 ~=? atingeInimigo torreTeste3 inimigoTeste4,
                      "Test6 atingeInimigo" ~: inimigoTeste8 ~=? atingeInimigo torreTeste1 inimigoTeste4,
                      "Test7 atingeInimigo" ~: inimigoTeste9 ~=? atingeInimigo torreTeste2 inimigoTeste4,
                      "Test8 atingeInimigo" ~: inimigoTeste10 ~=? atingeInimigo torreTeste3 inimigoTeste2,
                      "Test9 atingeInimigo" ~: inimigoTeste11 ~=? atingeInimigo torreTeste3 inimigoTeste1]


-- | Testes ativa inimigo
tAtivaInimigo :: Test 
tAtivaInimigo = test ["Test1 ativaInimigo" ~: (portalTeste3, inimigoTesteLista2) ~=? ativaInimigo portalTeste2 inimigoTesteLista1,
                      "Test2 ativaInimigo" ~: resultadoT2 ~=? ativaInimigo portalTeste1 inimigoTesteLista1 ,
                      "Test3 ativaInimigo" ~: resultadoT3 ~=? ativaInimigo portalTeste4 inimigoTesteLista1 ,
                      "Test4 ativaInimigo" ~: resultadoT4 ~=? ativaInimigo portalTeste5 inimigoTesteLista1 ,
                      "Test5 ativaInimigo" ~: resultadoT5 ~=? ativaInimigo portalTeste6 inimigoTesteLista1 ,
                      "Test6 ativaInimigo" ~: resultadoT6 ~=? ativaInimigo portalTeste7 inimigoTesteLista1 ,
                      "Test7 ativaInimigo" ~: resultadoT7 ~=? ativaInimigo portalTeste8 inimigoTesteLista1 ,
                      "Test8 ativaInimigo" ~: resultadoT8 ~=? ativaInimigo portalTeste9 inimigoTesteLista1  ]

-- | Testes ganhou jogo
tGanhouJogo :: Test
tGanhouJogo = test ["Test1 ganhouJogo" ~: True ~=? ganhouJogo jogoTeste,
                "Test2 ganhouJogo" ~: False ~=? ganhouJogo (jogoTeste {portaisJogo = [portalTeste1]})]
              
 

-- | Testes perdeu jogo
tPerdeuJogo :: Test 
tPerdeuJogo = test ["Test1 perdeuJogo" ~: True ~=? perdeuJogo jogoTesteP,
                    "Test2 perdeuJogo" ~: False ~=? perdeuJogo (jogoTesteP {baseJogo = (baseJogo jogoTesteP){vidaBase = 40}})]

-- | Testes terminou jogo
tTerminouJogo :: Test 
tTerminouJogo = test ["Test terminouJogo" ~: True  ~=? terminouJogo jogoTeste,
                        "Test terminouJogo" ~: False  ~=? terminouJogo (jogoTeste {portaisJogo = [portalTeste1]})]
              

-- | Testes tarefa 2
testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [ tInimigosNoAlcance,
        tAtingeInimigo,
        tAtivaInimigo,
        tGanhouJogo, 
        tPerdeuJogo,
        tTerminouJogo,
        tTemTipo  
      ]

