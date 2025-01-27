
{-|
Module      : Tarefa1Spec
Description : Este módulo contém definições em Haskell para a realização de testes automáticos à Tarefa 1 de LI1 em 2024/25.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo que  contém definições em Haskell para a realização de testes automáticos à Tarefa 1 de LI1 em 2024/25.

-}
module Tarefa1Spec (testesTarefa1) where

import Test.HUnit ( Test(TestLabel), (~:), (~=?), Testable(test) )
import LI12425 
import Tarefa1

-- |Base utilizada para testes
baseTeste :: Base
baseTeste = Base
  { vidaBase = 100,
    posicaoBase = (3.5, 1.5),
    creditosBase = 50,
    pregosBase = 2
  }

-- |Portais utilizados para testes
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

-- |Torres utilizadas para testes
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

-- |Primeiro mapa utilizado para testes
mapaTeste :: Mapa
mapaTeste =
    [ [Terra, Terra, Agua,Agua],
      [Relva, Terra, Relva, Terra],
      [Terra, Terra, Agua, Terra],
      [Relva, Cimento, Cimento, Terra]  
    ]

-- |Segundo mapa utilizado para testes
mapaTeste2 :: Mapa
mapaTeste2 =
  [ [Terra, Terra, Agua,Agua],
    [Relva, Terra, Relva, Terra],
    [Terra, Terra, Agua, Terra],
    [Cimento, Agua, Terra, Terra]  
  ]

-- |Inimigos utilizados para testes
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

-- |Primeiro inimigo utilizado para testes
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
      
-- |Segundo inimigo utilizado para testes
inimigoTeste2 :: Inimigo
inimigoTeste2 = Inimigo
      { posicaoInimigo = (1.5, 1.5),
        direcaoInimigo = Sul,
        vidaInimigo = 8,
        velocidadeInimigo = 1.0,
        ataqueInimigo = 5,
        butimInimigo = 20,
        projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},
                            Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}],
        visivelInimigo = True
      }
      
-- |Loja definida para ser utilizada e testar o tipo de dados Jogo 
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


-- |Jogo utilizado para testes
jogoTeste :: Jogo
jogoTeste = Jogo
  { baseJogo = baseTeste,
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

-- | Teste dos pregos
pregosTeste :: [Prego]
pregosTeste = [Prego (1.5, 1.5) 30, Prego (0.5, 2.5) 10]

-- | Teste que verifica se existem portais
tExistePortal :: Test 
tExistePortal = "Testa se há portais" ~: True ~=? existePortal portalTeste

-- | Teste que verifica se um portal esta sobre a terra
tPortalSobreTerra :: Test 
tPortalSobreTerra = "Portais sobre terra" ~: True ~=? portalSobreTerra mapaTeste portalTeste

-- | Teste que verifica se ha caminhos
tVerificaSeHaCaminhos:: Test 
tVerificaSeHaCaminhos = test 
  ["Pelo menos um caminho válido mapa1" ~: True ~=? verificaSeHaCaminhos mapaTeste baseTeste portalTeste,
  " Pelo menos um caminho válido mapa2" ~: False ~=? verificaSeHaCaminhos mapaTeste2 baseTeste portalTeste]

-- | Teste que verifica se um portal nao esta sobreposto
tPortalNaoSobreposto :: Test 
tPortalNaoSobreposto = "Elementos (base,portais,torres) estão sobrepostos " ~: True ~=? portalNaoSobreposto mapaTeste baseTeste portalTeste torreTeste 

-- | Teste que verifica se uma onda está ativa
tApenasUmaOndaAtiva:: Test 
tApenasUmaOndaAtiva =  "Onda ativa por portal" ~: True ~=? apenasUmaOndaAtiva portalTeste

-- | Teste que valida os inimigos
tValidaInimigos:: Test 
tValidaInimigos = "Validade dos inimigos" ~: True ~=? validaInimigos portalTeste

-- | Teste que valida inimigos sobre terra ou cimento
tValidaInimigosSobreTerraOuCimento :: Test 
tValidaInimigosSobreTerraOuCimento = "Inimigos sobre Terra" ~: True ~=? validaInimigosSobreTerraOuCimento mapaTeste inimigoTeste

-- | Teste que verifica se ẽ terra
tETerra :: Test
tETerra = "Testa se é terra" ~: True ~=? eTerra mapaTeste (1.5, 1.5)

-- | Teste que verifica se é cimento
tECimento :: Test
tECimento = "Testa se é cimento" ~: True ~=? eCimento mapaTeste (2.5, 3.5)

-- | Testa se é terra ou cimento
tETerraOuCimento :: Test
tETerraOuCimento = "Testa se é terra ou cimento" ~: False ~=? eTerraOuCimento mapaTeste (0.5, 1.5)

-- | Teste que verifica se torres e inimigos estão sobrepostos
tValidaInimigosSemSobreposicao :: Test 
tValidaInimigosSemSobreposicao = "Torres e inimigos sobrepostos" ~: True ~=? validaInimigosSemSobreposicao inimigoTeste torreTeste

-- | Teste que valida a velocidade dos inimigos 
tValidaVelocidadeInimigos :: Test 
tValidaVelocidadeInimigos = "Validade da velocidade" ~: True ~=? validaVelocidadeInimigos inimigoTeste

-- | Teste que verifica a validade de Projeteis ativos
tValidaProjeteisAtivos :: Test
tValidaProjeteisAtivos = test ["Validade Projeteis inimigo1" ~: True ~=? validaProjeteisInimigo inimigoTeste1 ,
                              "Validade Projeteis inimigo2" ~: False ~=? validaProjeteisInimigo inimigoTeste2] 


-- | Teste que verifica se a torre está sobre terra
tVerificaTorre :: Test 
tVerificaTorre = "Torre sobre terra" ~: True ~=? verificaTorre mapaTeste torreTeste

-- | Teste que verifica a validade do dano e alcance da torre
tAlcanceEDanoPositivo :: Test 
tAlcanceEDanoPositivo = "Validade do dano e alcance torre" ~: True ~=? alcanceEDanoPositivo torreTeste

-- | Teste que verifica a validade da rajada da torre
tRajadaPositiva :: Test 
tRajadaPositiva = "Validade da rajada da torre" ~: True ~=? rajadaPositiva torreTeste

-- | Teste que verifica a validade do ciclo das torres
tCicloNaoNegativo:: Test 
tCicloNaoNegativo = "Validade do ciclo das torres" ~: True ~=? cicloNaoNegativo torreTeste

-- | Teste que verifica a validade da posicao torre
tBaseSobreTerra :: Test
tBaseSobreTerra = "Validade posicao torre" ~: True ~=? baseSobreTerra baseTeste mapaTeste

-- | Teste que verifica a validade dos creditos da base
tCreditosNaoNegativos:: Test  
tCreditosNaoNegativos = "Validade creditos da base" ~: True ~=? creditosNaoNegativos baseTeste

-- | Teste que verifica a validade do jogo
tValidaJogo :: Test 
tValidaJogo = "Validade do jogo" ~: True ~=? validaJogo jogoTeste

-- | Teste que verifica a validade dos pregos
tValidaPregos :: Test 
tValidaPregos = "Validade dos pregos" ~: True ~=? validaPregos baseTeste portalTeste mapaTeste pregosTeste

-- | Teste que verifica a validade do boost
tValidaBoost :: Test
tValidaBoost = "Validade do boost" ~: True ~=? validaBoost boostTeste

-- | Testes da tarefa 1
testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ tExistePortal,
        tPortalSobreTerra, 
        tVerificaSeHaCaminhos, 
        tPortalNaoSobreposto,
        tApenasUmaOndaAtiva,
        tValidaInimigos,
        tValidaInimigosSobreTerraOuCimento,
        tETerra,
        tECimento, 
        tETerraOuCimento,
        tValidaInimigosSemSobreposicao,
        tValidaVelocidadeInimigos,
        tValidaProjeteisAtivos,
        tVerificaTorre,
        tAlcanceEDanoPositivo,
        tRajadaPositiva,
        tCicloNaoNegativo,
        tBaseSobreTerra,
        tCreditosNaoNegativos, 
        tValidaJogo,
        tValidaPregos,
        tValidaBoost
      ]


    

