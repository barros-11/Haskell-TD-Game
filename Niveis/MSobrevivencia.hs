{-|
Module      : MSobrevivência
Description : Este módulo contém a definição modo de jogo sobrevivência
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo que contém a definição do modo de sobrevivência 
-}
module MSobrevivencia where 

import LI12425
import Tarefa3
  
-- | Base do modo de sobrevivência
baseN1 :: Base 
baseN1 = Base
      { vidaBase = 100,
        posicaoBase = (10.5, 7.5),
        creditosBase = 55,
        pregosBase = 3 
      }

-- | Portais do modo de sobrevivência 
portaisN1 :: [Portal]
portaisN1 =
      [ Portal
          { posicaoPortal = (1.5, 9.5),
            ondasPortal = 
              criaInfinitas ( Onda
              { inimigosOnda =
                repete 20 (Inimigo
                      { posicaoInimigo = (1.5, 9.5),
                        direcaoInimigo = Este,
                        vidaInimigo = 50,
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
              } ) 20
              
          },
        Portal
          { posicaoPortal = (18.5, 5.5),
            ondasPortal =
              criaInfinitas (Onda
              { inimigosOnda =
                repete 15 (Inimigo
                      { posicaoInimigo = (18.5, 5.5),
                        direcaoInimigo = Oeste,
                        vidaInimigo = 75,
                        velocidadeInimigo = 1,
                        ataqueInimigo = 7,
                        butimInimigo = 5,
                        projeteisInimigo = [],
                        visivelInimigo = True
                      }) 
                ,
                cicloOnda = 2.5,
                tempoOnda = 0,
                entradaOnda = 5.0
              } ) 15
              
          }
      ]

-- | Torres do modo de sobrevivência 
torresN1 :: [Torre]
torresN1 = []

-- | Mapa do  modo de sobrevivência
mapaN1 :: Mapa    
mapaN1 =  
  [
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, t, t, t, t, t, t, t, t, t, t, t, c, c, c, r, r, r],
    [r, r, t, t, t, r, r, r, r, r, r, r, r, r, r, r, r, c, r, r, r],
    [r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, c, r, r, r],
    [r, r, c, r, r, r, r, t, t, t, t, t, t, t, t, r, r, t, r, r, r],
    [r, c, c, r, r, t, t, t, r, r, r, r, r, r, t, r, r, t, t, t, r],
    [r, c, r, r, r, t, r, r, r, r, r, r, r, r, t, r, r, r, r, t, r],
    [r, t, r, r, r, c, r, r, r, r, t, t, r, r, t, r, r, r, r, t, r],
    [r, t, r, r, r, c, r, r, r, r, r, t, r, r, c, r, r, r, r, t, r],
    [r, t, r, r, r, c, r, r, r, r, r, t, r, r, c, c, c, r, r, c, r],
    [r, r, r, r, r, c, c, t, t, t, t, t, r, r, r, r, c, c, c, c, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r]
  ]

      where 
        t = Terra 
        a = Agua 
        r = Relva 
        c = Cimento

-- | Inimigos do modo de sobrevivência
inimigosN1 :: [Inimigo]
inimigosN1 = []

-- | Loja do modo de sobrevivência
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

-- | Tiros associados ao modo de sobrevivência
tirosN1 :: [Tiro]
tirosN1 = []


-- | Jogo do modo de sobrevivência

jogoS :: Jogo
jogoS = Jogo { 
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

-- |Função que permite gerar ondas infinitas de dificuldade crescente
criaInfinitas :: Onda -> Int -> [Onda]
criaInfinitas onda num = onda :criaInfinitas  (aumentaDificuldade onda)  (num+5)
  where aumentaDificuldade :: Onda -> Onda 
        aumentaDificuldade onda1 = 
          onda1 {cicloOnda = case cicloOnda onda of 
                                      0.5 ->0.5 
                                      _ -> cicloOnda onda - 0.25,
                inimigosOnda = repete (num+5) (aumentaInimigo (head (inimigosOnda onda)))}
        aumentaInimigo :: Inimigo -> Inimigo 
        aumentaInimigo inimigo = 
          inimigo {vidaInimigo = vidaInimigo inimigo + 20,
                    velocidadeInimigo = velocidadeInimigo inimigo + 0.2,
                   ataqueInimigo = ataqueInimigo inimigo + 1,
                   butimInimigo = case butimInimigo inimigo of 
                                            1 ->   1
                                            _ -> butimInimigo inimigo - 1}