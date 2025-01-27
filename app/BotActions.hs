{- |
Module      : BootActions
Description : Módulo para as ações do boot de LI1 em 2024/25.
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo para as ações do boot de LI1 em 2024/25.
-}

module BotActions where

import Tarefa3 
import Tarefa1
import LI12425 
import Data.List
import Data.Ord (comparing)
import Tarefa2

-- |Funçã que dá a ação do boot (a criação do bot tem como objetivo que ele seja bom a jogar, pelo que ele é colocado apenas a fazer ações que consideramos as melhores possíveis)
botAction :: Jogo -> Jogo 
botAction j@(Jogo base portais torres mapa inimigos loja tiros torreSel mousePos  velocidade e3D torreASelecionar pregos pregoSel boost) 
    -- | A primeira coisa que boot faz é, se houver pregos, colocar os pregos no mapa no local mais pŕoximo da base possível 
    |pregosBase base > 0 = j {pregosJogo = pregos1, baseJogo = base1}
    |deveEvoluir j && existeParaEvoluir j= j {torresJogo = torres1, baseJogo = base2}
    |deveUsarBoost j && numeroBoost boost > 0 &&  ativoBoost boost == False = usaBoost j
    |deveComprarTorre j = compraTorre j
    |otherwise = j 
    where 
     (pregos1, base1) = metePrego (caminhoTotal portais base mapa) mapa base portais pregos
     (base2, torres1) = evTorre (reverse torres) torres base


-- |Função que dá o caminho total percorrido pelos que crescem no portal mais próximo da base 
caminhoTotal :: [Portal] -> Base -> Mapa -> [Posicao]
caminhoTotal portais base mapa = head (filter (\c -> length c == minimum (map length caminhos)) caminhos)
    where 
        caminhos = map (daCaminho mapa (posicaoBase base)) (map posicaoPortal portais)

-- |Função constante que dá um prego com as características iniciais
pregoDefault :: Prego 
pregoDefault = Prego (0,0) vidaInicialPrego

-- |Função que coloca os pregos no mapa 
metePrego :: [Posicao] -> Mapa -> Base -> [Portal] -> [Prego] -> ([Prego],Base)
metePrego [] _ base _ pregos = (pregos,base)
metePrego (p:ps) mapa base portais pregos 
    | eTerra mapa p && notElem p (pBase:pPregos ++ pPortais) = (pregoDefault {posicaoPrego = p}:pregos, base {pregosBase = pregosBase base - 1})
    | otherwise = metePrego ps mapa base portais pregos
    where 
        pPregos = map posicaoPrego pregos
        pBase = posicaoBase base 
        pPortais = map posicaoPortal portais

-- |Função responsável por comprar uma torre tendo em conta a situação do jogo
compraTorre :: Jogo -> Jogo 
compraTorre jogo 
    |length (torresJogo jogo) == 1 = jogo {torresJogo = segundaMelhor{posicaoTorre = daMelhorPosicao jogo caminho} : torresJogo jogo , baseJogo = (baseJogo jogo) {creditosBase = creditosBase (baseJogo jogo) - fst (daTorre tProjetilSM (lojaJogo jogo))}}
    |otherwise  = jogo {torresJogo = melhorTorre {posicaoTorre = daMelhorPosicao jogo caminho} : torresJogo jogo, baseJogo = (baseJogo jogo) {creditosBase = creditosBase (baseJogo jogo) - fst (daTorre tProjetilM (lojaJogo jogo))}}
    
    where 
        melhorTorre = snd (head (avaliaTorre (lojaJogo jogo)))
        segundaMelhor = snd (head (tail (avaliaTorre (lojaJogo jogo))))
        tProjetilM = tipoProjetil (projetilTorre melhorTorre)
        tProjetilSM = tipoProjetil (projetilTorre segundaMelhor)
        caminho  = reverse $ caminhoTotal (portaisJogo jogo) (baseJogo jogo) (mapaJogo jogo)


-- |Função que dá a melhor posição para colocar uma torre tendo em conta o mapa em jogo
daMelhorPosicao ::Jogo -> [Posicao] -> Posicao
daMelhorPosicao jogo [] = sBestPosicao jogo (reverse (caminhoTotal (portaisJogo jogo) (baseJogo jogo) (mapaJogo jogo)))
daMelhorPosicao jogo ((x,y):ps) 
    |eRelva (mapaJogo jogo) n && notElem n (map posicaoTorre (torresJogo jogo))  && temCurva  (mapaJogo jogo) n= n
    |eRelva (mapaJogo jogo) s && notElem s (map posicaoTorre (torresJogo jogo))  && temCurva  (mapaJogo jogo) s= s
    |eRelva (mapaJogo jogo) e && notElem e (map posicaoTorre (torresJogo jogo))  && temCurva  (mapaJogo jogo) e= e
    |eRelva (mapaJogo jogo) o && notElem o (map posicaoTorre (torresJogo jogo))  && temCurva  (mapaJogo jogo) o= o 
    |otherwise = daMelhorPosicao jogo ps
    where
        n = (x,y-1)
        s = (x,y+1)
        e = (x+1,y)
        o = (x-1,y)

-- |Função que dá a segunda melhor posição para colocar uma torre tendo em conta não foi possível colocar a melhor torre na melhor posição
sBestPosicao :: Jogo -> [Posicao] -> Posicao
sBestPosicao  _ [] = (0,0)
sBestPosicao jogo (p:ps) 
    |eRelva (mapaJogo jogo) n && notElem n (map posicaoTorre (torresJogo jogo)) = n
    |eRelva (mapaJogo jogo) s && notElem s (map posicaoTorre (torresJogo jogo)) = s
    |eRelva (mapaJogo jogo) e && notElem e (map posicaoTorre (torresJogo jogo)) = e
    |eRelva (mapaJogo jogo) o && notElem o (map posicaoTorre (torresJogo jogo)) = o
    |otherwise = sBestPosicao jogo ps
    where
        (x,y) = p
        n = (x,y-1)
        s = (x,y+1)
        e = (x+1,y)
        o = (x-1,y)


-- |Função que verifica se uma posição se encontra junto de uma curva
temCurva :: Mapa -> Posicao -> Bool 
temCurva mapa (x,y) = eTerra mapa cima && eTerra mapa diCima && eTerra mapa direita ||
                        eTerra mapa cima && eTerra mapa esCima && eTerra mapa esquerda ||
                        eTerra mapa baixo && eTerra mapa diBaixo && eTerra mapa direita ||
                        eTerra mapa baixo && eTerra mapa esBaixo && eTerra mapa esquerda

    where
     cima = (x,y-1)
     esCima = (x-1,y-1)
     esBaixo = (x-1,y+1)
     diCima = (x+1,y-1)
     diBaixo = (x+1,y+1)
     baixo = (x,y+1)
     esquerda = (x-1,y)
     direita = (x+1,y)




-- |Função que verifica se o bot deve comprar uma torre
deveComprarTorre :: Jogo -> Bool 
deveComprarTorre jogo =  
    (creditosBase (baseJogo jogo) >= fst (daTorre (tipoProjetil (projetilTorre melhorTorre)) (lojaJogo jogo)))
    where 
        melhorTorre =  snd (head (avaliaTorre (lojaJogo jogo)))


-- |Função que avalia as torres da loja para decidir qual a melhor torre a comprar tendo em conta as suas características
avaliaTorre:: Loja -> [(Float, Torre)]
avaliaTorre loja = 
    reverse listaO
    where 
        avalia:: (Creditos, Torre) -> (Float, Torre) 
        avalia (c,t) = ((danoTorre t + 2* alcanceTorre t + 10*(fromIntegral (rajadaTorre t)) + 5*(1/cicloTorre t))/ (fromIntegral c),t)
        lista = map avalia [daTorre Gelo loja, daTorre Fogo loja, daTorre Resina loja]
        listaO = sortBy (comparing fst) lista

-- |Função que verifica se o jogar deve evoluir uma torre 
deveEvoluir :: Jogo -> Bool
deveEvoluir jogo= length (torresJogo jogo) >= 4 && 
    creditosBase (baseJogo jogo) >= associaCusto (fst (head (avaliaEvolucao))) && 
    any (\t -> nivelTorre t < 3) (torresJogo jogo)
            
-- |Função que ordena as evoluções da melhor para a pior tendo em conta as suas características (os valores dados devem à experiência de jogo)
avaliaEvolucao :: [(Evolucao, Float)]
avaliaEvolucao = reverse listaO
    where 
        lista = [(Rajada, 100/ fromIntegral (associaCusto Rajada)), (Dano, 70/ fromIntegral(associaCusto Dano)), (Alcance, 65/ fromIntegral (associaCusto Alcance)), (Ciclo, 70/ fromIntegral (associaCusto Ciclo))]
        listaO = sortBy (comparing snd) lista

-- |Função responsavel por evoluir uma torre tendo em conta a evolução que é mais vantajosa
evTorre :: [Torre] -> [Torre] -> Base -> (Base,[Torre])
evTorre [] torres base = (base,torres)  
evTorre (t:ts) torres base 
    | nivelTorre t < 3 = evoluiTorre (t{tipoEvolucao = fst (head avaliaEvolucao)}) base torres 
    | otherwise = evTorre ts torres base 

-- |Função que determina se o bot deve usar o boost tendo em conta a sua situação no jogo
deveUsarBoost :: Jogo -> Bool
deveUsarBoost jogo  
    | not (null (pregosJogo jogo)) && any (perto (map posicaoPrego (pregosJogo jogo))) (inimigosJogo jogo) = True 
    | any (\i -> distancia (posicaoInimigo i) (posicaoBase (baseJogo jogo)) <= 1) (inimigosJogo jogo) = True
    | otherwise = False
    where perto :: [Posicao] -> Inimigo -> Bool 
          perto  ps i = any (\p ->distancia (posicaoInimigo i) p <= 1 ) ps


-- |Função que verifica se existe alguma torre que possa evoluir
existeParaEvoluir :: Jogo -> Bool 
existeParaEvoluir jogo = any (\t -> nivelTorre t < 3) (torresJogo jogo)


-- |Função que verifica se o jogo tem dinheiro suficiente para comprar a melhor torre
temDinheiro :: Jogo -> Bool
temDinheiro jogo = (creditosBase (baseJogo jogo) >= fst (daTorre (tipoProjetil (projetilTorre melhorTorre)) (lojaJogo jogo))) && length (torresJogo jogo) >= 4
    where melhorTorre = snd (head (avaliaTorre (lojaJogo jogo)))

-- |Função responsável por usar o boost 
usaBoost :: Jogo -> Jogo
usaBoost jogo = jogo {boostJogo = (boostJogo jogo) {ativoBoost = True, numeroBoost = numeroBoost (boostJogo jogo) -1, velocidadeBoost = velocidadeCBoost (boostJogo jogo)}}