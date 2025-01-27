{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425
import Tarefa1
import Tarefa2
import Data.Maybe


-- |Função que atualiza o jogo conforme o tempo passado
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t jogo@(Jogo base portais torres mapa inimigos _ tiros torreSel posRato velocidade _ _ pregos pregoSel boost) = 
    jogo {inimigosJogo = inimigos9, torresJogo = torres1, baseJogo = base2, portaisJogo = portais1, tirosJogo = tiros3, torreSelecionada = torreSel2, pregosJogo = pregos2,pregoSelecionado = pregoSel1, boostJogo = boost1}
    where 
        (inimigos1, torres1,tiros1) = atualizaInimigosTorres boost t velocidade torres inimigos tiros
        (inimigos2, base1) = retiraInimigosMortos base inimigos1 
        (base2, inimigos3) = atualizaVidaBase base1 inimigos2
        (portais1,inimigos4) = atualizaPortais t velocidade portais inimigos3
        inimigos5 = atualizaDirecaoInimigos base2 mapa inimigos4
        inimigos6 = atualizaPosicaoInimigos t velocidade inimigos5
        inimigos7 = atualizaInimigos boost t velocidade inimigos6
        (inimigos8,pregos1) = atualizaPregosInimigos inimigos7 pregos
        pregos2 = removePregosMortos pregos1
        tiros2 = removeTiros tiros1
        tiros3 = atualizaPosicaoTiros boost t velocidade tiros2
        torreSel2 = atualizaPosicaoTorre torreSel posRato 
        pregoSel1 = atualizaPosicaoPrego pregoSel posRato 
        inimigos9 = alteraVisibilidade mapa inimigos8
        boost1 = atualizaBoost t velocidade boost 



-- |Função que atualiza os inimigos e as torres conforme o tempo passado
atualizaInimigosTorres ::Boost -> Tempo -> Float -> [Torre] -> [Inimigo]-> [Tiro] -> ([Inimigo], [Torre], [Tiro] ) 
atualizaInimigosTorres _ _  _ [] inimigos tiros= (inimigos,[],tiros)  
atualizaInimigosTorres boost t v (torre:torres) inimigos tiros= 
    (inimigosA, torreA : torresR, tirosA ++ tirosR)
    where (inimigosA, torreA, inimigosAAtacar) = atualizaTorreInimigos boost t v torre inimigosR
          (inimigosR,torresR,tirosR) = atualizaInimigosTorres boost t v torres inimigos tiros
          tirosA = criaTiros torre inimigosAAtacar


-- |Função auxiliar que atualiza os inimigos ao alcance de uma torre e a torre em si
atualizaTorreInimigos :: Boost -> Tempo -> Float -> Torre -> [Inimigo] -> ([Inimigo], Torre, [Inimigo]) 
atualizaTorreInimigos boost t  v  torre inimigos
    |tempoTorre torre <= 0 && inimigosNoAlcance torre inimigos == [] = (inimigos, torre, [])
    |tempoTorre torre <= 0 = (inimigosAtacados ++ inimigosRestantes, torre {tempoTorre = cicloTorre torre }, inimigosAAtacar) 
    |otherwise = (inimigos, torreAtualizada, [])
    where inimigosAlcance = inimigosNoAlcance torre inimigos
          inimigosAAtacar = take (rajadaTorre torre) (sortByDistance inimigosAlcance (posicaoTorre torre))
          inimigosRestantes = filter (\ini -> notElem ini inimigosAAtacar) inimigos
          torreAtualizada = torre {tempoTorre = tempoTorre torre - t*v*vB}
          inimigosAtacados = map (atingeInimigo torre) inimigosAAtacar
          vB = velocidadeBoost boost

-- |Função que ordena os inimigos de acordo com a distância que se encontram de um ponto (utilizada para colocar a torre a dar prioridade aos inimigos que se encontram mais perto dela)
sortByDistance :: [Inimigo] -> Posicao -> [Inimigo]
sortByDistance [] _ = []
sortByDistance (i:is) posicao = 
    sortByDistance (filter (\ini -> distancia (posicaoInimigo ini) posicao <= distancia (posicaoInimigo i) posicao ) is) posicao ++ 
    [i] ++ sortByDistance (filter (\ini -> distancia (posicaoInimigo ini) posicao > distancia (posicao) (posicaoInimigo i)) is) posicao 
    
-- |Função responsável por criar os tiros 
criaTiros :: Torre -> [Inimigo] -> [Tiro]
criaTiros torre inimigos = map (criaTiro torre) inimigos
        where 
            criaTiro :: Torre -> Inimigo -> Tiro 
            criaTiro (Torre {posicaoTorre = (x1,y1)}) (Inimigo {posicaoInimigo = (x2,y2)}) 
                = ((x1,y1), (x2,y2), (5*(x2-x1),5*(y2-y1)), tipoProjetil (projetilTorre torre))


-- |Função responsável por remover os tiros quando chegam aos inimigos 
removeTiros :: [Tiro] -> [Tiro]
removeTiros [] = [] 
removeTiros ((pos1,pos2,vel,tipo):t) 
    |distancia pos1 pos2 <= 0.08 = removeTiros t 
    |otherwise = (pos1,pos2,vel,tipo):removeTiros t

-- |Função que atualiza a posição dos tiros conforme o tempo vai passado
atualizaPosicaoTiros:: Boost -> Tempo -> Float -> [Tiro] -> [Tiro]
atualizaPosicaoTiros boost t v tiros = map (\((x1,y1),final,(vx,vy),tipo) -> ((x1+vx*t*(sqrt v)*(sqrt vB),y1+vy*t* (sqrt v)* (sqrt vB)),final,(vx,vy),tipo)) tiros 
    where vB = velocidadeBoost boost

-- |Função que retira da lista dos projeteis, os projeteis com duração finita cuja duração já acabou 
tiraProjeteisAcabados:: [Inimigo] -> [Inimigo]
tiraProjeteisAcabados inimigos = map tira inimigos 
    where tira :: Inimigo -> Inimigo
          tira inimigo = inimigo {projeteisInimigo = projeteisA }
                where 
                projeteisA =  
                    filter (\proj -> ePositiva (duracaoProjetil proj ))  (projeteisInimigo inimigo)
                ePositiva :: Duracao -> Bool 
                ePositiva Infinita = True 
                ePositiva (Finita t) = t > 0

-- |Dano do fogo: o fogo vai tirar danoFogo de vida por segundo ao inimigo (o que vai corresponder a danoFogo* tempo de cada atualização) )
danoFogo :: Float
danoFogo = 5

-- |Velocidade dos inimigos: corresponde a velocidadeDefault blocos por segundo (corresponde a velocidadeDefault* tempo de cada atualização)
velocidadeDefault :: Float 
velocidadeDefault  = 0.8

-- |Velocidade nos inimigos quando atingidos por um projétil de resina
velocidadeComResina :: Float 
velocidadeComResina = 0.5

-- |Função que atualiza os inimigos de acordo com a projeteis ativos neles 
atualizaInimigos ::Boost -> Tempo -> Float -> [Inimigo] -> [Inimigo]
atualizaInimigos boost t v inimigos = map (atualizaInimigo boost t v) (tiraProjeteisAcabados inimigos)

-- |Função que atualiza um inimigo conforme os projeteis ativos nele
atualizaInimigo :: Boost -> Tempo -> Float -> Inimigo -> Inimigo
atualizaInimigo boost t v inimigo
    |temTipo listaProjeteis Fogo = inimigo {vidaInimigo = vidaInimigo inimigo - danoFogo*t*v, velocidadeInimigo = velocidadeDefault,projeteisInimigo = atualizaDuracaoProjeteis boost t v listaProjeteis}
    |temTipo listaProjeteis Gelo = inimigo {velocidadeInimigo = 0, projeteisInimigo = atualizaDuracaoProjeteis boost t v listaProjeteis} 
    |temTipo listaProjeteis Resina = inimigo {velocidadeInimigo = velocidadeComResina,projeteisInimigo= atualizaDuracaoProjeteis boost t v listaProjeteis}   
    |otherwise = inimigo {velocidadeInimigo = velocidadeDefault}  
    where listaProjeteis = projeteisInimigo inimigo 

-- |Função auxiliar que atualiza a duração dos projeteis ativos 
atualizaDuracaoProjeteis ::Boost -> Tempo-> Float -> [Projetil]-> [Projetil]
atualizaDuracaoProjeteis boost t v projeteis =
    map atualizaDuracao projeteis 
    where vB = velocidadeBoost boost 
          atualizaDuracao :: Projetil -> Projetil
          atualizaDuracao proj = case duracaoProjetil proj of 
                Infinita -> proj 
                Finita n -> proj {duracaoProjetil = Finita (n-t*v*vB)}

-- | Função responsável por alterar a direção dos inimigos de modo a que eles vão em direção à base
atualizaDirecaoInimigos :: Base -> Mapa -> [Inimigo] -> [Inimigo]
atualizaDirecaoInimigos base mapa inimigos = map (atualizaDirecaoInimigo base mapa) inimigos

-- | Função auxiliar que atualiza a direção de um inimigo de modo a que ele se mova em direção à base
atualizaDirecaoInimigo :: Base -> Mapa -> Inimigo -> Inimigo
atualizaDirecaoInimigo base mapa inimigo
    |proximoCentro p= case lista of
        [] -> inimigo
        [_] -> inimigo
        (_:h2:_) -> mudaDirecao inimigo h2
    | otherwise = inimigo
    where
        p =posicaoInimigo inimigo
        lista = daCaminho mapa (arredondaCentro p) (posicaoBase base)

-- |Função que verifica se uma posição está perto do centro de um bloco
proximoCentro :: Posicao -> Bool 
proximoCentro (x,y) = 
    abs (x- (fromIntegral(floor x :: Int)+0.5)) <= 0.05 && abs (y- (fromIntegral (floor y :: Int)+0.5)) <= 0.05

-- | Função que arredonda uma posição para o centro do bloco mais próximo
arredondaCentro :: Posicao -> Posicao 
arredondaCentro (x,y) = 
    (fromIntegral (floor x :: Int) + 0.5, fromIntegral (floor y :: Int) + 0.5)

-- | Função auxiliar para mudar a direção de um inimigo de acordo com a posição para onde ele se deve mover
mudaDirecao :: Inimigo -> Posicao -> Inimigo
mudaDirecao inimigo@(Inimigo {posicaoInimigo = (x, y)}) (x2, y2)
    | y1 > y2 = inimigo {direcaoInimigo = Norte}
    | y1 < y2 = inimigo {direcaoInimigo = Sul}
    | x1 > x2 = inimigo {direcaoInimigo = Oeste}
    | x1 < x2 = inimigo {direcaoInimigo = Este} 
    | otherwise = inimigo
    where (x1,y1) = arredondaCentro (x,y)

-- | Função que dá o caminho entre duas posições por terreno terra ou cimento 
daCaminho :: Mapa -> Posicao -> Posicao -> [Posicao]
daCaminho mapa inicio fim =
    case filter (/= Nothing) (daCaminho' mapa fim [] inicio) of
        [] -> []
        Just l:_ -> reverse l
        Nothing:_ -> []

-- | Função auxiliar que dá todos os caminhos entre duas posições por terreno terra ou cimento 
daCaminho' :: Mapa -> Posicao -> [Posicao] -> Posicao -> [Maybe [Posicao]]
daCaminho' mapa fim l atual@(x, y)
    | atual == fim = [Just (atual:l)]
    | null caminhosPossiveisValidos = [Nothing]
    | otherwise = concatMap (daCaminho' mapa fim (atual:l)) caminhosPossiveisValidos
    where
        caminhosPossiveisValidos = filter (\pos -> eTerraOuCimento mapa pos && notElem pos l) caminhosPossiveis
        -- | Função que dá os possíveis caminhos (cima, baixo, esquerda e direita)
        caminhosPossiveis :: [Posicao]
        caminhosPossiveis = [(x, y-1), (x, y+1), (x+1, y), (x-1, y)]
    
-- |Função que atualiza a posição dos inimigos conforme a sua direção e velocidade 
atualizaPosicaoInimigos ::Tempo -> Float -> [Inimigo] -> [Inimigo]
atualizaPosicaoInimigos t v inimigos = map (atualizaPosicaoInimigo t v)inimigos 

-- |Função auxiliar que atualiza a posição de um inimigo conforme a sua direção e velocidade
atualizaPosicaoInimigo :: Tempo-> Float  -> Inimigo -> Inimigo 
atualizaPosicaoInimigo t v i@(Inimigo {direcaoInimigo = Norte}) = i {posicaoInimigo = (x,y- velocidadeInimigo i* t*v)}
    where (x,y) = posicaoInimigo i
atualizaPosicaoInimigo t v i@(Inimigo {direcaoInimigo = Sul}) = i {posicaoInimigo = (x,y+velocidadeInimigo i*t*v )}
    where (x,y) = posicaoInimigo i
atualizaPosicaoInimigo t v i@(Inimigo {direcaoInimigo = Este}) = i {posicaoInimigo = (x+velocidadeInimigo i *t*v,y)}
    where (x,y) = posicaoInimigo i
atualizaPosicaoInimigo t v i@(Inimigo {direcaoInimigo = Oeste}) = i {posicaoInimigo = (x-velocidadeInimigo i *t*v,y)}
    where (x,y) = posicaoInimigo i


-- | Função que remove inimigos mortos da lista e atualiza os créditos da base
retiraInimigosMortos ::Base -> [Inimigo] -> ([Inimigo],Base) 
retiraInimigosMortos base [] = ([],base)
retiraInimigosMortos base (i :is) 
        |vidaInimigo i <= 0 = (inims, baseA {creditosBase = creditosBase base + butimInimigo i})
        |otherwise = (i:inims, baseA) 
        where (inims,baseA) = retiraInimigosMortos base is 

-- |Função que atualiza a vida da base conforme os inimigos que a atacam
atualizaVidaBase :: Base -> [Inimigo] -> (Base, [Inimigo])
atualizaVidaBase base [] = (base, [])
atualizaVidaBase base (i:is)
    |distancia (posicaoInimigo i) (posicaoBase base) <= 0.3  = (baseA {vidaBase = vidaBase baseA - ataqueInimigo i}, inimigosR)
    |otherwise = (baseA, i:inimigosR)
    where (baseA, inimigosR) = atualizaVidaBase base is 


-- |Função responsável por ativar um inimigo de cada portal e atualizar os inimigos ativos 
atualizaPortais :: Tempo -> Float -> [Portal] -> [Inimigo] -> ([Portal], [Inimigo])
atualizaPortais _ _ [] inimigos = ([], inimigos)
atualizaPortais t v (p@(Portal {ondasPortal = []}):prts) inims = (p:portaisR, inimigosA)
    where (portaisR, inimigosA) = atualizaPortais t v prts inims
atualizaPortais t  v (p@(Portal {ondasPortal = o:os}):prts) inimigos 
    | entradaOnda o > 0 = ((p {ondasPortal = (o {entradaOnda = entradaOnda o - t*v}):os}):portaisR, inimigosA) 
    | tempoOnda o <= 0 = (portal  : portaisR, inimigosF)
    | otherwise = (p {ondasPortal = o {tempoOnda = tempoOnda o -t*v}:os }: portaisR, inimigosA)
    where 
        (portal, inimigosF) = ativaInimigo p inimigosA
        (portaisR, inimigosA) = atualizaPortais t v prts inimigos 

-- |Função responsável por atualizar o Boost em função do tempo 
atualizaBoost :: Tempo -> Float -> Boost -> Boost 
atualizaBoost t v boost
    |ativoBoost boost && tempoBoost boost <= 0 = boost {velocidadeBoost = velocidadeSBoost boost, tempoBoost = duracaoBoost boost, ativoBoost = False }
    |ativoBoost boost = boost {tempoBoost = tempoBoost boost - t*v}
    |otherwise = boost



-- |Função responsável por colocar a torre selecionada pelo jogador no mapa
colocaTorre ::Bool -> Maybe Torre -> Posicao->  Mapa -> [Torre] -> (Maybe Torre, [Torre])
colocaTorre tresD torre1 pos1 mapa1 torres1 = 
        case torre1 of 
            Nothing -> (torre1,torres1)
            Just t ->   colocaTorre' t pos1 mapa1 torres1 
            
        where 
            colocaTorre' :: Torre -> Posicao->  Mapa -> [Torre] ->  (Maybe Torre, [Torre])
            colocaTorre' torre pos mapa torres  = case posM of 
                                                    Nothing -> (Just torre,torres) 
                                                    (Just p) -> if eRelva mapa p && notElem p (map posicaoTorre torres)  then (Nothing, torre {posicaoTorre = p}:torres)
                                                                else  (Just torre,torres)
                where posM = transformaGlossMat tresD pos mapa 

-- |Função que transforma as coordenadas do gloss para as coordenadas do mapa
transformaGlossMat :: Bool -> Posicao -> Mapa ->  Maybe Posicao 
transformaGlossMat tresD (x,y) mapa
    |x >= -(largura/2)  && x <= (largura/2) && y >= (altura/(-2)) && y<=  altura/2 && not tresD = 
         Just $ arredondaCentro ((x+(largura/2))/tileSize, (y-(altura/2))/(-tileSize))
    |tresD = transformaGlossMat False posF mapa 
    |otherwise = Nothing    
    where posF = (cos ang * x' +sin ang * y', cos ang *y'- sin ang * x') 
          (x',y') = (x, 1/contracaoVertical * y)   
          l = length (head mapa)
          a = length mapa 
          largura = fromIntegral l * tileSize 
          altura = fromIntegral a * tileSize 
          ang = converteEmRadianos (-anguloRotacao )


-- |Função auxiliar que converte graus em radianos
converteEmRadianos :: Float -> Float 
converteEmRadianos graus = graus * pi / 180

-- |Valor da rotação aplicado no mapa para dar o efeito de perspetiva 3D
anguloRotacao:: Float 
anguloRotacao = 45

-- |Valor da contração vertical aplicado no mapa para dar o efeito de perspetiva 3D
contracaoVertical :: Float 
contracaoVertical = 0.4

-- |Funçao responsável por selecionar a torre da loja que o jogador clicou
selecionaTorre :: Posicao -> Loja -> Base -> (Maybe Torre, Base) 
selecionaTorre pos loja base 
    |podeComprar pos loja base=  (Just ( (snd (fromJust tmc) ) {posicaoTorre= pos}), base {creditosBase = creditosBase base - fst (fromJust tmc)})
    |otherwise = (Nothing, base)
    where tmc = torreEmCima pos loja


-- |Função que devolve a torre da loja cujo rato clicou em cima 
torreEmCima :: Posicao -> Loja -> Maybe (Creditos,Torre) 
torreEmCima pos loja 
    |distancia pos (posx,posyFogo) <= tileSize/2 = Just (daTorre Fogo loja)
    |distancia pos (posx,posyResina) <= tileSize/2 =Just (daTorre Resina loja) 
    |distancia pos (posx,posyGelo) <= tileSize/2 = Just (daTorre Gelo loja)
    |otherwise = Nothing 
    
-- |Função que atualiza a posição da torre selecionada para a posicao do rato 
atualizaPosicaoTorre :: Maybe Torre -> Posicao -> Maybe Torre
atualizaPosicaoTorre Nothing _ = Nothing 
atualizaPosicaoTorre (Just torre) pos = Just (torre {posicaoTorre = pos})

-- |Função que devolve a torre da loja cujo tipo é igual ao tipo recebido assim como o seu custo 
daTorre :: TipoProjetil -> Loja -> (Creditos,Torre) 
daTorre tipo loja = head (filter (\(_,torre) -> tipoProjetil (projetilTorre torre) == tipo) loja)

-- |Função que verifica se o jogador pode comparar a torre em que clicou 
podeComprar :: Posicao -> Loja -> Base -> Bool
podeComprar  pos loja base 
    |torreEmCima pos loja  == Nothing = False
    |devolvePreco (torreEmCima pos loja) > creditosBase base = False 
    |otherwise = True 
    where devolvePreco :: Maybe (Creditos,Torre) -> Creditos 
          devolvePreco (Just (c,_)) = c 
          devolvePreco Nothing = 0

-- |Posição das torres na loja na componente das abcissas  
posx :: Float
posx = -1080 

-- |Posição da torre Fogo na loja na componente das ordendas 
posyFogo :: Float
posyFogo = 510 

-- |Posição da torre Gelo na loja na componente das ordenadas 
posyGelo :: Float
posyGelo = 170 

-- |Posição da torre Resina na loja na componente das ordenadas 
posyResina :: Float
posyResina = -170       

-- |Valor do comprimento e largura de um bloco do mapa 
tileSize :: Float
tileSize = 128


-- |Função que repete um elemento n vezes numa lista
repete :: Int -> a -> [a]
repete 0 _ = []
repete n a = a: repete (n-1) a 




-- |Variável que permite trocar de resoluções (se for 1920X1080 entao escala deve ser 0.75, a for 2560X1440 deve ser 1 por exemplo, só funciona caso as proporções da escala de mantenham)
escalaInicial:: Float 
escalaInicial = 0.75

-- |Função utilizada para devolver um 'a' conforme o nível atual  
daN :: Int -> [a] -> a 
daN a jogos = jogos !! n
    where n = mod a (length jogos)

-- |Função que devolve a torre cujo o rato está em cima 
daTorreEmJogo:: Bool -> Mapa -> Posicao -> [Torre] -> Maybe Torre
daTorreEmJogo e3D mapa pos torres = case posM of 
                                Nothing -> Nothing 
                                Just p -> if elem p (map posicaoTorre torres) then Just (head (filter (\t -> posicaoTorre t == p) torres))
                                            else Nothing                                
    where posM = transformaGlossMat e3D pos mapa 
        
-- | Função responsável por determinr se o jogador pode fazer a evolução esolhida (se tem créditos suficientes)
podeEvoluir ::  Base -> Evolucao -> Bool 
podeEvoluir base evolution = creditosBase base >= associaCusto evolution
    
-- |Função que associa um custo a cada evolução
associaCusto :: Evolucao -> Creditos 
associaCusto Alcance = 20
associaCusto Dano = 25
associaCusto Rajada = 30
associaCusto Ciclo = 25
associaCusto Nada = 0 

-- |Função responsável por evoluir uma torre  
evoluiTorre ::Torre-> Base -> [Torre] -> (Base, [Torre])
evoluiTorre  torreEscolhida  base torres =  
    if podeEvoluir base (tipoEvolucao torreEscolhida) && (nivelTorre torreEscolhida <= 2)then (base {creditosBase = creditosBase base - associaCusto (tipoEvolucao torreEscolhida)}, torresAtualizadas)
                  else (base,torres)
    where
          torresAtualizadas = map (\t -> if posicaoTorre t == posicaoTorre torreEscolhida then evolui torreEscolhida else t) torres
          evolui :: Torre -> Torre
          evolui torre = case tipoEvolucao torre of 
                                Alcance -> torre {alcanceTorre = alcanceTorre torre + 0.5, nivelTorre = nivelTorre torre + 1}
                                Dano -> torre {danoTorre = danoTorre torre + 8, nivelTorre = nivelTorre torre + 1}
                                Rajada -> torre {rajadaTorre = rajadaTorre torre + 1, nivelTorre = nivelTorre torre + 1}
                                Ciclo -> torre {cicloTorre = cicloTorre torre - 0.1,nivelTorre = nivelTorre torre + 1}
                                Nada -> torre



-- |Função responsável por vender uma torre
venderTorre :: Torre -> Base -> [Torre] -> (Base, [Torre])
venderTorre torre base torres = (base {creditosBase = creditosBase base + acrescenta}, filter (\t -> posicaoTorre t /= posicaoTorre torre) torres)
    where acrescenta = daValorDeVenda torre

-- |Função que retorna os creditos que o jogador recebe por vender uma torre dependendo do seu tipo e do nível da torre
daValorDeVenda :: Torre -> Creditos
daValorDeVenda torre = case tipoProjetil (projetilTorre torre) of 
                            Fogo -> 30 + 20*(nivelTorre torre - 1)
                            Gelo -> 25 + 20*(nivelTorre torre - 1)
                            Resina -> 25 + 20*(nivelTorre torre - 1)


-- |Função que atualiza a lista de pregos a de inimigos de acordo com os pregos que atingiram os inimigos     
atualizaPregosInimigos ::[Inimigo] -> [Prego] -> ([Inimigo], [Prego])
atualizaPregosInimigos inimigos [] = (inimigos, [])
atualizaPregosInimigos inimigos (p:ps) = (inimigosA, pregoA:pregosR)
    where 
        (inimigosA,pregoA) = atualizaPregoInimigos inimigosR p
        (inimigosR,pregosR) = atualizaPregosInimigos inimigos ps

-- |Função auxiliar que atualiza a lista dos inimigos em função de um prego 
atualizaPregoInimigos :: [Inimigo] -> Prego -> ([Inimigo], Prego)
atualizaPregoInimigos [] prego =([], prego)
atualizaPregoInimigos (i:is) prego 
    |distancia (posicaoInimigo i) (posicaoPrego prego) <= 0.5= (inimigosA, pregoA {vidaPrego = vidaPrego pregoA - ataqueInimigo i})
    |otherwise = (i:inimigosA, pregoA)
    where (inimigosA, pregoA) = atualizaPregoInimigos is prego

-- |Função que remove os pregos mortos (vida não positiva) da lista de pregos
removePregosMortos :: [Prego] -> [Prego]
removePregosMortos pregos = filter (\p -> vidaPrego p > 0) pregos 


-- |Função que seleciona um prego da loja 
selecionaPrego :: Base ->Posicao -> (Maybe Prego,Base) 
selecionaPrego base pos 
    |pregosBase base > 0  &&  estaDentroPregos pos = (Just (Prego {posicaoPrego = pos, vidaPrego = vidaInicialPrego}), base {pregosBase = pregosBase base - 1})
    |otherwise = (Nothing, base)

-- |Função que verifica se o rato está em cima de um prego na loja 
estaDentroPregos :: Posicao -> Bool
estaDentroPregos pos = distancia pos (posx,posyPrego) <= tileSize/2

-- |Posição dos pregos na loja na componente das ordenadas
posyPrego :: Float 
posyPrego = -510

-- |Valor da vida inicial de um prego
vidaInicialPrego :: Float 
vidaInicialPrego = 20

-- |Função responsável por colocar um prego no mapa
colocaPrego :: Base -> [Portal] -> Bool -> Mapa -> Prego ->  Posicao -> [Prego] -> ([Prego], Maybe Prego) 
colocaPrego base portais e3D mapa prego pos pregos = case posf of
                                            Just p ->  if eTerra mapa p && p `notElem` (map posicaoPrego pregos  ++ [posicaoBase base] ++ map posicaoPortal portais ) then (prego {posicaoPrego = p}:pregos,Nothing)
                                                                                                                else (pregos, Just prego)
                                            Nothing -> (pregos, Just prego) 
    where 
        posf = transformaGlossMat e3D pos mapa


-- |Função que atualiza a posição de um selecionado prego para uma posição (a posição do rato)
atualizaPosicaoPrego :: Maybe Prego -> Posicao -> Maybe Prego 
atualizaPosicaoPrego  prego pos 
    |prego == Nothing = Nothing 
    |otherwise = Just ((fromJust prego) {posicaoPrego = pos})

-- |Função responsável por alterar a visibilidade dos inimigos confrome o tipo de terreno que eles se encontram 
alteraVisibilidade  :: Mapa -> [Inimigo] -> [Inimigo] 
alteraVisibilidade mapa inimigos = map poemInvisivel inimigos 
    where 
        poemInvisivel ::Inimigo -> Inimigo 
        poemInvisivel inimigo 
            | eCimento mapa (posicaoInimigo inimigo) = inimigo{visivelInimigo = False} 
            |otherwise = inimigo {visivelInimigo = True}




 