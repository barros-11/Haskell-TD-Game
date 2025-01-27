{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425

-- |Função responsável por fornecer a lista de inimigos que estão no alcance de uma torre e que são simultâneamente visíveis  
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]                                
inimigosNoAlcance torre inimigos= 
    filter (\ini -> distancia posT (posicaoInimigo ini) <= alcanceT && visivelInimigo ini == True) inimigos
    where 
        posT = posicaoTorre torre 
        alcanceT = alcanceTorre torre 
            
-- |Função auxiliar que calcula a distância entre duas posições        
distancia:: Posicao -> Posicao -> Float 
distancia (x1,y1) (x2,y2) = 
        sqrt ((x1-x2)**2 + (y1-y2)**2)
    

-- |Função responsável por fazer as alterações a um inimigo quando ele é atingido por um projetil de uma torre 
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo torre inimigo = 
    inimigo {vidaInimigo = vidaInimigo inimigo - dano,
     projeteisInimigo = atualizaProjeteis (projeteisInimigo inimigo) projetil}
    where 
        dano = danoTorre torre 
        projetil = projetilTorre torre 

-- |Função que dado uma lista de projéteis e um projétil a ser adicionado à lista, atualiza a lista conforme as precedências 
atualizaProjeteis ::[Projetil]-> Projetil -> [Projetil]
atualizaProjeteis [] p = [p]
atualizaProjeteis pjs pj@(Projetil {tipoProjetil=Fogo}) 
    |pjs `temTipo` Gelo = removeProjetil Gelo pjs
    |pjs `temTipo` Resina =  dobraDuracao Fogo [pj]
    |pjs `temTipo` Fogo = somaDuracao pj pjs
    | otherwise = pj:pjs 
atualizaProjeteis pjs pj@(Projetil {tipoProjetil=Gelo}) 
    |pjs `temTipo` Fogo = removeProjetil Fogo pjs
    |pjs `temTipo` Gelo = somaDuracao    pj pjs   
    |otherwise = pj: pjs
atualizaProjeteis pjs pj@(Projetil {tipoProjetil=Resina}) 
    |pjs `temTipo` Fogo = dobraDuracao Fogo pjs 
    |pjs `temTipo` Resina = somaDuracao pj pjs 
    |otherwise  = pj: pjs 

-- |Função que dado um tipo de projetil e uma lista de projeteis duplica a duração ao projetil do tipo recebido 
dobraDuracao:: TipoProjetil -> [Projetil] -> [Projetil]
dobraDuracao _ [] = [] 
dobraDuracao tipo ((Projetil tipoP duracao ):t)
    |tipo == tipoP =  Projetil tipoP (dobra duracao) : t
    |otherwise = Projetil tipoP duracao: dobraDuracao tipo t 
    where 
        dobra:: Duracao -> Duracao 
        dobra (Finita n) = Finita (2*n)
        dobra Infinita = Infinita 

-- |Função que soma a duração de projétil recebido ao projetil do mesmo tipo contido na lista recebida 
somaDuracao:: Projetil -> [Projetil] -> [Projetil]
somaDuracao _ [] = [] 
somaDuracao pj ((Projetil tipoP duracao ):t)
    |tipo == tipoP =  Projetil tipoP (soma duracao duracao1) : t
    |otherwise = Projetil tipoP duracao : somaDuracao pj t
    where tipo = tipoProjetil pj 
          duracao1 = duracaoProjetil pj 
          soma:: Duracao -> Duracao -> Duracao 
          soma (Finita n) (Finita m) = Finita (n+m)
          soma  _ _ = Infinita


-- |Função que verifica se uma lista de projéteis tem um determinado tipo de projéteis 
temTipo:: [Projetil]-> TipoProjetil -> Bool 
temTipo pjs t = any (\p -> tipoProjetil p == t) pjs  
    

-- |Função responsável por retirar de uma lista de projeteis um projétil de determinado tipo 
removeProjetil :: TipoProjetil -> [Projetil] -> [Projetil] 
removeProjetil pj pjs = filter (\p -> tipoProjetil p /= pj) pjs 



-- |Função que dado um portal coloca um inimigo da onda ativa em jogo 
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo p@(Portal {ondasPortal = []}) inims = (p,inims)
ativaInimigo p@(Portal {ondasPortal = (Onda {entradaOnda = t,inimigosOnda = []}):os}) inims 
    |t <= 0 = ativaInimigo (p {ondasPortal = os}) inims
    |otherwise = (p,inims) 
ativaInimigo p@(Portal {ondasPortal = (Onda {entradaOnda = t,inimigosOnda = [i]}):os}) inims 
    |t <= 0 = (p {ondasPortal= os} ,i: inims)
    |otherwise = (p,inims)
ativaInimigo p@(Portal {ondasPortal = o@(Onda {entradaOnda = tempo, inimigosOnda = h:t}):os}) inims 
    |tempo <= 0 = (p {ondasPortal = o {inimigosOnda = t, entradaOnda = cicloOnda o}:os }, h:inims )
    |otherwise = (p,inims)

-- |Função indica se o jogo terminou
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo 

-- |Função responsável por indicar se o jogador ganhou o jogo 
ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = null (inimigosEmJogo ++ inimigosNaoEmJogo) &&
             vidaB > 0 
        where inimigosEmJogo = inimigosJogo jogo
              portais = portaisJogo jogo 
              ondas = concat (map ondasPortal portais) 
              inimigosNaoEmJogo = concat (map inimigosOnda ondas)
              vidaB = (vidaBase . baseJogo) jogo 

-- |Função responsável por indicar se o jogador perdeu o jogo 
perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vidaB <= 0 
        where vidaB = (vidaBase . baseJogo) jogo 
 

