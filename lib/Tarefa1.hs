{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Ricardo Daniel Pereira Lopes <a110498@alunos.uminho.pt>
              Afonso Capa de Barros <a112178@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425

-- |Função que verifica se um Jogo é válido 
validaJogo :: Jogo -> Bool 
validaJogo (Jogo base portais torres mapa inimigos _ _ _ _ _ _ _ _ _ _)=
  existePortal portais && 
  portalSobreTerra mapa portais && 
  verificaSeHaCaminhos mapa base portais && 
  portalNaoSobreposto mapa base portais torres && 
  apenasUmaOndaAtiva portais && 
  validaInimigos portais && 
  validaInimigosSobreTerraOuCimento mapa inimigos && 
  validaInimigosSemSobreposicao inimigos torres && 
  validaVelocidadeInimigos inimigos && 
  validaProjeteisAtivos inimigos &&
  verificaTorre mapa torres && 
  alcanceEDanoPositivo torres && 
  rajadaPositiva torres && 
  cicloNaoNegativo torres && 
  baseSobreTerra base mapa &&
  creditosNaoNegativos base 

  






-- |Função que verifica se existe pelo menos um portal (se a lista de portais não é  vazia)
existePortal :: [Portal] -> Bool 
existePortal l = not (null l)

-- |Função responsável por verificar se os portais se encontram sobre um terreno do tipo terra 
portalSobreTerra :: Mapa -> [Portal] -> Bool 
portalSobreTerra  mapa portais = all  (eTerra mapa) posicaoPortais 
    where posicaoPortais = map  posicaoPortal portais 

-- |Função que verifica se existe pelo menos um caminho (de terra ou cimento) válido ligando cada portal à base 
verificaSeHaCaminhos :: Mapa -> Base -> [Portal] -> Bool 
verificaSeHaCaminhos  mapa base portais = all (verificaCaminho mapa (posicaoBase base) []) posicoesPortais
    where posicoesPortais = map posicaoPortal portais

-- |Função auxiliar que dados um mapa e duas posicoes verfica se à caminho de terra ou cimento entre elas, acumulando o caminho percorrido.
verificaCaminho :: Mapa -> Posicao ->  [Posicao] -> Posicao -> Bool 
verificaCaminho mapa fim l atual@(x,y)  
    | atual == fim = True
    | null caminhosPossiveisValidos  = False
    | otherwise = any (verificaCaminho mapa fim  (atual:l) ) caminhosPossiveisValidos
    where 
        caminhosPossiveisValidos = 
            filter (\pos -> eTerraOuCimento mapa pos && (notElem pos l)) caminhosPossiveis
        -- |Função que dá os possíveis caminhos (cima,baixo,esquerda e direita)  
        caminhosPossiveis :: [Posicao]
        caminhosPossiveis = [(x,y-1), (x,y+1), (x+1,y), (x-1,y)] 
          
-- |Função que verfica se estes elementos (portal, base ou torre) estão sobrepostos, verifica se todas elas se encontram no centro de um bloco do mapa e ainda verifica se estão dentro do mapa.  
portalNaoSobreposto :: Mapa -> Base -> [Portal] -> [Torre]-> Bool 
portalNaoSobreposto mapa base portais torres =
   naoRepetidos posicoesTodas && validaPosicoes posicoesTodas && estaDentro posicoesTodas mapa 
    where 
        posicoesPortais = map posicaoPortal portais
        posicoesTorres = map posicaoTorre torres 
        posicoesTodas = posicaoBase base : posicoesPortais ++ posicoesTorres 
        naoRepetidos :: [Posicao] -> Bool 
        naoRepetidos [] = True 
        naoRepetidos (h:t)= h `notElem` t && naoRepetidos t 
    
-- |Função auxiliar que vai verificar se as posicoes dos portais e das torres se encontram no centro de um quadrado do mapa 
validaPosicoes :: [Posicao] -> Bool
validaPosicoes posicoes = 
    all (\(x,y) -> x- fromInteger (floor x) == 0.5 && y- fromInteger (floor y) == 0.5) posicoes 

-- |Função auxiliar que verifica uma lista de posições esta dentro do mapa 
estaDentro :: [Posicao] -> Mapa -> Bool 
estaDentro l mapa = 
  all (\(x,y)-> x>= 0.5 && y >= 0.5 && x<= xmax && y<= ymax) l
  where xmax = fromIntegral (length (head mapa)) - 0.5
        ymax = fromIntegral (length mapa) -0.5


-- |Função que vai verificar se apenas uma onda está ativa por portal
apenasUmaOndaAtiva :: [Portal] -> Bool 
apenasUmaOndaAtiva portais = all umaPorPortal portais 
        where 
            umaPorPortal :: Portal -> Bool 
            umaPorPortal p = length (filter (\onda -> entradaOnda onda <= 0)  (ondasPortal p)) <=1


-- |Função que verifica se os inimigos por lançar têm a posição do respetivo portal, nı́vel de vida positivo, e lista de projéteis ativos vazia
validaInimigos :: [Portal] -> Bool
validaInimigos portais = all validaPortal portais
  where
    -- Verifica todos os inimigos de um portal
    validaPortal :: Portal -> Bool
    validaPortal portal = all (validaOnda (posicaoPortal portal) )(ondasPortal portal)
    
    -- Verifica todos os inimigos de uma onda
    validaOnda :: Posicao -> Onda -> Bool
    validaOnda posPortal onda = all (validaInimigo posPortal) (inimigosOnda onda) 
    
    -- Verifica se um inimigo é válido
    validaInimigo :: Posicao -> Inimigo -> Bool
    validaInimigo posPortal inimigo =
      posicaoInimigo inimigo == posPortal && 
      vidaInimigo inimigo > 0 &&             
      null (projeteisInimigo inimigo)        


-- | Verifica se todos os inimigos em jogo estão sobre terrenos de terra ou cimento 
validaInimigosSobreTerraOuCimento :: Mapa -> [Inimigo] -> Bool
validaInimigosSobreTerraOuCimento mapa inimigos = all (eTerraOuCimento mapa) posicaoInimigos
    where posicaoInimigos = map posicaoInimigo inimigos 

-- | Converte uma posição em coordenadas (Float, Float) para índices inteiros (linha, coluna).
posicaoParaIndice :: Posicao -> (Int, Int)
posicaoParaIndice (x, y) = (floor x, floor y)

-- | Verifica se uma posição está sobre o terreno de terra.
eTerra :: Mapa -> Posicao -> Bool
eTerra mapa posicao =
      x >= 0 && x < length  (head mapa) &&  
      y >= 0 && y < length   mapa &&  
      ((mapa !! y) !! x == Terra)  
         where (x, y) = posicaoParaIndice posicao

-- | Verifica se uma posição está sobre o terreno de terra ou cimento.
eTerraOuCimento :: Mapa -> Posicao -> Bool
eTerraOuCimento mapa posicao =
      x >= 0 && x < length  (head mapa) &&  
      y >= 0 && y < length   mapa &&  
      (((mapa !! y) !! x == Terra) ||((mapa !! y) !! x == Cimento))
         where (x, y) = posicaoParaIndice posicao

-- | Verifica se uma posição está sobre o terreno de cimento.
eCimento :: Mapa -> Posicao -> Bool
eCimento mapa posicao =
      x >= 0 && x < length  (head mapa) &&  
      y >= 0 && y < length   mapa &&  
      ((mapa !! y) !! x == Cimento)  
         where (x, y) = posicaoParaIndice posicao




-- |Função que verifica se os inimigo não estão sobrepostos a uma torre 
validaInimigosSemSobreposicao :: [Inimigo] -> [Torre] -> Bool
validaInimigosSemSobreposicao inimigos torres = all inimigoNaoSobreposto inimigos
  where
    -- Função que verifica se o inimigo não está sobreposto a nenhuma torre
    inimigoNaoSobreposto :: Inimigo -> Bool
    inimigoNaoSobreposto inimigo = all (\torre -> posicaoInimigo inimigo /= posicaoTorre torre) torres

 

-- |Função que valida se a velocidade dos inimigos é não negativa
validaVelocidadeInimigos :: [Inimigo] -> Bool
validaVelocidadeInimigos inimigos = all inimigoComVelocidadeValida inimigos
  where
    -- Função que verifica se a velocidade do inimigo é maior ou igual a 0
    inimigoComVelocidadeValida :: Inimigo -> Bool
    inimigoComVelocidadeValida inimigo = velocidadeInimigo inimigo >= 0


 
-- |Função que valida se os inimigos em jogo tem os projéteis válidos 
validaProjeteisAtivos:: [Inimigo] -> Bool
validaProjeteisAtivos inimigos = all validaProjeteisInimigo inimigos  


-- |Função auxiliar que valida os projeteis para um inimigo 
validaProjeteisInimigo :: Inimigo -> Bool  
validaProjeteisInimigo  i = saoUnicos tiposProjeteis && semFogoResina tiposProjeteis && semFogoGelo tiposProjeteis
     where projeteis = projeteisInimigo i
           tiposProjeteis = map tipoProjetil projeteis 

-- |Função auxiliar que verifica se não há projéteis duplicados do mesmo tipo
saoUnicos :: [TipoProjetil] -> Bool
saoUnicos [] = True
saoUnicos  (x:xs) = not (x `elem` xs) && saoUnicos xs

-- |Função auxiliar  que verifica se existem na mesma lista de projeteis simultaneamente Fogo e Resina
semFogoResina :: [TipoProjetil] -> Bool 
semFogoResina tipos = not (Fogo `elem` tipos  && Resina `elem` tipos)

-- |Função auxiliar que verifica se existem na mesma lista de projeteis simultaneamente Fogo e Gelo
semFogoGelo :: [TipoProjetil] -> Bool 
semFogoGelo tipos = not (Fogo `elem` tipos && Gelo `elem` tipos)






-- |Função que verifica uma dada posicao no mapa é relva
eRelva :: Mapa -> Posicao -> Bool
eRelva mapa posicao =
      x >= 0 && x < length  (head mapa) &&  
      y >= 0 && y < length   mapa &&  
      ((mapa !! y) !! x == Relva)  
         where (x, y) = posicaoParaIndice posicao

-- |Função que verifica se as torres estão sobre a relva 
verificaTorre :: Mapa -> [Torre] -> Bool
verificaTorre  mapa torres = all (eRelva mapa) posTorres 
  where posTorres = map posicaoTorre torres 

-- |Função que verifica se o alcance e o dano das torres é um valor positivo 
alcanceEDanoPositivo :: [Torre] -> Bool
alcanceEDanoPositivo torres = all (>0) alcances && all (>0) danos  
  where alcances = map alcanceTorre torres
        danos = map danoTorre torres

-- |Função que verifica se as rajadas das torres são um valor positivo 
rajadaPositiva :: [Torre] -> Bool
rajadaPositiva torres = all (>0) rajadas 
  where rajadas = map rajadaTorre torres 

-- |Função que verifica se um ciclo das torres é um valor não negativo. 
cicloNaoNegativo :: [Torre] -> Bool
cicloNaoNegativo torres = all (>= 0) ciclos 
  where ciclos = map cicloTorre torres


-- |Função que verifica se a base está sobre terra
baseSobreTerra :: Base -> Mapa -> Bool 
baseSobreTerra base mapa = eTerra mapa (posicaoBase base)

-- |Função que verifica se os créditos da base são não negativos
creditosNaoNegativos :: Base -> Bool 
creditosNaoNegativos base = (creditosBase base) >= 0 


-- |Função que valida os pregos (se o número de pregos disponíveis é não negativo, se as posições dos pregos estão dentro do mapa, se estão sobre a terra (e nao sobrepostos a portais e à base) e se a vida é não negativa )
validaPregos :: Base -> [Portal] -> Mapa -> [Prego] -> Bool  
validaPregos base portais mapa pregos  = (pregosBase base) >= 0 && all pregoValido pregos 
  where pregoValido :: Prego -> Bool 
        pregoValido p = estaDentro [posicaoPrego p] mapa && vidaPrego p >= 0 && eTerra mapa (posicaoPrego p) && (posicaoPrego p ) `notElem` (posicaoBase base : map posicaoPortal portais)

-- |Função que valida o boost (se a velocidade sem boost é inferior à velocidade com boost, se a duracao do boost é positiva e se o número de boosts disponíveis e não negativo)
validaBoost :: Boost -> Bool 
validaBoost boost = velocidadeSBoost boost < velocidadeCBoost boost && duracaoBoost boost > 0 && numeroBoost boost >= 0


