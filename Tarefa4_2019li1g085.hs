-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g085 where

import LI11920
import Tarefa0_2019li1g085
import Tarefa2_2019li1g085

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9,tt10,tt11,tt12,tt13,tt14,tt15,tt16,tt17,tt18]
tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9,tt10,tt11,tt12,tt13,tt14,tt15,tt16,tt17,tt18 :: (Double,Mapa,Jogador)

tt1  = (1  ,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 0.5 3 (Ar 0.5 0 1 )))
tt2  = (1  ,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 2   3 (Ar 2   0 0 )))
tt3  = (1  ,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 0.9 3 (Ar 2   0 0 )))
tt4  = (1  ,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 0.5 3 (Chao False )))
tt5  = (1  ,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 2   3 (Chao False )))
tt6  = (1  ,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 0.9 3 (Chao False )))
tt7  = (1  ,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 0.5 3 (Chao True  )))
tt8  = (1  ,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 2   3 (Chao True  )))
tt9  = (1  ,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 0.9 3 (Chao True  )))
tt10 = (0.5,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 0   3 (Morto 0.3  )))
tt11 = (0.5,[[Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Terra 0]]   ,(Jogador 0 2 0   3 (Morto 1    )))
tt12 = (1  ,[[Recta Terra 0, Recta Terra 0,Rampa Relva 0 1,Recta Terra 1]] ,(Jogador 0 2 0.5 3 (Ar 0.5 15 1)))
tt13 = (1  ,[[Recta Terra 0, Recta Relva 0,Recta Relva 0,Recta Relva 0]]   ,(Jogador 0 2 0.5 3 (Chao True  )))
tt14 = (1  ,[[Recta Terra 0, Recta Lama  0,Recta Lama  0,Recta Lama  0]]   ,(Jogador 0 2 0.5 3 (Chao True  )))
tt15 = (1  ,[[Recta Terra 0, Recta Boost 0,Recta Boost 0,Recta Boost 0]]   ,(Jogador 0 2 2 3   (Chao True  )))
tt16 = (1  ,[[Recta Terra 0, Recta Cola  0,Recta Cola  0,Recta Cola  0]]   ,(Jogador 0 2 0.5 3 (Chao True  )))
tt17 = (1  ,[[Recta Terra 0, Recta Terra 0,Rampa Relva 0 1,Recta Terra 1]] ,(Jogador 0 2 0.5 3 (Ar 0.5 75 1)))
tt18 = (1  ,[[Recta Terra 0, Recta Terra 0,Rampa Relva 0 1,Recta Terra 1]] ,(Jogador 0 2 0.5 3 (Ar 0.5 (-75) 1)))


-- * Funções principais da Tarefa 4.


-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double  -- ^ O tempo decorrido.
      -> Mapa    -- ^ O mapa utilizado.
      -> Jogador -- ^ O estado anterior do 'Jogador'.
      -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.

passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double  -- ^ O tempo decorrido.
        -> Mapa    -- ^ O mapa utilizado.
        -> Jogador -- ^ O estado anterior do 'Jogador'.
        -> Jogador -- ^ O estado do 'Jogador' após acelerar.

acelera t m (Jogador p d v c (Ar a i g))  = (Jogador p d (v'A (Jogador p d v c (Ar a i g)) t) c (Ar a i (g' (Jogador p d v c (Ar a i g)) t)))
acelera t m (Jogador p d v c (Morto n))   = (Jogador p d v c (Morto n))
acelera t m (Jogador p d v c e)           = (Jogador p d (v'C m (Jogador p d v c e) t) c e)

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double  -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.

move t m j | morre && t <  cooldown   = decrementaMorto t j
           | morre && t >= cooldown   = reviveMorto j
           | chao  && fimDaPeca t m j = limitePeca j
           | chao                     = modPosicao t m j
           | colide t m j             = aterrar t m j
           | fimPecaAr t j            = limitePecaAr t j
           | otherwise                = modPosicaoAr t j
               where morre = verificaMorto j
                     cooldown = tempoMorto j
                     chao = verificaChao 0 [j]


-- ** Funções relativas à 'acelera'


-- | Mantém a velocidade final positiva ou com valor 0.
v'C :: Mapa    -- ^ 'Mapa' atual.
    -> Jogador -- ^ 'Jogador' a modificar.
    -> Double  -- ^ Tempo.
    -> Double  -- ^ Valor da Velocidade.

v'C m j t   | 0 <= valor = valor
            | otherwise = 0 
               where valor = calculav' m j t

-- | Calcula o valor da velocidade final (no 'Chao')
calculav' :: Mapa    -- ^ 'Mapa' atual.
          -> Jogador -- ^ 'Jogador' a modificar.
          -> Double  -- ^ Tempo.
          -> Double  -- ^ Valor da velocidade.

calculav' m j t = (velocidade + ((accelMota j - (atrito (getPiso (encontraPosicaoMatriz (getPista j , getIndicePeca j ) m))) * velocidade) * t))
                          where velocidade = getVelocidade j 

-- | Verifica se é possivel acelerar.
accelMota :: Jogador -- ^ 'Jogador' a verificar.
          -> Double  -- ^ Possivel acelerar?.

accelMota (Jogador _ _ v _ (Chao True)) | v < 2 = 1
accelMota _ = 0

-- | Devolve a velocidade do 'Jogador'.
getVelocidade :: Jogador -- ^ 'Jogador' a verificar.
              -> Double  -- ^ 'velocidadeJogador' correspondente.

getVelocidade (Jogador _ _ v _ _) = v

-- | Devolve o valor do atrito de cada 'Piso'.
atrito :: Piso   -- ^ 'Piso' a verificar.
       -> Double -- ^ Valor do atrito.

atrito p = case p of Terra -> 0.25
                     Relva -> 0.75
                     Lama  -> 1.50
                     Boost -> (-0.5)
                     Cola  -> 3.00

-- | Devolve o 'Piso' de uma 'Peca'.
getPiso :: Peca -- ^ 'Peca' a verificar.
        -> Piso -- ^ 'Piso' correspondente.

getPiso (Rampa p ai af) = p
getPiso (Recta p a)     = p

-- | Calcula o valor da velocidade final (No 'Ar')
v'A :: Jogador -- ^ 'Jogador' a modificar.
    -> Double  -- ^ Tempo.
    -> Double  -- ^ Valor da velocidade final.

v'A j t | 0 <= valor = valor
        | otherwise = 0
           where valor = getVelocidade j - (0.125 * getVelocidade j * t)


-- | Calcula o novo valor da velocidade da gravidade.
g' :: Jogador -- ^ 'Jogador'.
   -> Double  -- ^ Tempo.
   -> Double  -- ^ Novo valor da gravidade.

g' j t = getGravidade j + t

-- | Devolve a velocidade da gravidade num 'Jogador'.
getGravidade :: Jogador -- ^ 'Jogador'.
             -> Double  -- ^ 'gravidadeJogador'.

getGravidade (Jogador p d v c (Ar _ _ g)) = g


-- ** Funções relativas à 'move' em 'Morto'.


-- | Verifca se um 'Jogador' está 'Morto'.
verificaMorto :: Jogador -- ^ 'Jogador' a veriicar.
              -> Bool    -- ^ 'Jogador' está 'Morto'?

verificaMorto (Jogador _ _ _ _ (Morto _)) = True
verificaMorto _ = False

-- | Devolve o valor do tempo 'Morto' restante.
tempoMorto :: Jogador -- ^ 'Jogador' a verificar.
           -> Double  -- ^ Valor do tempo 'Morto'.

tempoMorto (Jogador _ _ _ _ (Morto i)) = i

-- | Remove o Tempo que decorreu num 'Jogador' 'Morto' (Tempo Decorrido < Tempo Morto).
decrementaMorto :: Double  -- ^ Tempo.
                -> Jogador -- ^ 'Jogador' 'Morto'.
                -> Jogador -- ^ 'Jogador' após o Tempo.

decrementaMorto t (Jogador p d v c (Morto i)) = (Jogador p d v c (Morto (i-t)))

-- | Revive um 'Jogador'.
reviveMorto :: Jogador -- ^ 'Jogador' 'Morto'.
            -> Jogador -- ^ 'Jogador' revivido.

reviveMorto (Jogador p d v c _) = Jogador p d v c (Chao False)


-- ** Funções relativas à 'move' no 'Chao'.


-- | Verifica se um 'Jogador' ultrapassa a 'Peca'.
fimDaPeca  :: Double  -- ^ Tempo.
           -> Mapa    -- ^ 'Mapa' atual.
           -> Jogador -- ^ 'Jogador' a verificar.
           -> Bool    -- ^ Ultrapassou a 'Peca'?

fimDaPeca t m j = (distancia2indice posicao) 
                   <
                  (distancia2indice (posicao + t * (getVelocidade j) * (cos(inclinacaoPeca (encontraPosicaoMatriz (getPista j , getIndicePeca j) m)))))

                   where posicao = getPosicao j

-- | Devolve a posicao de um 'Jogador'.
getPosicao :: Jogador -- ^ 'Jogador' a verificar.
           -> Double  -- ^ 'distanciaJogador' correspondente.

getPosicao (Jogador _ d _ _ _) = d

-- | Modifica a posicao de um 'Jogador' que ultrapassa a 'Peca' atual.
limitePeca :: Jogador -- ^ 'Jogador' a modificar.
           -> Jogador -- ^ 'Jogador' modificado.

limitePeca (Jogador p d v c e) = (Jogador p (fromIntegral(distancia2indice d)+1) v c e)

-- | Modifica a posicao de um 'Jogador' que se mantém na 'Peca' atual.
modPosicao :: Double  -- ^ Tempo.
           -> Mapa    -- ^ 'Mapa' atual.
           -> Jogador -- ^ 'Jogador' a modificar.
           -> Jogador -- ^ 'Jogador' modificado.

modPosicao t m (Jogador p d v c e) = (Jogador p ( d+t * v * (cos(inclinacaoPeca (encontraPosicaoMatriz (p, distancia2indice d) m)))) v c e)


-- ** Funções relativas à 'move' no 'Ar'


-- | Devolve o valor da inclinação de uma 'Peca' em radianos.
inclinacaoPeca :: Peca   -- ^ 'Peca' a veriicar.
               -> Double -- ^ Valor da inclinação.

inclinacaoPeca (Recta _ _) = 0
inclinacaoPeca (Rampa _ ai af) = (atan (declive ai af))

-- | Verifica se o 'Jogador' sobrevive a uma colisao com o 'Chao'.
sobrevive :: Mapa    -- ^ 'Mapa' atual.
          -> Jogador -- ^ 'Jogador' a verificar.
          -> Bool    -- ^ Sobrevive?

sobrevive m (Jogador p d _ _ (Ar _ i _ )) = 45 >= abs (radianos2graus((inclinacaoPeca (encontraPosicaoMatriz (p, distancia2indice d) m)))-i)

-- | Modifica a posição do 'Jogador' (Colide com o 'Chao').
aterrar :: Double  -- ^ Tempo.
        -> Mapa    -- ^ 'Mapa' atual.
        -> Jogador -- ^ 'Jogador' a modificar.
        -> Jogador -- ^ 'Jogador' modificado.

aterrar t m (Jogador p d v c (Ar a i g)) | sobrevive m (Jogador p d v c (Ar a i g)) = (Jogador p 
                                                                                               posicao
                                                                                               (v*cos((graus2radianos i) - inclinacaoPeca (encontraPosicaoMatriz (p, distancia2indice d) m)))
                                                                                               c 
                                                                                               (Chao False))
                                         | otherwise                                = (Jogador p 
                                                                                               posicao 
                                                                                               0 
                                                                                               c 
                                                                                               (Morto 1))

                                           where posicao = (posicaoX(posicaoColisao t m (Jogador p d v c (Ar a i g))))

-- | Devolve a Inclinação de um 'Jogador'.
getInclinacao :: Jogador -- ^ 'Jogador' a verificar.
              -> Double  -- ^ 'inclinacaoJogador' correspondente.

getInclinacao (Jogador _ _ _ _ (Ar _ i _)) = i

-- | Devolve a Altura a que se encontra um 'Jogador'.
getAltura :: Jogador -- ^ 'Jogador' a verificar.
          -> Double  -- ^ 'alturaJogador' correspondente.
          
getAltura (Jogador _ _ _ _ (Ar a _ _)) = a

-- | Devolve o vetor correspondente à velocidade (v*t).
vetorVelocidade :: Double  -- ^ Tempo.
                -> Jogador -- ^ 'Jogador'.
                -> Reta    -- ^ Vetor.

vetorVelocidade t j = ((Cartesiano (getPosicao j) (getAltura j)) ,
                       (Cartesiano ((getPosicao j) + (getVelocidade j * t * cos(graus2radianos(getInclinacao j)))) ((getAltura j) - (getGravidade j)*t)))

-- | Devolve o vetor correspondente à 'Peca' atual.
vetorPecaAtual :: Mapa    -- ^ 'Mapa' atual.
               -> Jogador -- ^ 'Jogador' a verificar.
               -> Reta    -- ^ Vetor.

vetorPecaAtual mapa (Jogador p d _ _ _) = ((Cartesiano (fromIntegral(truncate(d)))   (fromIntegral(alturaInicial pecaAtual))),
                                           (Cartesiano (fromIntegral(truncate(d+1))) (fromIntegral(alturaFinal   pecaAtual))))
                                            where pecaAtual = (encontraPosicaoMatriz (p , distancia2indice d) mapa )

-- | Verifica se um 'Jogador' colide com o 'Chao'.
colide :: Double  -- ^ Tempo.
       -> Mapa    -- ^ 'Mapa' atual.
       -> Jogador -- ^ 'Jogador' a verificar.
       -> Bool    -- ^ Colide?

colide t m j = intersetam (vetorVelocidade t j) (vetorPecaAtual m j)

-- | Determina o ponto de colisão do 'Jogador' com a 'Peca'.
posicaoColisao :: Double  -- ^ Tempo.
               -> Mapa    -- ^ 'Mapa' atual.
               -> Jogador -- ^ 'Jogador' a verificar.
               -> Ponto   -- ^ 'Ponto' de colisão.

posicaoColisao t m j = intersecao (vetorVelocidade t j) (vetorPecaAtual m j)

-- | Devolve o valor de x num ponto Cartesiano.
posicaoX :: Ponto  -- ^ 'Ponto'.
         -> Double -- ^ Valor de x.

posicaoX (Cartesiano x y) = x

-- | Devolve o valor de y num ponto Cartesiano.
posicaoY :: Ponto  -- ^ 'Ponto'.
         -> Double -- ^ Valor de y.
posicaoY (Cartesiano x y) = y

-- | Verifica se o 'Jogador' ultrapassa a 'Peca' atual (no 'Ar').
fimPecaAr :: Double  -- ^ Tempo.
          -> Jogador -- ^ 'Jogador' a verificar.
          -> Bool    -- ^ Ultrapassa?

fimPecaAr t j = posicaoX (snd(vetorVelocidade t j)) >= fromIntegral (truncate(getPosicao j)+1)

-- | Altera a posicao do 'Jogador' para o inicio da proxima 'Peca'.
limitePecaAr :: Double  -- ^ Tempo.
             -> Jogador -- ^ 'Jogador' a modificar.
             -> Jogador -- ^ 'Jogador' modificado.

limitePecaAr t (Jogador p d v c (Ar a i g)) = (Jogador p (fromIntegral(truncate (d+1))) v c (Ar (posicaoY(posicaoPecaAr t (Jogador p d v c (Ar a i g)))) i g))

-- | Devolve a posição final do 'Jogador' caso ultrapasse a 'Peca' atual e não colida com o 'Chao'.
posicaoPecaAr :: Double  -- ^ Tempo.
              -> Jogador -- ^ 'Jogador' a verificar.
              -> Ponto   -- ^ Ponto final.

posicaoPecaAr t j = intersecao (vetorVelocidade t j) (vetorProximaPeca t j)

-- | Vetor do limite final da 'Peca' atual.
vetorProximaPeca :: Double  -- ^ Tempo.
                 -> Jogador -- ^ 'Jogador' a verificar.
                 -> Reta    -- ^ Vetor da 'Peca'.

vetorProximaPeca t (Jogador p d v c (Ar a i g)) = ((Cartesiano (fromIntegral( truncate d)+1)     0),
                                                   (Cartesiano (fromIntegral((truncate d)+1)) (a-((g*t-velocidadeY t v i)))))

-- | Devolve o valor da velocidade no eixo X (horizontal) do 'Jogador'.
velocidadeX :: Double -- ^ Tempo.
            -> Double -- ^ Velocidade.
            -> Double -- ^ Inclinacao da mota.
            -> Double -- ^ Diferença no eixo X.

velocidadeX t v i = (v*t*cos(graus2radianos i))

-- | Devolve o valor da velocidade no eixo Y (vertical) do 'Jogador'.
velocidadeY :: Double -- ^ Tempo.
            -> Double -- ^ Velocidade.
            -> Double -- ^ Inclinação da mota.
            -> Double -- ^ Diferença no eixo Y.

velocidadeY t v i = (v*t*sin(graus2radianos i)) 

-- | Modifica a posicao do 'Jogador' caso o movimento não seja suficiente para alcançar a proxima 'Peca' em colidir com o 'Chao'.
modPosicaoAr :: Double  -- ^ Tempo.
             -> Jogador -- ^ 'Jogador' a modificar.
             -> Jogador -- ^ 'Jogador' modificado.

modPosicaoAr t (Jogador p d v c (Ar a i g)) = (Jogador p (d+velocidadeX t v i) v c (Ar (a-((g*t-velocidadeY t v i))) i g))

