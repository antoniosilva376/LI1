-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g085 where

import LI11920
import Tarefa0_2019li1g085

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)] -- ^ triplo de teste
testesT2 = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30]



-- | Teste de Acelerar no Chao False.
t1 ::  (Int,Jogada,Estado)
t1 =  (0 , Acelera     , (Estado ([[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 0 1 0 (Chao False))]))
-- | Teste de Acelerar no Ar.
t2 ::  (Int,Jogada,Estado)
t2 =  (1 , Acelera     , (Estado ([[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 0 1 0 (Chao False)),(Jogador 1 0 1 0 (Ar 1 1 1))]))
-- | Teste de Disparar numa Rampa.
t3 ::  (Int,Jogada,Estado)
t3 =  (1 , Dispara     , (Estado ([[Recta Terra 0,Rampa Relva 0 2,Recta Relva 2],[Recta Terra 0,Rampa Boost 0 2,Rampa Cola 2 4]]) ([(Jogador 0 1 5 5 (Chao True)),(Jogador 1 2 5 5 (Chao True))])))
-- | Teste de Disparar numa Recta.
t4 ::  (Int,Jogada,Estado) 
t4 =  (1 , Dispara     , (Estado ([[Recta Terra 0,Rampa Relva 0 2,Recta Lama 2],[Recta Terra 0,Recta Lama 0,Rampa Cola 0 2]]) ([(Jogador 0 1 5 5 (Chao True)),(Jogador 1 2 5 5 (Chao True))])))
-- | Teste de Disparar sem Disparos.
t5 ::  (Int,Jogada,Estado) 
t5 =  (1 , Dispara     , (Estado ([[Recta Terra 0,Rampa Relva 0 2,Recta Terra 2],[Recta Terra 0,Recta Cola 0,Rampa Cola 0 2]]) ([(Jogador 0 1 5 5 (Chao True)),(Jogador 1 2 5 0 (Chao True))])))
-- | Teste de Desacelerar no Chao False.
t6 ::  (Int,Jogada,Estado) 
t6 =  (0 , Desacelera  , (Estado ([[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 0 1 0 (Chao False))]))
-- | Teste de Desacelerar no Ar.
t7 ::  (Int,Jogada,Estado) 
t7 =  (1 , Desacelera  , (Estado ([[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 1 1 3 (Chao False)),(Jogador 1 1 1 1 (Ar 1 1 1))]))
-- | Teste de Movimenta D no Ar (Inclinação 0).
t8 ::  (Int,Jogada,Estado) 
t8 =  (1 , Movimenta D , (Estado ([[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 1 4 2 (Chao True)),(Jogador 0 1 1 3 (Ar 1 0 1))]))
-- | Teste de Movimenta E no Ar (Inclinação 0).
t9 ::  (Int,Jogada,Estado) 
t9 =  (1 , Movimenta E , (Estado ([[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 1 4 2 (Chao True)),(Jogador 0 1 1 3 (Ar 1 0 1))]))
-- | Teste de Movimenta D no Ar (Inclinação -90).
t10 :: (Int,Jogada,Estado) 
t10 = (1 , Movimenta D , (Estado ([[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 1 4 2 (Chao True)),(Jogador 0 1 1 3 (Ar 1 (-90) 1))]))
-- | Teste de Movimenta D no Ar (Inclinação 90).
t11 :: (Int,Jogada,Estado) 
t11 = (1 , Movimenta E , (Estado ([[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 1 4 2 (Chao True)),(Jogador 0 1 1 3 (Ar 1 90 1))]))
-- | Teste de Movimenta D no Chao.
t12 :: (Int,Jogada,Estado) 
t12 = (0 , Movimenta D , (Estado ([[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 1 4 2 (Chao True))]))
-- | Teste de Movimenta E no Chao.
t13 :: (Int,Jogada,Estado) 
t13 = (0 , Movimenta E , (Estado ([[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 1 4 2 (Chao True))]))
-- |Teste de Movimenta B (No limite).
t14 :: (Int,Jogada,Estado) 
t14 = (0 , Movimenta B , (Estado ([[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 1 4 2 (Chao True))]))
-- | Teste de Movimenta C (No limite).
t15 :: (Int,Jogada,Estado)
t15 = (0 , Movimenta C , (Estado ([[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 1 4 2 (Chao True))]))
-- | Teste de Movimenta B no Chao (mesma altura).
t16 :: (Int,Jogada,Estado)
t16 = (0 , Movimenta B , (Estado ([[Recta Terra 0,Rampa Terra 0 2,Recta Boost 2],[Recta Terra 0,Rampa Boost 0 2,Rampa Boost 2 4]]) ([(Jogador 0 1 5 5 (Chao True)),(Jogador 1 2 5 5 (Chao True))])))
-- | Teste de Movimenta C no Chao (mesma altura).
t17 :: (Int,Jogada,Estado)
t17 = (1 , Movimenta C , (Estado ([[Recta Terra 0,Rampa Lama 0 2,Recta Boost 2],[Recta Terra 0,Rampa Relva 0 2,Rampa Boost 2 4]]) ([(Jogador 0 1 5 5 (Chao True)),(Jogador 1 2 5 5 (Chao True))])))
-- | Teste de Movimenta B no Chao (Jogador cai/salta).
t18 :: (Int,Jogada,Estado) 
t18 = (0 , Movimenta B , (Estado ([[Recta Terra 0,Rampa Relva 0 2,Recta Boost 2],[Recta Terra 0,Recta Lama 0,Rampa Boost 0 2]]) ([(Jogador 0 2 5 5 (Chao True)),(Jogador 1 2 5 5 (Chao True))])))
-- | Teste de Movimenta C no Chao (Jogador cai/salta).
t19 :: (Int,Jogada,Estado) 
t19 = (1 , Movimenta C , (Estado ([[Recta Terra 0,Recta Terra 0,Rampa Boost 0 2],[Recta Terra 0,Rampa Relva 0 2,Rampa Boost 2 4]]) ([(Jogador 0 2 5 5 (Chao True)),(Jogador 1 2 5 5 (Chao True))])))
-- | Teste de Movimenta B no Chao (Jogador cai/morre).
t20 :: (Int,Jogada,Estado) 
t20 = (0 , Movimenta B , (Estado ([[Recta Terra 0,Recta Terra 0,Rampa Boost 0 2],[Recta Terra 0,Rampa Boost 0 2,Rampa Relva 2 4]]) ([(Jogador 0 2 5 5 (Chao True)),(Jogador 1 2 5 5 (Chao True))])))
-- | Teste de Movimenta C no Chao (Jogador cai/morre).
t21 :: (Int,Jogada,Estado) 
t21 = (1 , Movimenta C , (Estado ([[Recta Terra 0,Rampa Boost 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Rampa Boost 0 2]]) ([(Jogador 0 2 5 5 (Chao False)),(Jogador 1 2 5 5 (Chao False))])))
-- | Teste de Disparar na primeira Peca do Mapa.
t22 :: (Int,Jogada,Estado) 
t22 = (0 , Dispara     , (Estado ([[Recta Terra 0,Recta Relva 0],[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 0 1 1 (Chao False))]))
-- | Teste de Movimenta B.
t23 :: (Int,Jogada,Estado) 
t23 = (1 , Movimenta B , (Estado ([[Recta Terra 0,Recta Lama 0,Rampa Boost 0 2],[Recta Terra 0,Rampa Lama 0 2,Rampa Boost 2 4],[Recta Terra 0,Rampa Boost 0 2,Rampa Boost 2 4]]) ([(Jogador 0 2 4 3 (Chao True)),(Jogador 1 2 5 6 (Chao True))])))
-- | Teste de Movimenta B (Jogador Morto).
t24 :: (Int,Jogada,Estado) 
t24 = (1 , Movimenta B , (Estado ([[Recta Terra 0,Recta Terra 0,Rampa Boost 0 2],[Recta Terra 0,Rampa Boost 0 2,Rampa Lama 2 4],[Recta Terra 0,Rampa Boost 0 2,Rampa Boost 2 4]]) ([(Jogador 0 2 4 3 (Chao True)),(Jogador 1 2 5 6 (Morto 1))])))
-- | Teste de Disparo numa Peca que já é Cola.
t25 :: (Int,Jogada,Estado) 
t25 = (1 , Dispara     , (Estado ([[Recta Terra 0,Rampa Relva 0 2,Recta Boost 2],[Recta Terra 0,Rampa Cola 0 2,Rampa Boost 2 4]]) ([(Jogador 0 1 4 6 (Chao True)),(Jogador 1 2.3 5 3 (Chao True))])))
-- | Teste de Movimenta B (Morto).
t26 :: (Int,Jogada,Estado) 
t26 = (0 , Movimenta B , (Estado ([[Recta Terra 0,Rampa Relva 0 2,Recta Relva 2],[Recta Terra 0,Rampa Boost 0 2,Rampa Lama 2 4]]) ([(Jogador 0 1 4 6 (Morto 1)),(Jogador 1 2 5 3 (Chao True))])))
-- | Teste de Movimenta C (Ar).
t27 :: (Int,Jogada,Estado) 
t27 = (0 , Movimenta C , (Estado ([[Recta Terra 0,Rampa Relva 0 2,Recta Boost 2],[Recta Terra 0,Rampa Boost 0 2,Rampa Relva 2 4]]) ([(Jogador 0 1 4 6 (Chao True)),(Jogador 1 2 5 3 (Ar 5 5 5))])))
-- | Teste de Desacelerar no Chao True.
t28 :: (Int,Jogada,Estado)
t28 = (0 , Desacelera  , (Estado ([[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 0 1 0 (Chao True))]))
-- | Teste de Desacelerar no Chao True.
t29 :: (Int,Jogada,Estado)
t29 = (0 , Desacelera  , (Estado ([[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 0 1 0 (Chao True))]))
-- | Teste de Acelerar no Chao True.
t30 :: (Int,Jogada,Estado)
t30 =  (0 , Acelera     , (Estado ([[Recta Terra 0,Recta Terra 0]]) [(Jogador 0 0 1 0 (Chao True))]))
{-   Teste de Movimenta C (diferença de alturas exatamente = 0.2)
t31 :: (Int,Jogada,Estado)
t31 = (1 , Movimenta B , (Estado ([[Recta Terra 0,Rampa Relva 0 1,Recta Boost 1],[Recta Terra 0,Recta Boost 0,Rampa Relva 0 2]]) ([(Jogador 0 2.6 4 6 (Chao True)),(Jogador 1 2 5 3 (Ar 5 5 5))])))
-}





-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.

jogada ij Dispara (Estado mapa js) | (((encontraJogador (encontraIndiceLista ij js)) /= (getPista (encontraIndiceLista ij js),0))
                                        && (temCola (encontraIndiceLista ij js)))
                                               = (Estado (adicionaCola mapa (getPista (encontraIndiceLista ij js)) (getIndicePeca (encontraIndiceLista ij js))) (retiraCola ij js))
                                   | otherwise = (Estado mapa js)

jogada ij (Movimenta B) (Estado mapa js) | (verificaChao ij js) && (podeDescer (encontraIndiceLista ij js) mapa)
                                                     = (Estado mapa (modEstadoBaixo    ij js mapa))
                                         | otherwise = (Estado mapa js)

jogada ij (Movimenta C) (Estado mapa js) | (verificaChao ij js) && (podeSubir (encontraIndiceLista ij js))
                                                     = (Estado mapa (modEstadoCima     ij js mapa))
                                         | otherwise = (Estado mapa js)

jogada ij (Movimenta D) (Estado mapa js) = (Estado mapa (modEstadoDireita  ij js))

jogada ij (Movimenta E) (Estado mapa js) = (Estado mapa (modEstadoEsquerda ij js))

jogada ij Acelera (Estado mapa js)    = (Estado mapa (acelera'   ij js))

jogada ij Desacelera (Estado mapa js) = (Estado mapa (desacelera ij js))


-- ** Funções relativas a disparos

-- | Modifica uma 'Peca' depois de um 'Dispara'.

colocaCola :: Peca -- ^ 'Peca' que se pretende modificar.
           -> Peca -- ^ 'Peca' modifcada.

colocaCola (Rampa p a1 a2) = (Rampa Cola a1 a2) 
colocaCola (Recta p a)     = (Recta Cola a)

-- | Modifica o 'Mapa' colocando a 'Peca Cola' na respetiva posição (Reparar que na posicao inicial não é suposto alterar nada).

adicionaCola :: Mapa -- ^ 'Mapa' por modificar.
             -> Int  -- ^ 'Pista' que se encontra o 'Jogador'.
             -> Int  -- ^ 'distanciaJogador'.
             -> Mapa -- ^ 'Mapa' modificado.

adicionaCola mapa pistaJogador distanciaJogador = atualizaPosicaoMatriz (pistaJogador , (distanciaJogador)-1)
                                                                        (colocaCola (encontraIndiceLista ((distanciaJogador)-1) 
                                                                                                         (encontraIndiceLista pistaJogador mapa)))
                                                                        mapa

-- | Retira uma 'Cola' após um 'Dispara'.

retiraCola :: Int       -- ^ Indice do 'Jogador'.
           -> [Jogador] -- ^ Lista de 'Jogador' atual.
           -> [Jogador] -- ^ Lista dos 'Jogador' após um 'Dispara'.

retiraCola 0 ((Jogador p d v colaJogador e):js) = (Jogador p d v (colaJogador-1) e) : js
retiraCola x ((Jogador p d v colaJogador e):js) = (Jogador p d v (colaJogador) e)   : retiraCola (x-1) js

-- | Verifica se o 'Jogador' se pode efetuar um 'Dispara'.

temCola :: Jogador -- ^ 'Jogador' que 'Dispara'.
        -> Bool    -- ^ Pode efetuar um 'Dispara'?

temCola (Jogador p d v colaJogador e) | colaJogador > 0 = True
                                      | otherwise       = False


-- ** Funções relativas aos movimentos horizontais

-- | Altera a 'inclinacaoJogador' (Movimenta E).

esquerda :: EstadoJogador -- ^ 'EstadoJogador' do 'Jogador' que 'Movimenta E'.
         -> EstadoJogador -- ^ 'EstadoJogador' após 'Movimenta E'.

esquerda (Ar a inclinacaoJogador g) | ((inclinacaoJogador+15) <  90) = (Ar a (inclinacaoJogador+15) g)
                                    | ((inclinacaoJogador+15) >= 90) = (Ar a 90 g)
esquerda e = e

-- | Modifica o 'Estado Jogador' no jogador desejado ('Movimenta E').

modEstadoEsquerda :: Int       -- ^ Indice do 'Jogador' 'Movimenta E'.
                  -> [Jogador] -- ^ Lista de 'Jogador' atual.
                  -> [Jogador] -- ^ Lista dos 'Jogador' após um 'Movimenta E'.

modEstadoEsquerda 0 ((Jogador p d v c e):js) = (Jogador p d v c (esquerda e)) : js
modEstadoEsquerda n ((Jogador p d v c e):js) = (Jogador p d v c e)            : modEstadoEsquerda (n-1) js

-- | Altera a 'inclinacaoJogador' (Movimenta D).

direita :: EstadoJogador  -- ^ 'EstadoJogador' do 'Jogador' que 'Movimenta D'.
         -> EstadoJogador -- ^ 'EstadoJogador' após 'Movimenta D'.

direita (Ar a inclinacaoJogador g) | ((inclinacaoJogador-15) >  -90) = (Ar a (inclinacaoJogador-15) g)
                                   | ((inclinacaoJogador-15) <= -90) = (Ar a (-90) g)
direita e = e

-- | Modifica o 'Estado Jogador' no jogador desejado ('Movimenta D').

modEstadoDireita :: Int       -- ^ Indice do 'Jogador' 'Movimenta D'.
                 -> [Jogador] -- ^ Lista de 'Jogador' atual.
                 -> [Jogador] -- ^ Lista dos 'Jogador' após um 'Movimenta D'.

modEstadoDireita 0 ((Jogador p d v c e):js) = (Jogador p d v c (direita e)) : js
modEstadoDireita n (j:js) = j : modEstadoDireita (n-1) js


-- ** Funções relativas aos movimentos verticais

-- | Devolve o declive de uma 'Recta'/'Rampa' - Declive == (y1-y2)/1 pois x apenas altera 1 (peça em peça).
declive :: Int    -- ^ Altura inicial.
        -> Int    -- ^ Altura final.
        -> Double -- ^ Declive.
declive ai af | ai /= af   = fromIntegral (af-ai)
              | otherwise  = 0 

-- | Devolve a "percentagem" percorrida de uma 'Peca'.

percentagemPercorrida :: Double -- ^ 'distanciaJogador'.
                      -> Double -- ^ Fração da 'Peca' percorrida.

percentagemPercorrida r | r < 1 = r
                        | otherwise = percentagemPercorrida (r-1)

-- | Devolve a altura "atual" que depende da percentagem da 'Peca' já percorrida pelo 'Jogador'.

alturaPista :: Double -- ^ 'distanciaJogador'.
            -> Int    -- ^ Altura inicial da 'Peca'.
            -> Int    -- ^ Altura inicial da 'Peca'.
            -> Double -- ^ Altura "atual".

alturaPista d ai af = (fromIntegral ai) + ((percentagemPercorrida d) * (declive ai af))

-- | Devolve a altura inicial de uma 'Peca'.

alturaInicial :: Peca -- ^ 'Peca' analisada.
              -> Int  -- ^ Altura inicial.

alturaInicial (Rampa p ai af) = ai
alturaInicial (Recta p a) = a

-- | Devolve a altura final de uma 'Peca'.

alturaFinal :: Peca -- ^ 'Peca' analisada.
            -> Int  -- ^ Altura final.

alturaFinal (Rampa p ai af) = af
alturaFinal (Recta p a) = a

-- | Devolve o resultado de alterar de pista para cima ('Movimenta C').

sobePista :: Jogador -- ^ 'Jogador' que 'Movimenta C'.
          -> Mapa    -- ^ 'Mapa' atual.
          -> Jogador -- ^ 'Jogador' após o 'Movimenta C'.

sobePista (Jogador p d v c e) mapa   | abs diferenca <= 0.2 = (Jogador (p-1) d v c e) 

                                     |     diferenca >  0.2 = (Jogador (p-1) d v c (Ar (alturaAtual)     
                                                                                       (radianos2graus ( atan ( declive (alturaInicialAtual) (alturaFinalAtual))))
                                                                                       0)) 

                                     | otherwise = (Jogador p d 0 c (Morto 1))

                                            where alturaInicialAtual = alturaInicial (encontraPosicaoMatriz (p , distancia2indice d) mapa)
                                                  alturaFinalAtual   = alturaFinal   (encontraPosicaoMatriz (p , distancia2indice d) mapa)
                                                  alturaAtual        = alturaPista d alturaInicialAtual alturaFinalAtual

                                                  alturaInicialCima  = alturaInicial (encontraPosicaoMatriz ((p-1) , distancia2indice d) mapa)
                                                  alturaFinalCima    = alturaFinal   (encontraPosicaoMatriz ((p-1) , distancia2indice d) mapa)
                                                  alturaCima         = alturaPista d alturaInicialCima alturaFinalCima

                                                  diferenca = (alturaAtual - alturaCima)

-- | Modifica o 'Estado Jogador' no jogador desejado ('Movimenta C').

modEstadoCima :: Int       -- ^ Indice do 'Jogador' que 'Movimenta C'.
              -> [Jogador] -- ^ Lista de 'Jogador' atual.
              -> Mapa      -- ^ 'Mapa' atual.
              -> [Jogador] -- ^ Lista de 'Jogador' após um 'Movimenta C'.

modEstadoCima 0 (j:js) mapa = (sobePista j mapa) : js
modEstadoCima n (j:js) mapa = j : (modEstadoCima (n-1) js mapa)                                            

-- | Devolve o resultado de alterar de pista para baixo ('Movimenta B').

descePista :: Jogador -- ^ 'Jogador' que 'Movimenta B'.
           -> Mapa    -- ^ 'Mapa' atual.
           -> Jogador -- ^ 'Jogador' após o 'Movimenta B'.

descePista (Jogador p d v c e) mapa   | abs diferenca <= 0.2 = (Jogador (p+1) d v c e) 

                                      |     diferenca >  0.2 = (Jogador (p-1) d v c (Ar (alturaAtual)
                                                                                        (radianos2graus ( atan ( declive (alturaInicialAtual) (alturaFinalAtual))))
                                                                                        0))
                                      | otherwise = (Jogador p d 0 c (Morto 1))

                                            where alturaInicialAtual = alturaInicial (encontraPosicaoMatriz (p , distancia2indice d) mapa)
                                                  alturaFinalAtual   = alturaFinal   (encontraPosicaoMatriz (p , distancia2indice d) mapa)
                                                  alturaAtual        = alturaPista d alturaInicialAtual alturaFinalAtual

                                                  alturaInicialBaixo = alturaInicial (encontraPosicaoMatriz ((p+1) , distancia2indice d) mapa)
                                                  alturaFinalBaixo   = alturaFinal   (encontraPosicaoMatriz ((p+1) , distancia2indice d) mapa)
                                                  alturaBaixo        = alturaPista d alturaInicialBaixo alturaFinalBaixo

                                                  diferenca = (alturaAtual - alturaBaixo)

-- | Modifica o 'Estado Jogador' no jogador desejado ('Movimenta B').

modEstadoBaixo :: Int       -- ^ Indice do 'Jogador' que 'Movimenta B'.
               -> [Jogador] -- ^ Lista de 'Jogador' atual.
               -> Mapa      -- ^ 'Mapa' atual.
               -> [Jogador] -- ^ Lista de 'Jogador' após um 'Movimenta B'.

modEstadoBaixo 0 (j:js) mapa = (descePista j mapa) : js
modEstadoBaixo n (j:js) mapa = j : (modEstadoBaixo (n-1) js mapa)

-- | Modifica o 'Estado Jogador' no jogador desejado ('Acelera').

acelera' :: Int       -- ^ Indice do 'Jogador' que 'Acelera'.
        -> [Jogador] -- ^ Lista de 'Jogador' atual.
        -> [Jogador] -- ^ Lista de 'Jogador' após um 'Acelera'.

acelera' 0 ((Jogador p d v c e):js) | ((e == (Chao True)) || e == ((Chao False))) =  ((Jogador p d v c (Chao True)) : js)
                                   | otherwise                                   =  ((Jogador p d v c e)           : js)

acelera' n (j:js) = j : acelera' (n-1) js

-- | Modifica o 'Estado Jogador' no jogador desejado ('Desacelera').

desacelera :: Int       -- ^ Indice do 'Jogador' que 'Desacelera'.
           -> [Jogador] -- ^ Lista de 'Jogador' atual.
           -> [Jogador] -- ^ Lista de 'Jogador' após um 'Desacelera'.

desacelera 0 ((Jogador p d v c e):js) | ((e == (Chao True)) || e == ((Chao False))) =  ((Jogador p d v c (Chao False)) : js)
                                      | otherwise                                   =  ((Jogador p d v c e)            : js)

desacelera n (j:js) = j : desacelera (n-1) js

-- | Verifica se o 'Jogador' não se encontra na 1ª 'Pista'.

podeSubir :: Jogador -- ^ 'Jogador' analisado .
          -> Bool    -- ^ Pode 'Movimenta C'?

podeSubir (Jogador 0 d v c e) = False
podeSubir _                   = True

-- | Verifica se o 'Jogador' não se encontra na ultima 'Pista'.

podeDescer :: Jogador -- ^ 'Jogador' analisado.
           -> Mapa    -- ^ 'Mapa' atual.
           -> Bool    -- ^ Pode 'Movimenta B'?

podeDescer (Jogador p d v c e) mapa | ((length mapa)-1) == p = False
                                    | otherwise = True


-- ** Funções secundárias

-- | Verifica se o 'Jogador' está no 'Chao'.

verificaChao :: Int       -- ^ Indice do 'Jogador'.
             -> [Jogador] -- ^ Lista de 'Jogador'.
             -> Bool      -- ^ Está no 'Chao'?

verificaChao 0 ((Jogador p d v c e):js) = ((e == (Chao True)) || e == ((Chao False)))
verificaChao n (j:js)                   = verificaChao (n-1) js


-- | Dá a posição do jogador em forma de par ( nº da 'Pista' , nº da 'Peca' ).

encontraJogador :: Jogador   -- ^ 'Jogador' que pretende encontrar.
                -> (Int,Int) -- ^ Posicão encontrada do 'Jogador'.

encontraJogador (Jogador pistaJogador distanciaJogador v c e) = (pistaJogador , (distancia2indice distanciaJogador))

-- | Devolve a 'pistaJogador'.

getPista :: Jogador -- ^ 'Jogador' da 'Pista' que pretende encontrar.
         -> Int     -- ^ Numero da 'Pista' encontrada.
getPista (Jogador p d v c e) = p

-- | Devolve o indice da 'Peca' segundo o estado do 'Jogador'.

getIndicePeca :: Jogador -- ^ 'Jogador' que pretende avaliar.
               -> Int     -- ^ Indice da 'Peca' do 'Jogador'.

getIndicePeca (Jogador p d v c e) = distancia2indice d

-- | Devolve o indice da 'Peca' segundo a distancia a que 'Jogador' se encontra.

distancia2indice :: Double -- ^ 'distanciaJogador'.
                 -> Int    -- ^ Indice da 'Peca'.

distancia2indice x | x >= 1    = 1 + distancia2indice (x-1)
                   | otherwise = 0
 