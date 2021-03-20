{- |

= Introdução

Nesta tarefa, foi-nos proposta a criação de um bot que consuante as caracterias atuais do jogo, soubesse como e quando
atuar para produzir a melhor e mais humana resposta.

= Objetivos

O nosso objetivo passou por 3 simples casos:

  * Disparar caso o piso anterior fosse um boost, evitando assim que outro jogador utilize o melhor caminho possivel;

  * Encontrar o melhor caminho possivel, avaliando o menor atrito e seguindo esse caminho;

  * Evitar que o seu tempo no Ar o prejudique.

A implementação destes casos ficou aquém daquilo que desejavamos. No caso de encontrar o melhor caminho possivel, encontra
o primeiro caminho (topo do mapa para baixo), ao invés de encontrar o caminho mais perto. Na hipotese de melhorar o seu
o seu tempo no Ar, queriamos que utilizasse o melhor ângulo possivel para aproveitamento da velocidade, no entanto só evita
morrer de queda.

= Conclusão

Infelizmente é um bot muito primitivo e com pouca inteligência. As limitações anteriormente descritas tornam o bot, um bot
que não compete no topo. O seu ranking no torneio é mediano.
Concluindo, apesar de não termos alcançado os objetivos, foi uma tarefa, apesar de desafiante, divertida de se trabalhar e
permitiu-nos testar as nossas habilidades e obrigou-nos a pensar um pouco na forma como a inteligência artificial funciona,
uma área que pouco conheçemos.

-}

module Tarefa6_2019li1g085 where

import Tarefa0_2019li1g085
import Tarefa2_2019li1g085
import Tarefa4_2019li1g085
import LI11920

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.

bot i (Estado m js) | getPosicao indice >= 1 && verificaBoost m i js  = Just Dispara
                    | verificaChao    i js                            = melhorPista    m indice
                    | verificaAr      i js                            = verificaAngulo m indice
                    | otherwise                                       = Just Acelera

                       where indice = (encontraIndiceLista i js)


-- | Verifica se o 'Jogador' está no 'Ar'.
verificaAr :: Int       -- ^ Indentificar do 'Jogador'.
           -> [Jogador] -- ^ Lista de 'Jogador'.
           -> Bool      -- Está no 'Ar'?

verificaAr 0 ((Jogador _ _ _ _ (Ar _ _ _)):js) = True
verificaAr 0 _ = False
verificaAr n (j:js) = verificaAr (n-1) js

-- | Verifica se o jogador deve modificar a sua inclinação.
verificaAngulo :: Mapa         -- ^ 'Mapa' atual.
               -> Jogador      -- ^ 'Jogador' a verificar.
               -> Maybe Jogada -- ^ 'Jogada' resultante.

verificaAngulo m j | diferenca >  45 = Just (Movimenta D)
                   | diferenca < -45 = Just (Movimenta E)
                   | otherwise       = Nothing

                     where diferenca = (getInclinacao j) - (radianos2graus (inclinacaoPeca (encontraPosicaoMatriz (getPista j, getIndicePeca j) m)))

-- | Verifica se a 'Peca' anterior é um 'Boost'.
verificaBoost :: Mapa      -- ^ 'Mapa' atual.
              -> Int       -- ^ Identificador.
              -> [Jogador] -- ^ Lista de 'Jogador'.
              -> Bool      -- ^ É 'Boost'?.

verificaBoost m 0 (j:js) = ((getPiso (encontraPosicaoMatriz (pista,indice-1) m)) == Boost) && temCola j
                               where pista  = getPista j 
                                     indice = getIndicePeca j
verificaBoost m n (j:js) = verificaBoost m (n-1) js

-- | Devolve uma lista com os 'Piso' todos referentes à coluna atual.
listaPiso :: Mapa    -- ^ 'Mapa' atual.
          -> Jogador -- ^ 'Jogador' a verificar.
          -> [Piso]  -- ^ Lista de 'Piso'.

listaPiso m j = map getPiso (map (encontraIndiceLista (getIndicePeca j )) m)

-- | Devolve o menor atrito.
melhorAtrito :: Mapa    -- ^ 'Mapa' atual.
             -> Jogador -- ^ 'Jogador' a verificar.
             -> Double  -- ^ Melhor atrito.

melhorAtrito m j = minimum (map atrito (listaPiso m j))

-- | Devolve o indice da 'Pista' a que corresponde o melhor atrito (Devolve a primeira 'Peca', não a mais perto).
indiceMelhorAtrito :: Mapa    -- ^ 'Mapa' atual.
                   -> Jogador -- ^ 'Jogador' a verificar.
                   -> [Piso]  -- ^ Coluna de 'Piso'.
                   -> Int     -- ^ Indice.

indiceMelhorAtrito m j (x:xs) | (melhorAtrito m j == atrito x) = 0
                              | otherwise = (indiceMelhorAtrito m j xs) + 1

-- | Verifica que direção tem que jogar o 'Jogador' para alcançar a 'Peca' de menor atrito.
melhorPista :: Mapa         -- ^ 'Mapa' atual.
            -> Jogador      -- ^ 'Jogador' a verificar.
            -> Maybe Jogada -- ^ 'Jogada' resultante.
            
melhorPista m j | melhorPeca < pista  = Just (Movimenta C)
                | melhorPeca >  pista = Just (Movimenta B)
                | otherwise           = Just Acelera
                          where melhorPeca = indiceMelhorAtrito m j (listaPiso m j)
                                pista      = getPista j
