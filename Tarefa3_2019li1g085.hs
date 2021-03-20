{- |

= Introdução

Nesta tarefa, tivemos que encontrar uma forma de comprimir ao máximo o estado do jogo, fazendo com que o
ocupasse o menor número de instruções possiveis, mantendo toda a sua informação.

= Objetivos

Para atingir uma taxa de compressão alta, fazemo-lo de duas formas diferentes e, no final, comparamo-las vendo
qual aprensenta o melhor resultado para cada caso. A primeira abordagem foi agrupar horizontalmente os padroẽs
horizontais seguido de agrupar verticalmente os padrões verticais (não desfasados). O segundo caso é o inverso,
ou seja, tentar agrupar horizontalmente após agrupar verticalmente.

= Conclusão

Acreditamos que fizemos o melhor que conseguimos. Apesar de não termos apresentado a 3ª alternativa (teleporta),
achamos que conseguimos um resultado, não perfeito, mas aceitável e equilibrado. Desvendar a "magia" por de trás
da Teleporta, infelizmente até hoje não desvendado por nós, foi o maior desafio das tarefas.

-}

module Tarefa3_2019li1g085 where

import LI11920
import Data.List

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa] -- ^ 'Mapa' de teste
testesT3 = [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10]

-- | Mapa simples, composto por 1 'Pista' de 'Recta' 'Terra' 0
m1 :: Mapa
m1 = [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]]
-- | Mapa com 2 'Pista' com 'Recta' e 'Rampa' de diferentes pisos
m2 :: Mapa
m2 = [[Recta Terra 0,Recta Relva 0  ,Recta Boost 0  ,Rampa Terra 0 2,Rampa Terra 2 0,Rampa Terra 0 2],
      [Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0  ]]
-- | Mapa com 2 'Pista' iguais
m3 :: Mapa 
m3 = [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0],
      [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0]]  
-- | Mapa com 4 'Pista' não iguais
m4 :: Mapa
m4 = [[Recta Terra 0,Recta Terra 0  ,Recta Terra 0  ,Recta Terra 0  ,Recta Terra 0,  Recta Terra 0  ],
      [Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0  ],
      [Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0  ],
      [Recta Terra 0,Recta Relva 0  ,Recta Boost 0  ,Rampa Terra 0 2,Rampa Terra 2 0,Rampa Terra 0 2]]
-- | Mapa com 4 'Pista' não iguais
m5 :: Mapa
m5 = [[Recta Terra 0,Recta Terra 0  ,Recta Terra 0  ,Recta Terra 0  ,Recta Terra 0  ,Recta Terra 0  ,Recta Relva 0],
      [Recta Terra 0,Recta Terra 0  ,Rampa Terra 0 2,Rampa Terra 2 0,Rampa Terra 0 2,Rampa Terra 2 1,Recta Relva 1],
      [Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0  ,Recta Relva 0],
      [Recta Terra 0,Recta Relva 0  ,Recta Boost 0  ,Rampa Terra 0 2,Rampa Terra 2 0,Rampa Terra 0 2,Recta Relva 2]]
-- | Mapa com 4 'Pista' iguais com todos os tipos de 'Piso'
m6 :: Mapa       
m6 = [[Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Lama 0,Recta Boost 0,Recta Cola 0,Rampa Terra 0 2,Rampa Relva 2 0,Rampa Lama 0 2,Rampa Boost 2 0,Rampa Cola 0 2],
      [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Lama 0,Recta Boost 0,Recta Cola 0,Rampa Terra 0 2,Rampa Relva 2 0,Rampa Lama 0 2,Rampa Boost 2 0,Rampa Cola 0 2],
      [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Lama 0,Recta Boost 0,Recta Cola 0,Rampa Terra 0 2,Rampa Relva 2 0,Rampa Lama 0 2,Rampa Boost 2 0,Rampa Cola 0 2],
      [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Lama 0,Recta Boost 0,Recta Cola 0,Rampa Terra 0 2,Rampa Relva 2 0,Rampa Lama 0 2,Rampa Boost 2 0,Rampa Cola 0 2]]
-- | Mapa com apenas 1 'Pista' e 2 'Peca'
m7 :: Mapa
m7 = [[Recta Terra 0,Recta Terra 0]]
-- | Mapa gerado aleatoriamente a partir da Tarefa 1 (gera 5 6 1)
m8 :: Mapa
m8 = [[Recta Terra 0,Recta Boost 0,Recta Boost 0  ,Recta Boost 0  ,Recta Lama 0   ,Recta Terra 0  ],
      [Recta Terra 0,Recta Terra 0,Rampa Lama  0 2,Rampa Lama  2 0,Rampa Relva 0 1,Rampa Relva 1 0],
      [Recta Terra 0,Recta Lama  0,Recta Terra 0  ,Rampa Terra 0 2,Rampa Boost 2 0,Recta Terra 0  ],
      [Recta Terra 0,Recta Terra 0,Recta Terra 0  ,Recta Lama  0  ,Recta Terra 0  ,Recta Terra 0  ],
      [Recta Terra 0,Recta Terra 0,Recta Terra 0  ,Recta Terra 0  ,Recta Relva 0  ,Rampa Relva 0 2]]
-- | Teste com 2 'Pista'
m9 :: Mapa
m9 = [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],
      [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0]] 
-- | Teste com 4 'Pista'
m10 :: Mapa
m10 = [[Recta Terra 0,Recta Terra 0,Recta Terra 0  ,Recta Boost 0  ,Recta Lama 0   ,Recta Terra 0  ],
       [Recta Terra 0,Recta Terra 0,Rampa Lama  0 2,Rampa Lama  2 0,Rampa Relva 0 1,Rampa Relva 1 0],
       [Recta Terra 0,Recta Terra 0,Recta Terra 0  ,Rampa Terra 0 2,Rampa Boost 2 0,Recta Terra 0  ],
       [Recta Terra 0,Recta Terra 0,Recta Terra 0  ,Recta Lama  0  ,Recta Terra 0  ,Recta Terra 0  ]]







-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer 'Mapa' válido m, executar as instruções '(desconstroi m)' produza o mesmo 'Mapa' m.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.

desconstroi :: Mapa       -- ^ 'Mapa' atual.
            -> Instrucoes -- ^ 'Mapa' desconstruido em forma de 'Instrucoes'.

desconstroi m | tamanhoInstrucoes (horizontais (lista2inst (mapa2inst 0 m)))
                < 
                tamanhoInstrucoes (lista2inst (horizontaisFromMatriz (mapa2inst 0 m)))

                          = (horizontais (lista2inst (mapa2inst 0 m)))

              | otherwise = (lista2inst (horizontaisFromMatriz (mapa2inst 0 m)))

-- ** Funções de descontrução de mapa

-- | Transforma uma 'Pista' em 'Instrucoes'.

inst :: Pista      -- ^ 'Pista' por descontruir.
     -> Int        -- ^ Indice da 'Pista'.
     -> Instrucoes -- ^ 'Pista' desconstruida em forma de 'Instrucoes'.

inst [] n = []
inst ((Recta p a)     : ps) n             = (Anda  [n] p)         : inst ps n
inst ((Rampa p ai af) : ps) n | ai  <  af = (Sobe  [n] p (af-ai)) : inst ps n
                              | otherwise = (Desce [n] p (ai-af)) : inst ps n

-- | Desconstroi o 'Mapa' de 'Pista' em 'Pista'.

mapa2inst :: Int          -- ^ Iteração ( == Indice da 'Pista').
          -> Mapa         -- ^ 'Mapa' atual.
          -> [Instrucoes] -- ^ 'Mapa' desconstruido em forma de 'Instrucoes', dividido por 'Pista' excluindo a primeira 'Peca'.

mapa2inst _ []     = []
mapa2inst i (p:ps) = (inst (tail p) i) : mapa2inst (i+1) ps


-- ** Funções de agrupar horizontalmente

-- | Agrupa as sequencias horizontais de 'Instrucoes' iguais. (Usada antes de agrupar verticalmente)

horizontaisFromMatriz :: [Instrucoes] -> [Instrucoes]
horizontaisFromMatriz m = map horizontais m


-- | Agrupa as sequencias horizontais de 'Instrucoes' iguais.

horizontais :: Instrucoes -- ^ 'Instrucoes' com os padroẽs horizontais por agrupar.
            -> Instrucoes -- ^ 'Instrucoes' com os padroẽs horizontais agrupados.

horizontais [x] = [x]
horizontais ((Repete n [i]) : i2 : is) | i2 == i   = horizontais ((Repete (n+1) [i]) : is)
                                       | otherwise = (Repete n [i]) : horizontais (i2 : is)

horizontais (i1 : i2 : is) | i1 == i2  = horizontais ((Repete 2 [i1]) : is)
                           | otherwise = i1 : horizontais ( i2 : is)


-- ** Funções de agrupar verticalmente

-- | Agrupa os padroẽs verticais de 'Instrucoes' no mesmo "indice".

verticais :: Instrucoes -- ^ Linha de 'Instrucoes' por agrupar.
          -> Instrucoes -- ^ Linha de 'Instrucoes' agrupadas verticalmente.

verticais [i] = [i]
verticais ((Repete n [i]) :i2:is)  | instrucoesIguais (Repete n [i]) i2 = verticais ((Repete n [adicionaIndice i i2]) : is)
verticais (i1             :i2:is)  | instrucoesIguais i1 i2             = verticais ((adicionaIndice i1 i2)  : is)
                                   | otherwise                          = verticais (i1:is)

-- | Remove as 'Instrucoes' que ficaram a mais após serem agrupadas pela função 'verticais'.

removeVerticais :: Instrucoes -- ^ Linha de 'Instrucoes' com padrões verticais agrupados.
                -> Instrucoes -- ^ Linha de 'Instrucoes' apenas com as 'Instrucoes' pretendidas.

removeVerticais [i] = []
removeVerticais ((Repete n l)   :i2:is)  | instrucoesIguais (Repete n l)   i2 =      removeVerticais ((Repete n l) : is)
removeVerticais (i1             :i2:is)  | instrucoesIguais i1 i2             =      removeVerticais (i1           : is)
                                         | otherwise                          = i2 : removeVerticais (i1           : is)

-- | Função que permite aplicar as funções 'verticais' e 'removeVerticais' e assim, agrupar verticalmente uma coluna da matriz.

aplicaVerticais :: Instrucoes -- ^ Linha de 'Instrucoes'.
                -> Instrucoes -- ^ Linha de 'Instrucoes'.

aplicaVerticais [] = []
aplicaVerticais i  = verticais i ++ aplicaVerticais (removeVerticais i)

-- | Transforma uma lista de 'Instrucoes' orgazinadas por 'Pista' numa 'Instrucoes' apenas (Usada para ambas as sequencias de agrupar).

lista2inst :: [Instrucoes] -- ^ 'Mapa' desconstruido em forma de 'Instrucoes', dividido por 'Pista'.
           -> Instrucoes   -- ^ 'Mapa' inteiro em forma de 'Instrucoes' com os padrões verticais já agrupados.
lista2inst [m] = horizontais m
lista2inst  m  = foldl (++) [] (map aplicaVerticais (transpose m))


-- ** Funções de secundárias

-- | Verifica se duas 'Instrucoes' são iguais.

instrucoesIguais :: Instrucao -- ^ 'Instrucao' "atual".
                 -> Instrucao -- ^ 'Instrucao' "inferior".
                 -> Bool      -- ^ São iguais?.


instrucoesIguais (Anda  i1 p1)      (Anda  i2 p2)    = p2 == p1
instrucoesIguais (Sobe  i1 p1 a1)   (Sobe  i2 p2 a2) = p2 == p1 && a1 == a2
instrucoesIguais (Desce i1 p1 a1)   (Desce i2 p2 a2) = p2 == p1 && a1 == a2
instrucoesIguais (Repete n1 [l1])   (Repete n2 [l2]) = n1 == n2 && instrucoesIguais l1 l2
instrucoesIguais _ _ = False

-- | Obtem o "indice" da 'Instrucao'.

obtemIndice :: Instrucao -- ^ 'Instrucao' analisada.
            -> Int       -- ^ "Indice" a que pertence.

obtemIndice (Anda  [i] _)   = i
obtemIndice (Sobe  [i] _ _) = i
obtemIndice (Desce [i] _ _) = i
obtemIndice (Repete n [i])  = obtemIndice i

-- | Adiciona o indice da segunda 'Instrucao' à primeira (Só funciona para 'Instrucoes' iguais).

adicionaIndice :: Instrucao -- ^ 'Instrucao' que onde vai ser agrupado ambos os indices
               -> Instrucao -- ^ 'Instrucao' que irá ser agrupada
               -> Instrucao -- ^ 'Instrucao' após agrupar

adicionaIndice (Anda  i1 p)   i2 = (Anda  (i1++[obtemIndice i2]) p)
adicionaIndice (Sobe  i1 p a) i2 = (Sobe  (i1++[obtemIndice i2]) p a)
adicionaIndice (Desce i1 p a) i2 = (Desce (i1++[obtemIndice i2]) p a)
