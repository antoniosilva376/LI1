-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g085 where

import LI11920
import Tarefa0_2019li1g085
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.

-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(0,1,2),(2,1,1),(2,4,6),(5,20,5),(5,20,4),(5,20,7),(10,7,9),(2,13,17),(7,7,3),(9,8,8),(1,50,1),(2,25,2),(6,21,73)]

-- * Funções pré-definidas da Tarefa 1.

-- | Função que gera uma lista de inteiros aleatoriamente.

geraAleatorios :: Int   -- ^ Numero de Inteiros necessarios.
               -> Int   -- ^ Inteiro da semente.
               -> [Int] -- ^ Lista de Inteiros gerados.

geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.

-- | Funçao que gera o mapa.

gera :: Int  -- ^ Inteiro que corresponde ao numero de pistas.
     -> Int  -- ^ Inteiro que corresponde ao comprimento.
     -> Int  -- ^ Inteiro correspondente à semente.
     -> Mapa -- ^ 'Mapa' gerado.

gera 0 _ _ = []
gera npistas 1 semente = replicate npistas [Recta Terra 0]  
gera npistas comprimento semente = geraPistas 0 npistas comprimento semente

-- | Gera as pistas.

geraPistas :: Int  -- ^ Inteiro que corresponde ao numero da iteração (Cada iteração gera uma pista).
           -> Int  -- ^ Inteiro que define a quantidade de 'Pista' existentes.
           -> Int  -- ^ Inteiro que define o comprimento da 'Pista'.
           -> Int  -- ^ Inteiro da semente.
           -> Mapa -- ^ 'Mapa' gerado.

geraPistas n npistas _ _ | n==npistas = []
geraPistas n npistas comprimento semente = tipoPeca (comprimento-1) (drop ((comprimento-1)*2*n) (geraAleatorios ((comprimento-1)*2*npistas) semente)) [Recta Terra 0] :
                                           geraPistas (n+1) npistas comprimento semente

-- | A partir de inteiros gera o 'Piso' correspondente.

geraPiso :: Int  -- ^ Inteiro gerado.
         -> Piso -- ^ 'Piso' da 'Peca' anterior.
         -> Piso -- ^ 'Piso' gerado.

geraPiso i p | i == 0 || i == 1 = Terra
             | i == 2 || i == 3 = Relva
             | i == 4           = Lama
             | i == 5           = Boost
             | i >= 6 && i <= 9 = p

-- |  Gera uma pista, a partir da lista de inteiros gerados aleatoriamente, com as 'Peca' correspondentes.

tipoPeca :: Int   -- ^ Inteiro correspondente ao comprimento.
         -> [Int] -- ^ Lista de inteiros gerados aleatoriamente.
         -> Pista -- ^ 'Pista' que permite verificar a 'Peca' anterior.
         -> Pista -- ^ 'Pista' gerada.

tipoPeca 0 _ p = p

-- Caso a 'Anterior Peca' seja 'Recta'.

tipoPeca c (i1:i2:is) [Recta p a]         | (i2==0||i2==1)                = (Recta p a):
                                                                            tipoPeca (c-1) is [Rampa (geraPiso i1 p) a (a+i2+1)]
                                          | (i2>=2&&i2<=5) && a==0        = (Recta p a):
                                                                            tipoPeca (c-1) is [Recta (geraPiso i1 p) a]
                                          | (i2>=2&&i2<=5) && (a-i2+1<=0) = (Recta p a):
                                                                            tipoPeca (c-1) is [Rampa (geraPiso i1 p) a 0]
                                          | (i2>=2&&i2<=5)                = (Recta p a):
                                                                            tipoPeca (c-1) is [Rampa (geraPiso i1 p) a (a-i2+1)]
                                          | (i2>=6&&i2<=9)                = (Recta p a):
                                                                            tipoPeca (c-1) is [Recta (geraPiso i1 p) a]
                                          
-- Caso a 'Anterior Peca' seja 'Rampa'.
tipoPeca c (i1:i2:is) [Rampa p a1 a2]         | (i2==0||i2==1)                 = (Rampa p a1 a2):
                                                                                 tipoPeca (c-1) is [Rampa (geraPiso i1 p) a2 (a2+i2+1)]
                                              | (i2>=2&&i2<=5) && a2==0        = (Rampa p a1 a2):
                                                                                 tipoPeca (c-1) is [Recta (geraPiso i1 p) a2]
                                              | (i2>=2&&i2<=5) && (a2-i2+1<=0) = (Rampa p a1 a2):
                                                                                 tipoPeca (c-1) is [Rampa (geraPiso i1 p) a2 0]
                                              | (i2>=2&&i2<=5)                 = (Rampa p a1 a2):
                                                                                 tipoPeca (c-1) is [Rampa (geraPiso i1 p) a2 (a2-i2+1)]
                                              | (i2>=6&&i2<=9)                 = (Rampa p a1 a2):
                                                                                 tipoPeca (c-1) is [Recta (geraPiso i1 p) a2]