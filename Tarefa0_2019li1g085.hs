
module Tarefa0_2019li1g085 where

-- * FunÃ§Ãµes nÃ£o-recursivas.

-- | Um ponto a duas dimensÃµes dado num referencial cartesiado (distÃ¢ncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distÃ¢ncia Ã  origem e Ã¢ngulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo
 deriving (Show)

-- | Um Ã¢ngulo em graus.
type Angulo = Double

-- ** FunÃ§Ãµes sobre vetores

-- | Um 'Vetor' na representaÃ§Ã£o escalar Ã© um 'Ponto' em relaÃ§Ã£o Ã  origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** FunÃ§Ãµes gerais sobre 'Vetor'es.\
--
-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = (Cartesiano (x1+x2) (y1+y2))
somaVetores v1 v2                                 = somaVetores (polar2cart v1) (polar2cart v2)

-- | Transforma polar em cartesiano
polar2cart :: Vetor -> Vetor
polar2cart (Polar r a)      = (Cartesiano (r*cos (graus2radianos(a))) (r*sin (graus2radianos(a))))
polar2cart (Cartesiano x y) = (Cartesiano x y)

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = (Cartesiano (x1-x2) (y1-y2))
subtraiVetores v1 v2                                 = subtraiVetores (polar2cart v1) (polar2cart v2)


-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor r (Cartesiano x y) = (Cartesiano (x*r) (y*r))
multiplicaVetor r v                = multiplicaVetor r (polar2cart v) 

-- ** FunÃ§Ãµes sobre rectas.

-- | Um segmento de reta Ã© definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equaÃ§Ãµes matemÃ¡ticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam (p1,p2) (p3,p4)| 0<=ta (p1,p2) (p3,p4) && 1>=ta (p1,p2) (p3,p4) && 0<=tb (p1,p2) (p3,p4) && 1>=tb (p1,p2) (p3,p4) = True
                          | otherwise                                                                                        = False


-- | Calcula ta
ta :: Reta -> Reta -> Double
ta ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = ((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3)) / ((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
ta (p1,p2) (p3,p4)                                                                 = ta (polar2cart p1,polar2cart p2) (polar2cart p3,polar2cart p4)

-- | Calcula tb
tb :: Reta -> Reta -> Double
tb ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = ((y1-y2)*(x1-x3)+(x2-x1)*(y1-y3)) / ((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
tb (p1,p2) (p3,p4)                                                                 = tb (polar2cart p1,polar2cart p2) (polar2cart p3,polar2cart p4)

-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equaÃ§Ãµes matemÃ¡ticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao (p1,p2) (p3,p4) =  somaVetores p1 (multiplicaVetor (ta (p1,p2) (p3,p4)) (subtraiVetores p2 p1))




-- ** FunÃ§Ãµes sobre listas

-- *** FunÃ§Ãµes gerais sobre listas.
--
-- FunÃ§Ãµes nÃ£o disponÃ­veis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence Ã  lista.
--
-- __SugestÃ£o:__ use a funÃ§Ã£o 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i [] = False
eIndiceListaValido i v | i>=0 && i<length v = True
                       | otherwise          = False  

-- ** FunÃ§Ãµes sobre matrizes.

-- *** FunÃ§Ãµes gerais sobre matrizes.

-- | A dimensÃ£o de um mapa dada como um par (/nÃºmero de linhas/,/nÃºmero de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posiÃ§Ã£o numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas sÃ£o dois nÃºmeros naturais e comeÃ§am com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz Ã© um conjunto de elementos a duas dimensÃµes.
--
-- Em notaÃ§Ã£o matemÃ¡tica, Ã© geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensÃ£o de uma matriz.
--
-- __NB:__ Note que nÃ£o existem matrizes de dimensÃ£o /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensÃ£o /0 * 0/.
--
-- __SugestÃ£o:__ relembre a funÃ§Ã£o 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz []      = (0,0)
dimensaoMatriz ([]:xs) = (0,0)
dimensaoMatriz (x:xs)  = (length (x:xs),length x)

-- | Verifica se a posiÃ§Ã£o pertence Ã  matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool
ePosicaoMatrizValida _ [] = False 
ePosicaoMatrizValida (l,c) (m:ms) | length (m:ms) > l && length m > c = True
                                  | otherwise                         = False

-- * FunÃ§Ãµes recursivas.

-- ** FunÃ§Ãµes sobre Ã¢ngulos

-- | Normaliza um Ã¢ngulo na gama [0..360).
--  Um Ã¢ngulo pode ser usado para representar a rotaÃ§Ã£o
--  que um objecto efectua. Normalizar um Ã¢ngulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientaÃ§Ã£o do
--  objecto que resulta da aplicaÃ§Ã£o de uma rotaÃ§Ã£o. Por exemplo, Ã© verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo a | a>=360 = normalizaAngulo (a-360)
                  | a<0 = normalizaAngulo (a+360)
                  | otherwise = a

-- | Conversão Radianos para Graus
radianos2graus :: Double -> Double
radianos2graus a = a*(180/pi)

-- | Conversão Graus para Radianos
graus2radianos :: Double -> Double
graus2radianos a = a*(pi/180)

-- ** FunÃ§Ãµes sobre listas.

-- | Devolve o elemento num dado Ã­ndice de uma lista.
--
-- __SugestÃ£o:__ NÃ£o use a funÃ§Ã£o (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (x:xs) = x
encontraIndiceLista i (x:xs) = encontraIndiceLista (i-1) xs

-- | Modifica um elemento num dado Ã­ndice.
--
-- __NB:__ Devolve a prÃ³pria lista se o elemento nÃ£o existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista 0 x (h:hs)=(x:hs)
atualizaIndiceLista i x [] = [x]
atualizaIndiceLista i x (h:hs)=h:atualizaIndiceLista (i-1) x hs
-- ** FunÃ§Ãµes sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (x,y) (h:hs) | x==0 = encontraIndiceLista y h
                                   | otherwise = encontraPosicaoMatriz (x-1,y) hs

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a prÃ³pria 'Matriz' se o elemento nÃ£o existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l,c) n (h:t) | l == 0 = [(take (c) h) ++ [n] ++ (drop (c+1) h)] ++ t
                                    | otherwise = ([h] ++ atualizaPosicaoMatriz (l-1,c) n t)