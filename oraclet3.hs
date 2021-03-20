module OracleT3 where
 
import LI11920
 
import Control.Monad
import Data.Foldable
 
import Oracle
 --import OracleTH
import Control.Lens
import Safe

-- erro: bulldozer n pintou a pista toda

numeroPistasInstrucoes :: Instrucoes -> Int
numeroPistasInstrucoes = maximum0 . map numeroPistasInstrucao

numeroPistasInstrucao :: Instrucao -> Int
numeroPistasInstrucao (Anda pistas piso) = succ $ maximum0 pistas
numeroPistasInstrucao (Sobe pistas piso _) = succ $ maximum0 pistas
numeroPistasInstrucao (Desce pistas piso _) = succ $ maximum0 pistas
numeroPistasInstrucao (Teleporta pistas _) = succ $ maximum0 pistas
numeroPistasInstrucao (Repete _ is) = numeroPistasInstrucoes is
 
constroiIter :: Instrucoes -> [Either String Construtor]
constroiIter is = constroiIter' (construtorInicial is)
    where
    constroiIter' :: Construtor -> [Either String Construtor]
    constroiIter' c@(Construtor is posicoes mapa) = (Right c) : case is of
        [] -> []
        (i:is) -> case instrucao i (Construtor is posicoes mapa) of
            Left err -> [Left err]
            Right c' -> constroiIter' c'

constroi :: Instrucoes -> Either String Mapa
constroi = construtorMapa <=< liftM mapaConstrutor . instrucoes . construtorInicial 

construtorMapa :: MapaConstrutor -> Either String Mapa
construtorMapa m = mapM (mapM (maybe (Left $ "Mapa incompleto:\n" ++ prettyString (PrettyMapaConstrutor m)) Right)) m

construtorInicial :: Instrucoes -> Construtor
construtorInicial is = Construtor is (replicate npistas 0) (replicate npistas [Just $ Recta Terra 0])
    where
    npistas = numeroPistasInstrucoes is

instrucoes :: Construtor -> Either String Construtor
instrucoes c@(Construtor is posicoes mapa) = case is of
    [] -> return c
    (i:is) -> instrucao i (Construtor is posicoes mapa) >>= instrucoes

instrucao :: Instrucao -> Construtor -> Either String Construtor
instrucao (Anda pistas piso) c = foldM (flip (move $ Recta piso)) c pistas
instrucao (Sobe pistas piso alt) c = foldM (flip (move $ \from -> Rampa piso from $ max 0 $ from+alt)) c pistas
instrucao (Desce pistas piso alt) c = foldM (flip (move $ \from -> Rampa piso from $ max 0 $ from-alt)) c pistas
instrucao (Teleporta pistas dist) c = foldM (flip (teleporta dist)) c pistas
instrucao (Repete n is) c = repete is n c

move :: (Int -> Peca) -> Int -> Construtor -> Either String Construtor
move mkPeca pista c@(Construtor is posicoes mapa) = do
    let (pos) = maybe (-1) id $ atMay posicoes pista
    let from = maybe 0 (snd . alturasPeca) $ join $ getPeca mapa pista pos
    let pos' = succ pos
    let mapa' = extendeMatrix pista pos' Nothing (const $ Just $ mkPeca from) mapa
    let posicoes' = extendeLista pista 0 (const pos') posicoes
    return $ Construtor is posicoes' mapa'
 
teleporta :: Int -> Int -> Construtor -> Either String Construtor
teleporta dist pista c@(Construtor is posicoes mapa) = do
      let (pos) = maybe (-1) id $ atMay posicoes pista
      let pos' = max (-1) $ pos+dist
      let mapa' = extendeMatrix pista pos' Nothing id mapa
      let posicoes' = extendeLista pista 0 (const pos') posicoes
      return $ Construtor is posicoes' mapa'

repete :: Instrucoes -> Int -> Construtor -> Either String Construtor
repete ris 0 c = return c
repete ris n c@(Construtor is posicoes mapa) = return $ Construtor (ris++Repete (pred n) ris:is) posicoes mapa