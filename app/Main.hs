{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Arrow (Arrow (first))
import Control.Exception (SomeException (SomeException), catch, try)
import Control.Exception.Base (evaluate)
import Control.Monad (replicateM)
import Data.Bits (Bits (xor))
import Data.Char (toUpper)
import Data.List (elemIndex, isSuffixOf, minimumBy)
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.Generics (Selector (selName))
import System.Exit (exitFailure)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, openFile)
import Text.ParserCombinators.ReadP (string)
import Text.Read (readMaybe)

-- ! Dati

class Array a b where
  asArray :: a -> [b]

data Valori = Valori
  { humility :: Int,
    courage :: Int,
    kindness :: Int,
    respect :: Int
  }

data Senpai = Senpai
  { valori :: Valori,
    posizione :: (Int, Int)
  }

defaultSenpai :: (Int, Int) -> Senpai
defaultSenpai coordinate =
  Senpai
    { valori =
        Valori
          { humility = 0,
            courage = 0,
            kindness = 0,
            respect = 0
          },
      posizione = coordinate
    }

data Table = Table
  { dimensione :: Int,
    senpai :: [Senpai],
    u :: [(Int, Int)],
    c :: [(Int, Int)],
    g :: [(Int, Int)],
    r :: [(Int, Int)]
  }

instance Array Valori Int where
  asArray :: Valori -> [Int]
  asArray (Valori h c k r) = [h, c, k, r]

instance Show Senpai where
  show (Senpai _ posizione) = show posizione

-- Show indica che Table appartiene un'interfaccia Show per definire la funzione show
instance Show Table where
  show :: Table -> String
  show (Table n senpai u c g r) =
    "\nN = "
      ++ show n
      ++ "\nS = "
      ++ show senpai
      ++ "\nU = "
      ++ show u
      ++ "\nC = "
      ++ show c
      ++ "\nG = "
      ++ show g
      ++ "\nR = "
      ++ show r

-- ! Metodi create init table
-- sfrutta il pattern matching per definire un formato da trasformare
format :: [Char] -> [Char]
format ('{' : a : ',' : b : '}' : ',' : other) = '(' : a : ',' : b : ')' : ',' : format other
format ('{' : a : ',' : b : '}' : other) = '(' : a : ',' : b : ')' : other
format s = s

-- gestisce la riga raw di un file.dojo per poter essere formattata e la trasforma nella relativa valutazione
lineFormat :: String -> [(Int, Int)]
lineFormat string = read s :: [(Int, Int)]
  where
    dropped = drop 1 (dropWhile (/= '{') string)
    taked = take (length dropped - 2) dropped
    s = "[" ++ format taked ++ "]"

createPlayTable :: String -> Table
createPlayTable file =
  Table
    { dimensione = n,
      senpai = map defaultSenpai (lineFormat (l !! 2)),
      u = lineFormat (l !! 3),
      c = lineFormat (l !! 4),
      g = lineFormat (l !! 5),
      r = lineFormat (l !! 6 ++ " ")
    }
  where
    l = lines file

    n = read (drop 4 (head l)) :: Int

-- ! utilities
incrementIf :: Int -> Bool -> Int
incrementIf valore condizione = valore + if condizione then 1 else 0

-- Determina la complessità di tutti i percorsi in una coppia (complexity, coordinate), prende il valore minore di complexity e ritorna le sue coordinate
nearestValueIn :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
nearestValueIn base coordinates = snd (minimum (pathComplexity base coordinates))
  where
    pathComplexity :: (Int, Int) -> ([(Int, Int)] -> [(Int, (Int, Int))])
    pathComplexity (a, b) = map (\(x, y) -> ((a - x) ^ 2 + (b - y) ^ 2, (x, y)))

closerTo :: (Int, Int) -> (Int, Int) -> (Int, Int)
closerTo (x, y) (a, b)
  | null [a, b] = (x, y)
  | otherwise =
      ( oneNear x a,
        if x == a then oneNear y b else y
      )
  where
    oneNear :: Int -> Int -> Int
    oneNear x y
      | x > y = x - 1
      | x < y = x + 1
      | otherwise = x

arrTupleToTupleArr :: [(a, b)] -> ([a], [b])
arrTupleToTupleArr tuples = (map fst tuples, map snd tuples)

maybeConvertTuple :: (Maybe Int, Maybe Int) -> Maybe (Int, Int)
maybeConvertTuple (Nothing, Nothing) = Nothing
maybeConvertTuple (Nothing, _) = Nothing
maybeConvertTuple (_, Nothing) = Nothing
maybeConvertTuple (x, y) =
  Just
    ( head arr,
      arr !! 1
    )
  where
    arr = fromMaybe [0, 0] (sequenceA [x, y])

-- ! Run

allCoordinatesValori :: Table -> [(Int, Int)]
allCoordinatesValori (Table _ _ u c g r) = u ++ c ++ g ++ r

-- Muove il senpai verso la prossima meta
moveSenpai :: Table -> Senpai -> Senpai
moveSenpai table (Senpai valori posizione) =
  Senpai
    { valori = valori,
      posizione = nextPosizione
    }
  where
    nearest = posizione `nearestValueIn` allCoordinatesValori table
    nextPosizione = posizione `closerTo` nearest

handleValori :: Table -> Table
handleValori (Table dimensione senpai u c g r) =
  Table
    { dimensione = dimensione,
      senpai = map incrementValore senpai,
      u = filterCoordinate u,
      c = filterCoordinate c,
      g = filterCoordinate g,
      r = filterCoordinate r
    }
  where
    -- qui vengono applicate due semplificazioni dovute a due condizioni
    -- due elementi non possonno trovarsi nella stessa posizione (se dovesse succedere si rimuovono entrambi)
    -- la posizione degli elementi da rimuovere coincide con quella dei senpai, senza applicare filtri, in quanto
    -- se un senpai si trova su di una casella senza elemento, non succede nulla
    -- se un senpai si trova sulla stessa casella di un elemento, l'elemento viene rimosso perchè abbiamo incrementato la relativa virtu
    filterCoordinate = filter (`notElem` map posizione senpai)

    -- ! TODO PROBLEMA QUANDO U DIVENTA VUOTO ANCHE S DIVENTA VUOTO, PROBABILMENTE C'`E UN QUALCHE PROBLEMA NEL PRENDERE IL PROSSIMO VALORE MINORE
    -- Prende un senpai e lo ritorna incrementato se la posizione è presente in uno dei rispettivi array
    incrementValore :: Senpai -> Senpai
    incrementValore (Senpai valori posizione) =
      Senpai
        { valori =
            Valori
              { humility = humility valori `incrementIf` (posizione `elem` u),
                courage = courage valori `incrementIf` (posizione `elem` c),
                kindness = kindness valori `incrementIf` (posizione `elem` g),
                respect = respect valori `incrementIf` (posizione `elem` r)
              },
          posizione = posizione
        }

-- trasforma la condizione da statica a dinamica, (posizione s `elem` array) devi far si che l'incremento avvenga solo nel caso in cui la posizione di s corrisponde ad una nell'array di posizioni dei valori

-- Ogni esecuzione corrisponde ad un movimento
gong :: Table -> Table
gong table =
  handleValori
    Table
      { dimensione = dimensione table,
        senpai = nextPositions,
        u = u table,
        c = c table,
        g = g table,
        r = r table
      }
  where
    -- definisce una funzione toNext con le coordinate già definite
    toNext = moveSenpai table

    nextPositions = map toNext (senpai table)

runFor :: Table -> Int -> IO [Table]
runFor table 1 = return [gong table]
runFor table for
  | length (senpai table) <= 1 = return [table]
  | null (allCoordinatesValori table) = return [table]
  | otherwise = fmap (table :) rest
  where
    rest = do gong table `runFor` (for - 1)

main :: IO ()
main = do
  putStr "Inserisci il file: "
  fileName <- getLine

  if ".dojo" `isSuffixOf` fileName
    then putStrLn "Opening File... "
    else do
      putStrLn "Questo non è un file di tipo .dojo, vuoi aprirlo comunque? [no|yes]"
      risposta <- getLine

      case map toUpper risposta of
        "Y" -> putStr "Opening File... "
        "YES" -> putStr "Opening File... "
        _ -> exitFailure

  isAllowed <- try (openFile fileName ReadMode) :: IO (Either SomeException Handle)

  case isAllowed of
    Left ex -> putStrLn "Error Opening File"
    Right handle -> do
      file <- hGetContents handle

      let table = createPlayTable file

      putStr "Inserisci il numero di esecuzioni o qualsiasi carattere per raggiungere la configurazione finale: "
      inputStr <- getLine

      let numerOfExecutions = fromMaybe 0 (readMaybe inputStr)

      ex <- table `runFor` numerOfExecutions

      putStrLn "~ Esecuzione ~"
      mapM_ print ex

      hClose handle