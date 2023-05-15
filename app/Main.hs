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
  { umilta :: Int,
    coraggio :: Int,
    gentilezza :: Int,
    rispetto :: Int
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
          { umilta = 0,
            coraggio = 0,
            gentilezza = 0,
            rispetto = 0
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

points :: Senpai -> Int
points (Senpai valori _) = umilta valori + coraggio valori + gentilezza valori + rispetto valori

instance Array Valori Int where
  asArray :: Valori -> [Int]
  asArray (Valori h c k r) = [h, c, k, r]

instance Show Senpai where
  show :: Senpai -> String
  show (Senpai _ posizione) = show posizione

instance Eq Senpai where
  (==) :: Senpai -> Senpai -> Bool
  (==) s1 s2 = valori s1 == valori s2 && posizione s1 == posizione s2

instance Eq Valori where
  (==) :: Valori -> Valori -> Bool
  (==) v1 v2 = umilta v1 == umilta v2 && coraggio v1 == coraggio v2 && gentilezza v1 == gentilezza v2 && rispetto v1 == rispetto v2

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

complexityCalc :: (Int, Int) -> (Int, Int) -> Int
complexityCalc (x, y) (a, b) = (a - x) ^ 2 + (b - y) ^ 2

pathComplexity :: (Int, Int) -> ([(Int, Int)] -> [(Int, (Int, Int))])
pathComplexity (a, b) = map (\(x, y) -> (complexityCalc (a, b) (x, y), (x, y)))

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

handleCombat :: Table -> Table
handleCombat (Table dimensione senpai u c g r) =
  Table
    { dimensione = dimensione,
      senpai = filter haveMorePoints newSenpai,
      u = u,
      c = c,
      g = g,
      r = r
    }
  where
    newSenpai = map incrementValore senpai

    -- determina se il senpai possiede più punti del suo immediatamente vicino
    haveMorePoints :: Senpai -> Bool
    haveMorePoints senpai 
      | fst complexity <= 1 = points senpai > points otherSenpai -- ? TODO attenzione, qui ho definito che i punti vengono controllati anche se la posizione è la stessa, cosa probabilmente corretta ma mancante nella documentazione
      | otherwise = True
      where
        -- prende il vicino
        complexity = nearestSenpai senpai
        otherSenpai = snd complexity


    -- ritorna la posizione del senpai più vicino
    nearestSenpai :: Senpai -> (Int, Senpai)
    nearestSenpai actualSenpai = head (map (\s -> (complexityCalc (posizione actualSenpai) (posizione s), s)) senpai)
    -- Prende un senpai e lo ritorna incrementando ogni valore se il senpai più vicino si trova a complessità 1 ed inoltre il relativo valore è maggiore di quello dell'avversario
    incrementValore :: Senpai -> Senpai
    incrementValore senpai
      | fst complexity <= 1 =
          Senpai
            { valori =
                Valori
                  { umilta = umilta v `incrementIf` (umilta v > umilta (valori otherSenpai)),
                    coraggio = coraggio v `incrementIf` (coraggio v > coraggio (valori otherSenpai)),
                    gentilezza = gentilezza v `incrementIf` (gentilezza v > gentilezza (valori otherSenpai)),
                    rispetto = rispetto v `incrementIf` (rispetto v > rispetto (valori otherSenpai))
                  },
              posizione = posizione senpai
            }
      | otherwise = senpai
      where
        v = valori senpai
        complexity = nearestSenpai senpai
        otherSenpai = snd complexity

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
    --  due elementi non possonno trovarsi nella stessa posizione (se dovesse succedere si rimuovono entrambi)
    --  la posizione degli elementi da rimuovere coincide con quella dei senpai, senza applicare filtri, in quanto
    --   se un senpai si trova su di una casella senza elemento, non succede nulla
    --   se un senpai si trova sulla stessa casella di un elemento, l'elemento viene rimosso perchè abbiamo incrementato la relativa virtu
    filterCoordinate = filter (`notElem` map posizione senpai)

    -- Prende un senpai e lo ritorna incrementato se la posizione è presente in uno dei rispettivi array
    incrementValore :: Senpai -> Senpai
    incrementValore (Senpai valori posizione) =
      Senpai
        { valori =
            Valori
              { umilta = umilta valori `incrementIf` (posizione `elem` u),
                coraggio = coraggio valori `incrementIf` (posizione `elem` c),
                gentilezza = gentilezza valori `incrementIf` (posizione `elem` g),
                rispetto = rispetto valori `incrementIf` (posizione `elem` r)
              },
          posizione = posizione
        }

-- trasforma la condizione da statica a dinamica, (posizione s `elem` array) devi far si che l'incremento avvenga solo nel caso in cui la posizione di s corrisponde ad una nell'array di posizioni dei valori

-- Ogni esecuzione corrisponde ad un movimento
gong :: Table -> Table
gong table =
  handleCombat
    ( handleValori
        Table
          { dimensione = dimensione table,
            senpai = nextPositions,
            u = u table,
            c = c table,
            g = g table,
            r = r table
          }
    )
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