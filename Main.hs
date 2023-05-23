{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Arrow (Arrow (first))
import Control.Exception (SomeException (SomeException), catch, try)
import Control.Exception.Base (evaluate)
import Control.Monad (replicateM)
import Data.Bits (Bits (xor))
import Data.Char (toUpper)
import Data.List (elemIndex, isSuffixOf, minimumBy)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import GHC.Generics (Selector (selName))
import System.Exit (exitFailure)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, openFile)
import Text.ParserCombinators.ReadP (string)
import Text.Read (readMaybe)

-- ! Dati
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

data Table = Table
  { dimensione :: Int,
    senpai :: [Senpai],
    u :: [(Int, Int)],
    c :: [(Int, Int)],
    g :: [(Int, Int)],
    r :: [(Int, Int)]
  }

class Array a b where
  asArray :: a -> [b]

instance Array Valori Int where
  asArray :: Valori -> [Int]
  asArray (Valori h c k r) = [h, c, k, r]

instance Show Senpai where
  show :: Senpai -> String
  show (Senpai v p) = show (fst p, snd p, array !! 0, array !! 1, array !! 2, array !! 3)
    where
      array = asArray v :: [Int]

instance Eq Senpai where
  (==) s1 s2 = False
  (/=) s1 s2 = True

instance Ord Senpai where
  (>) :: Senpai -> Senpai -> Bool
  (>) senpai otherSenpai
    | points senpai == points otherSenpai = positionPoints senpai > positionPoints otherSenpai
    | otherwise = points senpai > points otherSenpai
  (<=) = (not .) . (>)

-- Show indica che Table appartiene un'interfaccia Show per definire la funzione show
instance Show Table where
  show :: Table -> String
  show (Table n senpai u c g r) =
    "\nN = " ++ show n ++ "\nD = {" ++ "\n\tS = " ++ show senpai ++ ",\n\tU = " ++ show u ++ ",\n\tC = " ++ show c ++ ",\n\tG = " ++ show g ++ ",\n\tR = " ++ show r ++ "\n}"

defaultValori :: Int -> Int -> Int -> Int -> Valori
defaultValori u c g r =
  Valori
    { umilta = u,
      coraggio = c,
      gentilezza = g,
      rispetto = r
    }

defaultSenpai :: (Int, Int, Int, Int, Int, Int) -> Senpai
defaultSenpai (x, y, u, c, g, r) =
  Senpai
    { valori = defaultValori u c g r,
      posizione = (x, y)
    }

maybeValori :: Maybe Senpai -> Maybe Valori
maybeValori Nothing = Nothing
maybeValori s = (Just . valori . fromJust) s

positionPoints :: Senpai -> Int
positionPoints (Senpai _ (n, m)) = ((n + m) * (n + m - 1) `div` 2) + n - m

points :: Senpai -> Int
points (Senpai valori _) = umilta valori + coraggio valori + gentilezza valori + rispetto valori

-- ! Metodi create init table
-- sfrutta il pattern matching per definire la formattazione da rispettare
format :: [Char] -> [Char]
format "}}" = ")]"
format "}}," = ")]"
format ('{' : '{' : some) = '[' : '(' : format some
format ('{' : some) = '(' : format some
format (',' : some) = ',' : format some
format ('}' : some) = ')' : format some
format (s : some) = s : format some

-- gestisce la riga raw di un file.dojo per poter essere formattata e la trasforma nella relativa valutazione
lineFormat :: String -> [(Int, Int)]
lineFormat ('\t' : _ : '=' : values) = read (format values) :: [(Int, Int)]

senpaiLineFormat :: String -> [(Int, Int, Int, Int, Int, Int)]
senpaiLineFormat ('\t' : _ : '=' : values) = read (format values) :: [(Int, Int, Int, Int, Int, Int)]

createPlayTable :: String -> Table
createPlayTable file =
  Table
    { dimensione = n,
      senpai = if areOut (map posizione _senpai) then error "Un senpai ha una coordinata superiore alla dimensione" else _senpai,
      u = if areOut _u then error "Un oggetto 'u' ha una coordinata superiore alla dimensione" else _u,
      c = if areOut _c then error "Un oggetto 'c' ha una coordinata superiore alla dimensione" else _c,
      g = if areOut _g then error "Un oggetto 'g' ha una coordinata superiore alla dimensione" else _g,
      r = if areOut _r then error "Un oggetto 'r' ha una coordinata superiore alla dimensione" else _r
    }
  where
    l = lines file
    n = read (drop 4 (head l)) :: Int

    areOut :: [(Int, Int)] -> Bool
    areOut [] = False
    areOut (x : xs) = (\(x, y) -> x > n || y > n) x || areOut xs

    _senpai = map defaultSenpai (senpaiLineFormat (l !! 2))
    _u = lineFormat (l !! 3)
    _c = lineFormat (l !! 4)
    _g = lineFormat (l !! 5)
    _r = lineFormat (l !! 6)

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
    arr = fromMaybe [0] (sequenceA [x, y])

-- ! Run

allCoordinatesValori :: Table -> [(Int, Int)]
allCoordinatesValori (Table _ _ u c g r) = u ++ c ++ g ++ r

-- Muove il senpai verso la prossima meta
moveSenpai :: [(Int, Int)] -> Senpai -> Senpai
moveSenpai globalValori (Senpai valori posizione) =
  Senpai
    { valori = valori,
      posizione = nextPosizione
    }
  where
    nearest = posizione `nearestValueIn` globalValori
    nextPosizione = posizione `closerTo` nearest

combat :: [Senpai] -> [Senpai]
combat senpai = filter toKeep . map incrementValoriSenpai $ senpai
  where
    -- determina se il senpai possiede più punti totali di quello immediatamente vicino (o true se nessuno si trova a distanza 1)
    toKeep senpai = Just senpai > nearestSenpai senpai -- se other senpai è Nothing ritorna True
    nearestSenpai :: Senpai -> Maybe Senpai -- il senpai vicino 1, nothing altrimenti
    nearestSenpai (Senpai _ pos)
      | length near == 1 = Just (head near)
      | otherwise = Nothing
      where
        near = filter (\s -> complexityCalc pos (posizione s) == 1) senpai

    incrementValoriSenpai :: Senpai -> Senpai -- Prende un senpai e lo ritorna incrementando ogni valore se il senpai più vicino si trova a complessità 1 ed inoltre il relativo valore è maggiore di quello dell'avversario
    incrementValoriSenpai senpai =
      -- ? TODO attenzione, qui ho definito che i punti vengono controllati anche se la posizione è la stessa, cosa probabilmente corretta ma mancante nella documentazione
      Senpai
        { valori = incrementValori (valori senpai) ((maybeValori . nearestSenpai) senpai),
          posizione = posizione senpai
        }

    incrementValori :: Valori -> Maybe Valori -> Valori
    incrementValori valori Nothing = valori
    incrementValori (Valori u c g r) (Just (Valori u' c' g' r')) =
      Valori
        { umilta = u `incrementIf` (u > u'),
          coraggio = c `incrementIf` (c > c'),
          gentilezza = g `incrementIf` (g > g'),
          rispetto = r `incrementIf` (r > r')
        }

gong :: Table -> Table -- Ogni esecuzione corrisponde ad un movimento
gong (Table dimensione senpai u c g r) =
  Table
    { dimensione = dimensione,
      senpai = map incrementValori . combat $ moved,
      u = filterCoordinate u,
      c = filterCoordinate c,
      g = filterCoordinate g,
      r = filterCoordinate r
    }
  where
    moved = map (moveSenpai $ u ++ c ++ g ++ r) senpai
    -- qui vengono applicate due semplificazioni dovute a due condizioni
    --  due elementi non possonno trovarsi nella stessa posizione (se dovesse succedere si rimuovono entrambi)
    --  la posizione degli elementi da rimuovere coincide con quella dei senpai, senza applicare filtri, in quanto
    --   se un senpai si trova su di una casella senza elemento, non succede nulla
    --   se un senpai si trova sulla stessa casella di un elemento, l'elemento viene rimosso perchè abbiamo incrementato la relativa virtu
    filterCoordinate = filter (`notElem` map posizione moved)

    incrementValori :: Senpai -> Senpai -- Prende un senpai e lo ritorna incrementato se la posizione è presente in uno dei rispettivi array
    incrementValori (Senpai v posizione) =
      Senpai
        { valori =
            Valori
              { umilta = umilta v `incrementIf` (posizione `elem` u),
                coraggio = coraggio v `incrementIf` (posizione `elem` c),
                gentilezza = gentilezza v `incrementIf` (posizione `elem` g),
                rispetto = rispetto v `incrementIf` (posizione `elem` r)
              },
          posizione = posizione
        }

runFor :: Table -> Int -> IO Table
runFor table 0 = return table
runFor table for
  | (length . senpai) table <= 1 = return table
  | (null . allCoordinatesValori) table = return table
  | otherwise = rest
  where
    rest = do gong table `runFor` (for - 1)

runIterations :: Table -> Int -> IO [Table]
runIterations table 1 = return [gong table]
runIterations table for
  | length (senpai table) <= 1 = return [table]
  | null (allCoordinatesValori table) = return [table]
  | otherwise = fmap (table :) rest
  where
    rest = do gong table `runIterations` (for - 1)

main :: IO ()
main = do
  putStr "Inserisci il file: "
  fileName <- getLine

  isAllowed <- try (openFile fileName ReadMode) :: IO (Either SomeException Handle)

  case isAllowed of
    Left ex -> putStrLn "Error Opening File"
    Right handle -> do
      file <- hGetContents handle

      let table = createPlayTable file

      putStr
        "Scegli l'esecuzione inserendo il valore corrispondente: \
        \\n - `$num` per mostrare tutte le iterazioni fino a num\
        \\n - `num` per mostrare solo l'esecuzione num\
        \\n - qualsiasi altro carattere per mostrare solo la configurazione finale\
        \\n > "
      inputStr <- getLine

      putStrLn "\n   ~ Esecuzione ~"

      ex <- case inputStr of
        ('$' : n) -> do
          ex <- table `runIterations` fromMaybe 0 (readMaybe n)
          print ex
        _ -> do
          ex <- table `runFor` fromMaybe 0 (readMaybe inputStr)
          print ex

      hClose handle