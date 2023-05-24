module Main where

import Control.Exception (SomeException (SomeException), catch, try)
import Control.Monad (guard)
import Data.Foldable (minimumBy)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Ord
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, openFile)
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

instance Show Table where -- Show indica che Table appartiene un'interfaccia Show per definire la funzione show
  show :: Table -> String
  show (Table n senpai u c g r) = "\nN = " ++ show n ++ "\nD = {" ++ "\n\tS = " ++ show senpai ++ ",\n\tU = " ++ show u ++ ",\n\tC = " ++ show c ++ ",\n\tG = " ++ show g ++ ",\n\tR = " ++ show r ++ "\n}"

instance Ord Senpai where
  (>) :: Senpai -> Senpai -> Bool
  (>) senpai otherSenpai
    | points senpai == points otherSenpai = positionPoints senpai > positionPoints otherSenpai
    | otherwise = points senpai > points otherSenpai
  (<=) = (not .) . (>)

positionPoints :: Senpai -> Int
positionPoints (Senpai _ (n, m)) = ((n + m) * (n + m - 1) `div` 2) + n - m

points :: Senpai -> Int
points (Senpai valori _) = umilta valori + coraggio valori + gentilezza valori + rispetto valori

defaultSenpai :: (Int, Int, Int, Int, Int, Int) -> Senpai
defaultSenpai (x, y, u, c, g, r) =
  Senpai
    { valori =
        Valori
          { umilta = u,
            coraggio = c,
            gentilezza = g,
            rispetto = r
          },
      posizione = (x, y)
    }

-- ! utilities
incrementedIf :: Int -> Bool -> Int
incrementedIf valore condizione = valore + if condizione then 1 else 0

complexityCalc :: Num a => (a, a) -> (a, a) -> a
complexityCalc (x, y) (a, b) = (a - x) ^ 2 + (b - y) ^ 2

closerToNearestIn :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -- ritorna la prima posizione incrementata di 1 verso la seconda
closerToNearestIn (x, y) globalValori
  | x < fst nearest = (x + 1, y)
  | x > fst nearest = (x - 1, y)
  | otherwise = (x, y + if y < snd nearest then 1 else if y > snd nearest then -1 else 0)
  where
    nearest = (minimumBy . comparing) (complexityCalc (x, y)) globalValori -- prende il valore con complessità minore

-- ! Run
-- Muove il senpai verso la prossima meta
moveSenpai :: [(Int, Int)] -> Senpai -> Senpai
moveSenpai globalValori senpai = senpai {posizione = posizione senpai `closerToNearestIn` globalValori}

combat :: [Senpai] -> [Senpai]
combat senpai = filter toKeep . map incrementValoriSenpai $ senpai
  where
    toKeep senpai = Just senpai > nearestSenpai senpai -- determina se il senpai possiede più punti totali di quello immediatamente vicino se other senpai è Nothing ritorna True

    incrementValoriSenpai :: Senpai -> Senpai -- Prende un senpai e lo ritorna incrementando ogni valore se il senpai più vicino si trova a complessità 1 ed inoltre il relativo valore è maggiore di quello dell'avversario
    incrementValoriSenpai senpai = senpai {valori = incrementValori (valori senpai) (fmap valori . nearestSenpai $ senpai)}
    
    nearestSenpai :: Senpai -> Maybe Senpai -- il senpai vicino 1, nothing altrimenti
    nearestSenpai (Senpai _ pos) = do
      guard (length near == 1)
      return $ head near
      where
        -- ? TODO attenzione, qui ho definito che i punti vengono controllati anche se la posizione è la stessa, cosa probabilmente corretta ma mancante nella documentazione
        near = filter (\s -> 1 == (complexityCalc pos . posizione) s) senpai

    incrementValori :: Valori -> Maybe Valori -> Valori
    incrementValori valori Nothing = valori
    incrementValori (Valori u c g r) (Just (Valori u' c' g' r')) =
      Valori
        { umilta = u `incrementedIf` (u > u'),
          coraggio = c `incrementedIf` (c > c'),
          gentilezza = g `incrementedIf` (g > g'),
          rispetto = r `incrementedIf` (r > r')
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
    moved = (map . moveSenpai $ u ++ c ++ g ++ r) senpai
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
              { umilta = umilta v `incrementedIf` (posizione `elem` u),
                coraggio = coraggio v `incrementedIf` (posizione `elem` c),
                gentilezza = gentilezza v `incrementedIf` (posizione `elem` g),
                rispetto = rispetto v `incrementedIf` (posizione `elem` r)
              },
          posizione = posizione
        }

-- ! Metodi init table
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

tableInit :: String -> Table
tableInit file =
  Table
    { dimensione = n,
      senpai = if areOk (map posizione _senpai) then _senpai else error "Un senpai ha una coordinata superiore alla dimensione",
      u = if areOk _u then _u else error "Un oggetto 'u' ha una coordinata superiore alla dimensione",
      c = if areOk _c then _c else error "Un oggetto 'c' ha una coordinata superiore alla dimensione",
      g = if areOk _g then _g else error "Un oggetto 'g' ha una coordinata superiore alla dimensione",
      r = if areOk _r then _r else error "Un oggetto 'r' ha una coordinata superiore alla dimensione"
    }
  where
    l = lines file
    n = read (drop 4 (head l)) :: Int

    areOk :: [(Int, Int)] -> Bool
    areOk [] = True
    areOk ((x, y) : xs) = x < n && y < n && areOk xs

    _senpai = map defaultSenpai (senpaiLineFormat (l !! 2))
    _u = lineFormat (l !! 3)
    _c = lineFormat (l !! 4)
    _g = lineFormat (l !! 5)
    _r = lineFormat (l !! 6)

-- ! Avvio e lettura file
runFor :: Table -> Int -> IO Table
runFor table 0 = return table
runFor table for
  | (length . senpai) table <= 1 = return table
  | null $ u table ++ c table ++ g table ++ r table = return table
  | otherwise = rest
  where
    rest = do gong table `runFor` (for - 1)

runIterations :: Table -> Int -> IO [Table]
runIterations table 1 = return [gong table]
runIterations table for
  | length (senpai table) <= 1 = return [table]
  | null $ u table ++ c table ++ g table ++ r table = return [table]
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

      let table = tableInit file

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