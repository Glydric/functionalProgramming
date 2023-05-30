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

instance Show Table where -- Show indica che Table appartiene un'interfaccia Show per definire la funzione show
  show :: Table -> String
  show (Table n senpai u c g r) = "\nN = " ++ show n ++ "\nD = {" ++ "\n\tS = " ++ show senpai ++ ",\n\tU = " ++ show u ++ ",\n\tC = " ++ show c ++ ",\n\tG = " ++ show g ++ ",\n\tR = " ++ show r ++ "\n}"

instance Eq Valori where
  (==) (Valori u c g r) (Valori u' c' g' r') = u == u' && c == c' && g == g' && r == r'
  (/=) = (not .) . (==)

instance Eq Senpai where
  (==) (Senpai v1 p1) (Senpai v2 p2) = v1 == v2 && p1 == p2
  (/=) = (not .) . (==)

instance Ord Senpai where
  (<=) = (not .) . (>)
  (>) :: Senpai -> Senpai -> Bool
  (>) senpai otherSenpai
    | points senpai == points otherSenpai = positionPoints senpai > positionPoints otherSenpai
    | otherwise = points senpai > points otherSenpai

positionPoints :: Senpai -> Int
positionPoints (Senpai _ (n, m)) = ((n + m) * (n + m - 1) `div` 2) + n - m

points :: Senpai -> Int
points (Senpai valori _) = umilta valori + coraggio valori + gentilezza valori + rispetto valori

makeSenpai :: (Int, Int, Int, Int, Int, Int) -> Senpai
makeSenpai (x, y, u, c, g, r) =
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

complexityBetween :: Num a => (a, a) -> (a, a) -> a
complexityBetween (x, y) (a, b) = (a - x) ^ 2 + (b - y) ^ 2

closerToNearestIn :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -- ritorna la prima posizione incrementata di 1 verso la seconda
closerToNearestIn (x, y) globalValori
  | (x - fst nearest) ^ 2 > (y - snd nearest) ^ 2 = (x + if x < fst nearest then 1 else if x > fst nearest then -1 else 0, y)
  | otherwise = (x, y + if y < snd nearest then 1 else if y > snd nearest then -1 else 0)
  where
    nearest = (minimumBy . comparing) (complexityBetween (x, y)) globalValori -- prende il valore con complessità minore

-- ! Run
-- Muove il senpai verso la prossima meta
moveSenpai :: [(Int, Int)] -> Senpai -> Senpai
moveSenpai globalValori senpai = senpai {posizione = posizione senpai `closerToNearestIn` filter (/= posizione senpai) globalValori}

nearestSenpai :: [Senpai] -> Senpai -> Maybe Senpai -- il senpai vicino 1 o 0, nothing altrimenti
nearestSenpai incremented actual = do
  guard . not . null $ near
  return . head $ near
  where
    -- !TODO attenzione, qui ho definito che i punti vengono controllati anche se la posizione è la stessa, cosa probabilmente corretta ma mancante nella documentazione
    near = filter (\s -> 1 >= posizione actual `complexityBetween` posizione s) . filter (/= actual) $ incremented

incrementValoriSenpai :: [Senpai] -> Senpai -> Senpai -- Prende un senpai e lo ritorna incrementando ogni valore se il senpai più vicino si trova a complessità 1 ed inoltre il relativo valore è maggiore di quello dell'avversario
incrementValoriSenpai all actual = actual {valori = incrementValori (valori actual) (fmap valori . nearestSenpai all $ actual)}

incrementValori :: Valori -> Maybe Valori -> Valori
incrementValori valori Nothing = valori
incrementValori (Valori u c g r) (Just (Valori u' c' g' r')) =
  Valori
    { umilta = u `incrementedIf` (u > u'),
      coraggio = c `incrementedIf` (c > c'),
      gentilezza = g `incrementedIf` (g > g'),
      rispetto = r `incrementedIf` (r > r')
    }

combat :: [Senpai] -> [Senpai]
combat senpai = filter (\s -> Just s > nearestSenpai incremented s) incremented
  where
    -- determina se il senpai possiede più punti totali di quello immediatamente vicino se other senpai è Nothing ritorna True
    incremented = map (incrementValoriSenpai senpai) senpai

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
    _coordinatesToMove = if null $ u ++ c ++ g ++ r then map posizione senpai else u ++ c ++ g ++ r
    moved = (map . moveSenpai $ _coordinatesToMove) senpai
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
format "{}" = "[]"
format "{}," = "[]"
format ('{' : '{' : some) = '[' : '(' : format some
format ('{' : some) = '(' : format some
format ('}' : some) = ')' : format some
format (s : some) = s : format some

-- gestisce la riga raw di un file.dojo per poter essere formattata e la trasforma nella relativa valutazione
lineFormat :: Read a => String -> a
lineFormat ('\t' : _ : '=' : values) = read (format values)

makeTable :: [String] -> Table
makeTable file =
  Table
    { dimensione = n,
      senpai = if areOk $ map posizione _senpai then _senpai else error "Un senpai si trova fuori dalla tabella",
      u = if areOk $ lines !! 3 then lines !! 3 else error "Un oggetto 'u' si trova fuori dalla tabella",
      c = if areOk $ lines !! 4 then lines !! 4 else error "Un oggetto 'c' si trova fuori dalla tabella",
      r = if areOk $ lines !! 6 then lines !! 6 else error "Un oggetto 'r' si trova fuori dalla tabella",
      g = if areOk $ lines !! 5 then lines !! 5 else error "Un oggetto 'g' si trova fuori dalla tabella"
    }
  where
    n = read . drop 4 . head $ file

    _senpai = map makeSenpai . lineFormat $ file !! 2

    areOk = and . fmap (\(x, y) -> x < n && y < n)

    lines = map lineFormat file

-- ! Avvio e lettura file
runIterations :: Table -> [Table]
runIterations table
  | (length . senpai) table <= 1 = [table]
  | otherwise = table : (runIterations . gong $ table)

run :: Table -> Table
run table
  | (length . senpai) table <= 1 = table
  | otherwise = run $ gong table

main :: IO ()
main = do
  putStr "Inserisci il file: "
  fileName <- getLine

  isAllowed <- try $ openFile fileName ReadMode :: IO (Either SomeException Handle)

  case isAllowed of
    Left ex -> putStrLn $ "Errore nell'apertura del file " ++ show ex
    Right handle -> do
      putStr
        "Scegli l'esecuzione inserendo il valore corrispondente: \
        \\n - `$` per mostrare tutte le iterazioni\
        \\n - qualsiasi altro carattere per mostrare solo la configurazione finale\
        \\n > "
      inputStr <- getLine

      file <- hGetContents handle

      let table = makeTable . lines $ file
      putStrLn "\n   ~ Esecuzione ~"

      case inputStr of
        ('$' : _) -> print $ runIterations table
        _ -> print $ run table

      hClose handle