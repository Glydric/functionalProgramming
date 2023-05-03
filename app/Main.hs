{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Exception (SomeException (SomeException), catch, try)
import Control.Exception.Base (evaluate)
import Data.Char (toUpper)
import Data.List (elemIndex, isSuffixOf)
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, openFile)
import Text.ParserCombinators.ReadP (string)

data Valori = Valori
  { humility :: Int,
    courage :: Int,
    kindness :: Int,
    respect :: Int
  }

asArray :: Valori -> [Int]
asArray (Valori h c k r) = [h, c, k, r]

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

instance Show Senpai where
  show (Senpai _ posizione) = show posizione

-- Show indica che Table appartiene un'interfaccia Show per definire la funzione show
instance Show Table where
  show :: Table -> String
  show (Table n senpai u c g r) =
    "N = "
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

nextItem :: Table -> Senpai -> [(Int, Int)]
nextItem t s = case fromMaybe 0 index of
  0 -> u t
  1 -> c t
  2 -> g t
  3 -> r t
  where
    index = elemIndex (minimum array) array
    array = asArray (valori s)

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

      putStrLn file

      let table = createPlayTable file

      hClose handle

      let actualNext = nextItem table

      putStrLn ("Tabella Formattata: \n" ++ show table)
