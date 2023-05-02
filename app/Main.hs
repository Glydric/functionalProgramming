module Main where
import Data.List (elemIndex, isSuffixOf)
import Data.Maybe (fromMaybe)
import System.IO
import Data.Char
import System.Exit (exitFailure)
import Control.Exception (try, SomeException (SomeException))
import Control.Exception.Base (evaluate)
import Control.Exception (catch)

data Valori = Valori{
    humility :: Int,
    courage :: Int,
    kindness :: Int,
    respect :: Int
}

data Senpai = Senpai {
    valori :: Valori,
    posizione :: (Int, Int)
}

baseSenpai :: Int -> Int -> Senpai
baseSenpai x y = Senpai{
    valori = Valori{
        humility = 0,
        courage = 0,
        kindness = 0,
        respect = 0
    },
    posizione = (x, y)
}

asArray :: Valori -> [Int]
asArray (Valori h c k r) = [h, c, k, r]

nextItem :: Senpai -> Int
nextItem s = fromMaybe 0 index where
    index = elemIndex (minimum array) array
    array = asArray (valori s)

firstSempai = baseSenpai 5 6

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
                "Y" -> putStrLn "Opening File... "
                "YES" -> putStrLn "Opening File... "
                _ -> exitFailure

    isAllowed <- try(openFile fileName ReadMode) :: IO (Either SomeException Handle)

    case isAllowed of
        Left ex -> putStrLn "Error Opening File"
        Right handle -> do 
            file <- hGetContents handle
            putStr file
            hClose handle
            print (nextItem firstSempai)



