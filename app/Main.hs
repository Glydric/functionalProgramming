module Main where
import Data.List (elemIndex, isSuffixOf)
import Data.Maybe (fromMaybe)
import System.IO
import Data.Char
import System.Exit (exitFailure)
import Control.Exception (try, SomeException(SomeException), catch)
import Control.Exception.Base (evaluate)

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

data Table = Table {
    dimensione :: Int,
    senpai :: [Senpai],
    u :: [(Int, Int)],
    c :: [(Int, Int)],
    g :: [(Int, Int)],
    r :: [(Int, Int)]
}


baseSenpai :: (Int, Int) -> Senpai
baseSenpai cord = Senpai{
    valori = Valori {
        humility = 0,
        courage = 0,
        kindness = 0,
        respect = 0
    },
    posizione = cord
}


asArray :: Valori -> [Int]
asArray (Valori h c k r) = [h, c, k, r]

nextItem :: Table -> Senpai -> [(Int, Int)]
nextItem t s = case fromMaybe 0 index of
        0 -> u t
        1 -> c t
        2 -> g t
        3 -> r t
    where
    index = elemIndex (minimum array) array
    array = asArray (valori s)

split :: Char -> [Char] -> [String]
split char str = case break (== char) str of
  (a, char : b) -> a  : split char b
  (a, "") -> [a]


lineFormat:: String -> [(Int, Int)]
lineFormat string = read s :: [(Int, Int)] where
    dropped = drop 1 (dropWhile (/= '{') string)
    taked = take (length dropped - 2) dropped
    s = "[" ++ format taked ++ "]"

format :: [Char] -> [Char]
format ('{' : a : ',' : b : '}' : ',' : other) = '(' : a : ',' : b : ')' : ',' : format other
format ('{' : a : ',' : b : '}' : other) = '(' : a : ',' : b : ')' : other
format s = s


createPlayTable :: String -> Table
createPlayTable file = Table {
        dimensione = n,
        senpai = _s,
        u = _u,
        c = _c,
        g = _g,
        r = _r
    } where
        n = read (drop 4 (head l)) :: Int

        l = lines file

        sValues = lineFormat (l !! 2)
        _s = map baseSenpai sValues
        _u = lineFormat (l !! 3)
        _c = lineFormat (l !! 4)
        _g = lineFormat (l !! 5)
        _r = lineFormat (l !! 6)
        

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

            let actualNext = nextItem table

            let p = map posizione (senpai table)

            print (p)
            hClose handle
        -- il read non esegue il cast in maniera corretta e quindi otteniamo l'errore Prelude.read
