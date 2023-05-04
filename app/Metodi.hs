-- ! Metodi
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
