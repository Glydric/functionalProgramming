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
