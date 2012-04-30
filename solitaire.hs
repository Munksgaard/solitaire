import Shuffle

data Rank = Ace | Two | Three | Four | Five | Six | Seven
          | Eight | Nine | Ten | Jack | Queen | King
            deriving (Enum, Show, Read, Bounded, Eq, Ord)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Enum, Show, Read, Bounded, Eq, Ord)

data Card = Card Suit Rank
            deriving (Eq, Ord, Bounded)

otherSuit :: Suit -> Suit
otherSuit Clubs = Spades
otherSuit Spades = Clubs
otherSuit Hearts = Diamonds
otherSuit Diamonds = Hearts

--{-
data GameState = GameState { stock :: ([Card], [Card])
                           , tableaus :: [([Card],[Card])]
                           , foundations :: [Card]
                           } deriving (Show)
--}

instance Enum Card where
    toEnum n = let d = n `divMod` 13
               in Card (toEnum (fst d)) (toEnum (snd d))
    fromEnum c = 13 * fromEnum(suit c) + fromEnum(rank c)

instance Show Card where
    show (Card s r) = (showRank r) ++ (showSuit s)

newDeck = [(Card Clubs Ace) .. (Card Spades King)]

suit :: Card -> Suit
suit (Card s _) = s

rank :: Card -> Rank
rank (Card _ r) = r

showSuit :: Suit -> String
showSuit s = (take 1) (show s)

showRank :: Rank -> String
showRank f = (take 1) (drop (fromEnum f) "A23456789TJQK")

sumNums :: (Integral b, RealFrac a) => a -> b
sumNums n = round $ n/2 * (n+1)


initializeGame :: [Card] -> GameState
initializeGame deck = let ts = [(splitAt 1 $ fst $ splitAt (round x + 1)
                                $ snd $ splitAt (sumNums x) deck)
                                | x <- [0..6]]
                          fs = []
                          st = ([], snd $ splitAt (sumNums 7) deck)
                 in GameState {tableaus=ts, foundations=fs,
                               stock=st}

onTableau :: Card -> Card -> Bool
onTableau (Card suit1 rank1) (Card suit2 rank2) =
    suit1 /= suit2 &&
    otherSuit suit1 /= suit2 &&
    succ rank1 == rank2

flipStock up down = 
    (\(newUp,newDown) -> (reverse newUp ++ up, newDown)) $ splitAt 3 down

nextStock :: GameState -> GameState
nextStock GameState {stock=(up, []), tableaus=ts, foundations=fs} = 
    GameState {stock=flipStock [] $ reverse up, tableaus=ts, foundations=fs}
nextStock GameState {stock=(up,down), tableaus=ts, foundations=fs} =
    GameState {stock=flipStock up down, tableaus=ts, foundations=fs}

{-
moveCard :: GameState -> Card -> Card -> GameState
moveCard state card to
    | card `elem` (fst $ stock GameState) =
        
--}
