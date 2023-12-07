module Y2023.Day07 (main') where

import Lib.IO
import Lib.Parser qualified as P
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import Relude.Unsafe qualified as Unsafe

-----------------------
-- Type declarations --
-----------------------

data Bidding = Bidding
  { hand :: Hand,
    bid :: Int
  }

newtype Hand = Hand [Card] deriving (Show, Eq)

data Card = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | CJ | CQ | CK | CA deriving (Show, Eq, Ord, Enum, Bounded)

data HandType = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord, Enum, Bounded)

------------
-- Part 1 --
------------

solve1 :: [Bidding] -> Int
solve1 biddings = sum winnings
  where
    sortedBiddings = sortWith hand biddings
    winnings = zipWith (\rank bidding -> rank * bid bidding) [1 ..] sortedBiddings

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare a b = compareType a b <> compareCards a b

compareCards :: Hand -> Hand -> Ordering
compareCards = compare `on` un @[Card]

compareType :: Hand -> Hand -> Ordering
compareType = compare `on` getHandType

getHandType :: Hand -> HandType
getHandType (Hand hand) = handType
  where
    groupedCards :: [[Card]]
    groupedCards = sortWith (Down . length) . group . sort $ hand

    handType = case length groupedCards of
      1 -> FiveOfAKind
      2 -> if length (Unsafe.head groupedCards) == 4 then FourOfAKind else FullHouse
      3 -> if length (Unsafe.head groupedCards) == 3 then ThreeOfAKind else TwoPairs
      4 -> OnePair
      5 -> HighCard
      _ -> error "Impossible"

------------
-- Part 2 --
------------

solve2 :: [Bidding] -> Int
solve2 biddings = sum winnings
  where
    sortedBiddings = sortWith (WithJoker . hand) biddings
    winnings = zipWith (\rank bidding -> rank * bid bidding) [1 ..] sortedBiddings

newtype WithJoker a = WithJoker a

instance Eq a => Eq (WithJoker a) where
  (==) :: WithJoker a -> WithJoker a -> Bool
  (==) (WithJoker a) (WithJoker b) = a == b

instance Ord (WithJoker Card) where
  compare :: WithJoker Card -> WithJoker Card -> Ordering
  compare (WithJoker CJ) (WithJoker CJ) = EQ
  compare (WithJoker CJ) _ = LT
  compare _ (WithJoker CJ) = GT
  compare (WithJoker a) (WithJoker b) = compare a b

instance Ord (WithJoker Hand) where
  compare :: WithJoker Hand -> WithJoker Hand -> Ordering
  compare a b = compareTypeWithJoker a b <> compareCardsWithJoker a b

compareCardsWithJoker :: WithJoker Hand -> WithJoker Hand -> Ordering
compareCardsWithJoker = compare `on` un @[WithJoker Card]

compareTypeWithJoker :: WithJoker Hand -> WithJoker Hand -> Ordering
compareTypeWithJoker = compare `on` getHandTypeWithJoker

getHandTypeWithJoker :: WithJoker Hand -> HandType
getHandTypeWithJoker (WithJoker (Hand hand)) = handType
  where
    groupedCards :: [[Card]]
    groupedCards = sortWith (Down . length) . group . sort . filter (/= CJ) $ hand
    jokerCount = length $ filter (== CJ) hand

    handType = case length groupedCards of
      0 -> FiveOfAKind -- JJJJJ
      1 -> FiveOfAKind
      2 -> if length (Unsafe.head groupedCards) + jokerCount == 4 then FourOfAKind else FullHouse
      3 -> if length (Unsafe.head groupedCards) + jokerCount == 3 then ThreeOfAKind else TwoPairs
      4 -> OnePair
      5 -> HighCard
      _ -> error "Impossible"

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  biddings <- parseBidding <<$>> readFileLines "inputs/Y2023/Day07.txt" :: IO [Bidding]
  print $ solve1 biddings
  print $ solve2 biddings

parseBidding :: Text -> Bidding
parseBidding line = Bidding hand bid
  where
    [handStr, bidStr] = words line
    hand = Hand . fromList $ parseCard <$> toString handStr
    bid = readInt bidStr

parseCard :: Char -> Card
parseCard '2' = C2
parseCard '3' = C3
parseCard '4' = C4
parseCard '5' = C5
parseCard '6' = C6
parseCard '7' = C7
parseCard '8' = C8
parseCard '9' = C9
parseCard 'T' = C10
parseCard 'J' = CJ
parseCard 'Q' = CQ
parseCard 'K' = CK
parseCard 'A' = CA
parseCard _ = error "Invalid card"
