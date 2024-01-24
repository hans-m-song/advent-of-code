import Data.List (stripPrefix)

type Winning = [Int]
type Revealed = [Int]
type Card = (Winning, Revealed)
type Points = [Int]
type Score = Int
type Count = Int
type Limit = Int
type Total = Int

sample :: String
sample =
    unlines
        [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
        , "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
        , "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
        , "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
        , "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
        , "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
        ]

main :: IO ()
main = do
    contents <- getContents
    let cards = map parseCard $ lines contents
        counts = map countWinningNumbers cards
        scores = map calculateCardScore counts
    putStrLn "-- part 1"
    putStrLn $ "scores: " ++ show scores
    putStrLn $ "total: " ++ show (sum scores)
    putStrLn "-- part 2"
    putStrLn $ "total: " ++ show (countCards scores (length scores))

parseCard :: String -> Card
parseCard input = (winning, revealed)
  where
    contents = dropWhile (== ' ') $ drop 1 $ dropWhile (/= ':') input
    winning = map atoi $ words $ takeWhile (/= '|') contents
    revealed = map atoi $ words $ drop 1 $ dropWhile (/= '|') contents

countWinningNumbers :: Card -> Count
countWinningNumbers (winning, revealed) = length $ filter (`elem` winning) revealed

calculateCardScore :: Count -> Score
calculateCardScore count
    | count > 0 = 2 ^ (count - 1)
    | otherwise = 0

countCards :: Points -> Limit -> Count
-- no more to count
countCards [] _ = 0
-- finished counting copies
countCards _ 0 = 0
countCards (count : points) limit =
    1
        -- continue last count for last card
        + countCards points (limit - 1)
        -- start a new one for current card
        + countCards points count

atoi :: String -> Int
atoi = read
