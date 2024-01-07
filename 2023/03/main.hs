import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)

type Width = Int

type X = Int

type Y = Int

type Coordinate = (Y, X)

type Item = (Char, Width)

type Cell = (Coordinate, Item)

type Boundary = (Coordinate, Coordinate)

sample :: String
sample =
    unlines
        [ "467..114.."
        , "...*......"
        , "..35..633."
        , "......#..."
        , "617*......"
        , ".....+.58."
        , "..592....."
        , "......755."
        , "...$.*...."
        , ".664.598.."
        ]

main :: IO ()
main = interact run

run :: String -> String
run input =
    -- "part 1 - total: " ++ show total
    unlines $ map show parsed
  where
    indexed = enumerate $ lines input
    parsed = map (\(y, line) -> (y, line, parse line (y, 0) [])) indexed

analyseInput :: String -> String
analyseInput input =
    unlines $ map mapY indexed
  where
    indexed = enumerate $ lines input
    mapX y (x, char) = showCell (y, x, char)
    mapY (y, line) = unwords $ map (mapX y) $ enumerate line

parse :: [Char] -> Coordinate -> [Cell] -> [Cell]
parse [] loc cells = cells -- eol
parse ('.' : line) loc cells = parse line (loc >+ (0, 1)) cells -- space char
parse (char : line) loc cells
    -- continued digit char
    | isDigit char && isDigitCell (lastCell cells) = case cells of
        (last : cells)
            --  continue previous
            | isDigit $ fst $ snd last -> parse line nextLoc (incCellWidth last : cells)
            -- new digit
            | otherwise -> parse line nextLoc (newDigitCell : cells)
        -- new digit char
        [] -> parse line nextLoc [newDigitCell]
    -- probably a symbol char
    | otherwise = parse line nextLoc (newDigitCell : cells)
  where
    nextLoc = loc >+ (0, 1)
    last = listToMaybe cells
    newDigitCell = (loc, (char, 0))

-- coordinate utils

within :: Boundary -> Coordinate -> Bool
within ((topLeftY, topLeftX), (bottomRightY, bottomRightX)) (y, x) =
    topLeftY <= y && topLeftX <= x && bottomRightY >= y && bottomRightX >= x

bounding :: Coordinate -> Width -> Boundary
bounding char width = (char >+ (-1, -1), char >+ (1, width + 1))

infixl 9 >+
(>+) :: Coordinate -> Coordinate -> Coordinate
(y, x) >+ (a, b) = (y + a, x + b)

-- cell utils

incCellWidth :: Cell -> Cell
incCellWidth (loc, (char, width)) = (loc, (char, width + 1))

lastCell :: [Cell] -> Maybe Cell
lastCell [] = Nothing
lastCell [cell] = Just cell

isDigitCell :: Maybe Cell -> Bool
isDigitCell cell = case cell of
    Nothing -> False
    Just (_, (char, _)) -> isDigit char

showCell :: (Y, X, Char) -> String
showCell (y, x, char) = discriminate char ++ "(" ++ show y ++ "," ++ show x ++ "," ++ [char] ++ ")"

cellWidth :: Cell -> Int
cellWidth (_, (_, w)) = w

cellChar :: Cell -> Char
cellChar (_, (c, _)) = c

-- misc

enumerate :: [item] -> [(Int, item)]
enumerate = zip [0 ..]

isSymbol :: Char -> Bool
isSymbol c
    | c == '.' = False
    | isDigit c = False
    | otherwise = True

discriminate :: Char -> String
discriminate '.' = "Nil"
discriminate char
    | isDigit char = "Int"
    | otherwise = "Sym"
