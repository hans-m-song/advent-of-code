import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)

type Width = Int

type X = Int

type Y = Int

type Coordinate = (Y, X)

type Cell = (Coordinate, String)

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
    unlines
        [ "-- part 1"
        , "part numbers:   " ++ show partNumbers
        , "symbols:        " ++ show symbols
        , "total:          " ++ show total1
        , "-- part 2"
        , "gears:          " ++ show gears
        , "gear ratios:    " ++ show gearRatios
        , "part 2 - total: " ++ show total2
        ]
  where
    indexed = enumerate $ lines input
    parsed = map (\(y, line) -> parse line (y, 0) []) indexed
    numbers = filterNumbers parsed
    symbols = filterSymbols parsed
    partNumbers = filter (isPartNumber symbols) numbers
    total1 = foldr ((+) . atoi . snd) 0 partNumbers
    gears = filter ((== "*") . snd) symbols
    gearRatios = filter ((>= 2) . length) $ map (getGearRatios partNumbers) gears
    total2 = foldr ((+) . product . map (atoi . snd)) 0 gearRatios

identity :: String -> String
identity input =
    unlines $ map mapY indexed
  where
    indexed = enumerate $ lines input
    mapX y (x, char) = showCell (y, x, char)
    mapY (y, line) = unwords $ map (mapX y) $ enumerate line

parse :: String -> Coordinate -> [Cell] -> [Cell]
parse [] loc cells =
    -- end of line
    cells
parse (char : line) loc [] =
    -- first char
    parse line (loc >+ (0, 1)) [(loc, [char])]
parse (char : line) loc (lastCell : cells)
    -- continued char
    | isDigit char && isNumbers lastWord =
        parse line nextLoc (contCell : cells)
    | isSpace char && isSpaces lastWord =
        parse line nextLoc (contCell : cells)
    | isSymbol char && isSymbols lastWord =
        parse line nextLoc (contCell : cells)
    -- different char
    | otherwise =
        parse line nextLoc (newCell : lastCell : cells)
  where
    (lastLoc, lastWord) = lastCell
    nextLoc = loc >+ (0, 1)
    newCell = (loc, [char]) :: Cell
    contCell = (lastLoc, lastWord ++ [char]) :: Cell

filterSymbols :: [[Cell]] -> [Cell]
filterSymbols = filter (isSymbols . snd) . concat

filterNumbers :: [[Cell]] -> [Cell]
filterNumbers = filter (isNumbers . snd) . concat

isPartNumber :: [Cell] -> Cell -> Bool
isPartNumber symbols partNumber = any (near (bounding partNumber) . fst) symbols

getGearRatios :: [Cell] -> Cell -> [Cell]
getGearRatios partNumbers gear = filter (proximal gear) partNumbers

-- coordinate utils

proximal :: Cell -> Cell -> Bool
proximal centre radial = (radialBounds `near` fst centre) || (centreBounds `near` fst radial)
  where
    (radCoord, radWord) = radial
    radialBounds = bounding radial
    centreBounds = bounding centre

near :: Boundary -> Coordinate -> Bool
near ((topLeftY, topLeftX), (bottomRightY, bottomRightX)) (y, x) =
    topLeftY <= y && topLeftX <= x && bottomRightY >= y && bottomRightX >= x

bounding :: Cell -> Boundary
bounding (coord, word) =
    (coord >+ (-1, -1), coord >+ (1, length word))

infixl 9 >+
(>+) :: Coordinate -> Coordinate -> Coordinate
(y, x) >+ (a, b) = (y + a, x + b)

-- cell utils

showCell :: (Y, X, Char) -> String
showCell (y, x, char) =
    discriminate char ++ "(" ++ show y ++ "," ++ show x ++ "," ++ [char] ++ ")"

-- misc

atoi :: String -> Int
atoi = read

enumerate :: [item] -> [(Int, item)]
enumerate = zip [0 ..]

isNumbers :: String -> Bool
isNumbers = all isDigit

isSpace :: Char -> Bool
isSpace c = c == '.'

isSpaces :: String -> Bool
isSpaces = all isSpace

isSymbol :: Char -> Bool
isSymbol c
    | isSpace c = False
    | isDigit c = False
    | otherwise = True

isSymbols :: String -> Bool
isSymbols = all isSymbol

discriminate :: Char -> String
discriminate char
    | isSpace char = "Nil"
    | isDigit char = "Int"
    | otherwise = "Sym"
