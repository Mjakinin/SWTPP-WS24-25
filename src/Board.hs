module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char
import Data.Char (chr, ord)

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = White | Black deriving Show
data Cell = Empty | General Player | Soldier Player | Flag Player deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) White White = True
  (==) Black Black = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Soldier p1) (Soldier p2) = p1 == p2
  (==) (General p1) (General p2) = p1 == p2
  (==) (Flag p1) (Flag p2) = p1 == p2
  (==) _ _ = False


-- ##############################################################################
-- ################## IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                   ###################
-- ################## - 1 Coverage Point                      ###################
-- #############################################################################
---Funktion aus ChatGPT-o1 auf PDF Seite 8-11
validateFEN :: String -> Bool
validateFEN fen = 
  let rows = splitOn '/' fen
  in '0' `notElem` fen && length rows == 10 && all validRow rows

validRow :: String -> Bool
validRow row = 
  let expandedRow = expandRow row
  in (length expandedRow == 10 || null expandedRow) && all validChar expandedRow

expandRow :: String -> String
expandRow [] = []
expandRow (x:xs)
  | isDigit x = replicate (digitToInt x) '1' ++ expandRow xs
  | otherwise = x : expandRow xs

validChar :: Char -> Bool
validChar c = c `elem` "bwBWgG1"

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimiter (c:cs)
  | c == delimiter = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitOn delimiter cs

-- ##############################################################################
-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 2 Functional Points                   ###################
-- ################## - 1 Coverage Point                      ###################
-- ##############################################################################
---Funktion aus ChatGPT-o1 auf PDF Seite 13-17
buildBoard :: String -> Board
buildBoard fen
  | validateFEN fen = map parseRow rows
  | otherwise = []
  where
    rows = splitOn '/' fen

parseRow :: String -> [Cell]
parseRow row
  | null row = replicate 10 Empty
  | otherwise = concatMap parsePiece (expandRow row)

parsePiece :: Char -> [Cell]
parsePiece c
  | isDigit c = replicate (digitToInt c) Empty
  | c == 'w'  = [Soldier White]
  | c == 'b'  = [Soldier Black]
  | c == 'g'  = [General White]
  | c == 'G'  = [General Black]
  | c == 'W'  = [Flag White]
  | c == 'B'  = [Flag Black]
