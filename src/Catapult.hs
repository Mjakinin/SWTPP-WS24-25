module Catapult where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Board

--importe für sortBy GithubCopilot QuickFix Funktion
import Data.List(sortBy)

data Move = Move {start :: Pos, target :: Pos}

instance Show Move where
  show (Move (Pos startC startR) (Pos targetC targetR)) = [startC] ++ show startR ++ "-" ++ [targetC] ++ show targetR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1)) (Move (Pos sc2 sr2) (Pos tc2 tr2)) =
    sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2

-- #################################################################################################
-- ################## IMPLEMENT flagMoves :: Board -> Player -> [Move]           ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

data Catapult = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq)

---Funktion aus ChatGPT-o1 auf PDF Seite 20-28
flagMoves :: Board -> Player -> [Move]
flagMoves board player
  | not (isValidStartConfiguration board player) = []
  | otherwise = map (\pos -> Move pos pos) validPositions
  where
    validPositions = case player of
      White -> [Pos col 9 | col <- ['b'..'i']]
      Black -> [Pos col 0 | col <- ['b'..'i']]

isValidStartConfiguration :: Board -> Player -> Bool
isValidStartConfiguration board player =
  case player of
    White -> board == buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
    Black -> board `elem`
                [ buildBoard (show x ++ "W" ++ show (9 - x) ++ "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/")
                | x <- [1..8 :: Int]
                ]


-- #################################################################################################
-- ################## IMPLEMENT generalMoves :: Board -> Player -> Pos -> [Move] ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################
---Funktion aus ChatGPT-o1 auf PDF Seite 30-32
generalMoves :: Board -> Player -> Pos -> [Move]
generalMoves board player currentPos =
  case lookupPosition board currentPos of
    Just (General p) | p == player -> validMoves
    _ -> []
  where
    directions = [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]
    validMoves = [Move currentPos target | dir <- directions, let target = moveDirection currentPos dir, isValidMove target]

    isValidMove :: Pos -> Bool
    isValidMove target = case lookupPosition board target of
      Just Empty -> True
      _ -> False

    moveDirection :: Pos -> Dir -> Pos
    moveDirection (Pos col row) dir =
      case dir of
        North     -> Pos col (row + 1)
        South     -> Pos col (row - 1)
        East      -> Pos (succ col) row
        West      -> Pos (pred col) row
        NorthEast -> Pos (succ col) (row + 1)
        SouthEast -> Pos (succ col) (row - 1)
        NorthWest -> Pos (pred col) (row + 1)
        SouthWest -> Pos (pred col) (row - 1)

lookupPosition :: Board -> Pos -> Maybe Cell
lookupPosition board (Pos col row) =
  if rowIndex < 0 || rowIndex >= length board || colIndex < 0 || colIndex >= length (head board)
  then Nothing
  else Just ((board !! rowIndex) !! colIndex)
  where
    rowIndex = 9 - row
    colIndex = fromEnum col - fromEnum 'a'

-- #################################################################################################
-- ################## IMPLEMENT soldierMoves :: Board -> Player -> Pos -> [Move] ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################
---Funktion aus ChatGPT-o1 auf PDF Seite 43-55
soldierMoves :: Board -> Player -> Pos -> [Move]
soldierMoves board player currentPos =
  case lookupPosition board currentPos of
    Just (Soldier p) | p == player -> normalMoves ++ attackMoves ++ retreatMoves
    _ -> []
  where
    forward = if player == White then -1 else 1
    backward = -forward

    normalMoves = [Move currentPos target | dir <- [North, East, West], 
                                                let target = moveDirection currentPos dir, 
                                                isValidMove target] ---aus ChatGPT-o1 auf PDF Seite 44-46

    attackMoves = [Move currentPos target | dir <- [North, NorthEast, NorthWest],
                                            let target = moveDirection currentPos dir,
                                            isOpponentAttack target] ---aus ChatGPT-o1 auf PDF Seite 46-48

    retreatMoves =
      if isThreatened currentPos
      then [Move currentPos target | dir <- [South, SouthWest, SouthEast],
                                     let intermediate = moveDirection currentPos dir,
                                     let target = moveDirection intermediate dir,
                                     isPathClear intermediate target] ---aus ChatGPT-o1 auf PDF Seite 48-55
      else []

    isValidMove :: Pos -> Bool
    isValidMove target = case lookupPosition board target of
      Just Empty -> True
      _ -> False

    --selber geschrieben
    isOpponentAttack :: Pos -> Bool
    isOpponentAttack target = case lookupPosition board target of
      Just (Soldier p) -> p /= player
      Just (General p) -> p /= player
      Just (Flag p) -> p /= player
      _ -> False

    isOpponent :: Pos -> Bool
    isOpponent target = case lookupPosition board target of
      Just (Soldier p) -> p /= player
      _ -> False

    isThreatened :: Pos -> Bool
    isThreatened pos = any (\dir -> isOpponent (moveDirection pos dir))[North, NorthEast, NorthWest]

    isPathClear :: Pos -> Pos -> Bool
    isPathClear intermediate target =
      isValidMove intermediate && isValidMove target

    moveDirection :: Pos -> Dir -> Pos
    moveDirection (Pos col row) dir =
      case dir of
        North     -> Pos col (row + forward)
        South     -> Pos col (row + backward)
        East      -> Pos (succ col) row
        West      -> Pos (pred col) row
        NorthEast -> Pos (succ col) (row + forward)
        NorthWest -> Pos (pred col) (row + forward)
        SouthEast -> Pos (succ col) (row + backward)
        SouthWest -> Pos (pred col) (row + backward)

-- #################################################################################################
-- ################## IMPLEMENT catapultMoves :: Board -> Player -> Pos -> [Move]  #################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################
---Funktion aus ChatGPT-o1 auf PDF Seite 56-82
catapultMoves :: Board -> Player -> Pos -> [Move]
catapultMoves board player currentPos
  | not (isSoldier  board player currentPos) = []
  | otherwise = concatMap (catapultMovesForCatapult board player currentPos) catapults
  where
    catapults = findCatapults board player currentPos

---Funktion aus ChatGPT-o1 auf PDF Seite 56-66
findCatapults :: Board -> Player -> Pos -> [[Pos]]
findCatapults board player currentPos =
  case lookupPosition board currentPos of
    Just (Soldier p) | p == player ->
      removeDuplicates $ map sortPositions $ concatMap (findCatapultInDirection board player currentPos) directions
  where
    directions = [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]

findCatapultInDirection :: Board -> Player -> Pos -> Dir -> [[Pos]]
findCatapultInDirection board player currentPos dir =
  let
    firstPos = moveDirection currentPos dir
    secondPos = moveDirection firstPos dir
    oppositeDir = oppositeDirection dir
    backwardPos = moveDirection currentPos oppositeDir
  in
    case (lookupPosition board firstPos, lookupPosition board secondPos, lookupPosition board backwardPos) of
      (Just (Soldier p1), Just (Soldier p2), _) | p1 == player && p2 == player ->
        [[currentPos, firstPos, secondPos]]
      (Just (Soldier p1), _, Just (Soldier p3)) | p1 == player && p3 == player ->
        [[backwardPos, currentPos, firstPos]]
      _ -> []

sortPositions :: [Pos] -> [Pos]
sortPositions = sortBy (\(Pos c1 r1) (Pos c2 r2) -> compare (r1, c1) (r2, c2))

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs


---Funktion aus ChatGPT-o1 auf PDF Seite 66-68
obereSpitzefinden :: Player -> [Pos] -> Pos
obereSpitzefinden player catapult =
  case player of
    Black -> last (sortLexicographically catapult)
    White -> head (sortLexicographically catapult)

untereSpitzefinden :: Player -> [Pos] -> Pos
untereSpitzefinden player catapult =
  case player of
    Black -> head (sortLexicographically catapult)
    White -> last (sortLexicographically catapult)

sortLexicographically :: [Pos] -> [Pos]
sortLexicographically = sortBy compareLexicographically

compareLexicographically :: Pos -> Pos -> Ordering
compareLexicographically (Pos col1 row1) (Pos col2 row2)
  | row1 == row2 = compare col1 col2
  | otherwise    = compare row1 row2


---Funktion aus ChatGPT-o1 auf PDF Seite 69-73
catapultMovesForCatapult :: Board -> Player -> Pos -> [Pos] -> [Move]
catapultMovesForCatapult board player currentPos catapult =
  let
    obereSpitze  = obereSpitzefinden player catapult
    untereSpitze = untereSpitzefinden player catapult
    direction = calcDir currentPos (findDifferentPos currentPos catapult)

    shots =
      if currentPos == obereSpitze || currentPos == untereSpitze
      then catapultShots board player currentPos obereSpitze untereSpitze direction catapult
      else []
  in
    if currentPos == obereSpitze || currentPos == untereSpitze
      then catapultMove board currentPos direction ++ shots
      else shots

findDifferentPos :: Pos -> [Pos] -> Pos
findDifferentPos current (x:xs)
  | current /= x = x
  | otherwise = findDifferentPos current xs

catapultMove :: Board -> Pos -> Dir -> [Move]
catapultMove board pos dir =
  let move = moveDirection (moveDirection (moveDirection pos dir) dir) dir
  in if isValidMove board move
     then [Move pos move]
     else []

isValidMove :: Board -> Pos -> Bool
isValidMove board pos = case lookupPosition board pos of
  Just Empty -> True
  _ -> False


---Funktion aus ChatGPT-o1 auf PDF Seite 74-80
catapultShots :: Board -> Player -> Pos -> Pos -> Pos -> Dir -> [Pos] -> [Move]
catapultShots board player currentPos obereSpitze untereSpitze dir catapult =
  if not (isGeneralNear board catapult player)
  then []
  else
    let
      shotPositions = catapultShotList currentPos obereSpitze untereSpitze dir
      ownCatapult = any (`elem` catapult) shotPositions
      shotDir = if ownCatapult then oppositeDirection dir else dir
      shots = catapultShotList currentPos obereSpitze untereSpitze shotDir

      validShots = filter (isOpponent board player) shots
    in
        [Move currentPos target | target <- validShots]


---Funktion aus ChatGPT-o1 auf PDF Seite 80-82
isGeneralNear :: Board -> [Pos] -> Player -> Bool
isGeneralNear board catapult player =
  any (isNearGeneral board player) catapult

isNearGeneral :: Board -> Player -> Pos -> Bool
isNearGeneral board player pos =
  any (compareGeneral board player) (neighborPositions pos)

compareGeneral :: Board -> Player -> Pos -> Bool
compareGeneral board player pos = case lookupPosition board pos of
  Just (General p) -> p == player
  _ -> False

neighborPositions :: Pos -> [Pos]
neighborPositions pos =
  [ moveDirection pos dir |
    dir <- [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]
  ]


isOpponent :: Board -> Player -> Pos -> Bool
isOpponent board player pos = case lookupPosition board pos of
  Just (Soldier p)  -> p /= player
  Just (General p)  -> p /= player
  Just (Flag p)     -> p /= player
  _                 -> False

catapultShotList :: Pos -> Pos -> Pos -> Dir -> [Pos]
catapultShotList currentPos obereSpitze untereSpitze dir
  | currentPos == obereSpitze =
      [ moveDirection (moveDirection untereSpitze (oppositeDirection dir)) (oppositeDirection dir)
      , moveDirection (moveDirection (moveDirection untereSpitze (oppositeDirection dir)) (oppositeDirection dir)) (oppositeDirection dir)
      ]
  | currentPos == untereSpitze =
      [ moveDirection (moveDirection obereSpitze dir) dir
      , moveDirection (moveDirection (moveDirection obereSpitze dir) dir) dir
      ]

calcDir :: Pos -> Pos -> Dir
calcDir (Pos col1 row1) (Pos col2 row2)
  | rowDiff > 0 && colDiff > 0  = NorthEast
  | rowDiff > 0 && colDiff < 0  = NorthWest
  | rowDiff < 0 && colDiff > 0  = SouthEast
  | rowDiff < 0 && colDiff < 0  = SouthWest
  | rowDiff > 0 && colDiff == 0 = North
  | rowDiff < 0 && colDiff == 0 = South
  | rowDiff == 0 && colDiff > 0 = East
  | rowDiff == 0 && colDiff < 0 = West
  where
    rowDiff = row2 - row1
    colDiff = fromEnum col2 - fromEnum col1

--selber geschrieben
oppositeDirection :: Dir -> Dir
oppositeDirection dir = case dir of
  North     -> South
  South     -> North
  East      -> West
  West      -> East
  NorthEast -> SouthWest
  SouthEast -> NorthWest
  NorthWest -> SouthEast
  SouthWest -> NorthEast

--selber geschrieben bzw von oben übernommen
moveDirection :: Pos -> Dir -> Pos
moveDirection (Pos col row) dir =
  case dir of
    NorthEast -> Pos (succ col) (row + 1)
    SouthEast -> Pos (succ col) (row - 1)
    NorthWest -> Pos (pred col) (row + 1)
    SouthWest -> Pos (pred col) (row - 1)
    East      -> Pos (succ col) row
    West      -> Pos (pred col) row
    North     -> Pos col (row + 1)
    South     -> Pos col (row - 1)

isSoldier :: Board -> Player -> Pos -> Bool
isSoldier  board player currentPos = case lookupPosition board currentPos of
  Just (Soldier p) -> p == player
  _ -> False
-- #################################################################################################
-- ################## IMPLEMENT playerWon :: Board -> Maybe Player               ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################
---Funktion aus ChatGPT-o1 auf PDF Seite 87-89
playerWon :: Board -> Player -> Bool
playerWon board player =
  hasFlag player board && hasGeneralOnBoard player board &&
    let opponentPlayer = opponent player
        noMoves = null (listMoves board opponentPlayer)
    in
        not (hasFlag opponentPlayer board) ||
        not (hasGeneralOnBoard opponentPlayer board) ||
        noMoves
  where
    hasFlag :: Player -> Board -> Bool
    hasFlag p b = any (hasPlayerFlag p) (concat b)
    
    hasPlayerFlag :: Player -> Cell -> Bool
    hasPlayerFlag p (Flag p') = p == p'
    hasPlayerFlag _ _ = False
    
    hasGeneralOnBoard :: Player -> Board -> Bool
    hasGeneralOnBoard p b = any (hasPlayerGeneral p) (concat b)
    
    hasPlayerGeneral :: Player -> Cell -> Bool
    hasPlayerGeneral p (General p') = p == p'
    hasPlayerGeneral _ _ = False

    opponent :: Player -> Player
    opponent White = Black
    opponent Black = White

-- #################################################################################################
-- ################## IMPLEMENT listMoves :: Board -> Player -> [Move]           ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################
---Funktion aus ChatGPT-o1 auf PDF Seite 84-86
listMoves :: Board -> Player -> [Move]
listMoves board player
    | isValidStartConfiguration board player = flagMoves board player
    | otherwise = concatMap (getMovesForPosition board player) allPositions
    where
        allPositions = [Pos col row | col <- ['a'..'j'], row <- [0..9]]
                
        getMovesForPosition :: Board -> Player -> Pos -> [Move]
        getMovesForPosition b p pos = case lookupPosition b pos of
            Just (General _)  -> generalMoves b p pos
            Just (Soldier _)  -> soldierMoves b p pos ++ catapultMoves b p pos
            _                 -> []
