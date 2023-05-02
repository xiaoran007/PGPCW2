-- Represent different types of pieces
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
data PieceType = General | Guard | Elephant | Horse | Chariot | 
                 Cannon | Soldier deriving (Eq,Show)
data PieceColour = Black | Red deriving (Eq,Show)
type Piece = (PieceColour, PieceType)

-- Represent a position on the board
data BoardPosition = On Piece | Empty deriving (Eq,Show)
type Board = [[BoardPosition]]  -- represent a board
-- Index row (1 to 10) and column (1 to 9)
type Pos = (Int,Int)   -- represent a position index on the board
type Move = (Pos, Pos) -- represent a move from one position to another
type Path = [Pos]      -- represent a path as a sequence of positions

-- add a vector to update a position
addPos :: Pos -> (Int,Int) -> Pos
addPos (x,y) (a,b) = (x+a,y+b)

oppositeColour :: PieceColour -> PieceColour
oppositeColour Black = Red
oppositeColour Red = Black

-- Set up an empty board with 9 columns and 10 rows
emptyBoard :: Board
emptyBoard = replicate 10 (replicate 9 Empty)

-- This code can be used to display the pieces and board
strPiece :: Piece -> String
strPiece (Red, General)  = "G"
strPiece (Red, Guard)    = "A"
strPiece (Red, Elephant) = "E"
strPiece (Red, Horse)    = "H"
strPiece (Red, Chariot)  = "R"
strPiece (Red, Cannon)   = "C"
strPiece (Red, Soldier)  = "S"
strPiece (Black, General)  = "g"
strPiece (Black, Guard)    = "a"
strPiece (Black, Elephant) = "e"
strPiece (Black, Horse)    = "h"
strPiece (Black, Chariot)  = "r"
strPiece (Black, Cannon)   = "c"
strPiece (Black, Soldier)  = "s"
strBoardPosition :: BoardPosition -> String
strBoardPosition (On p) = strPiece p
strBoardPosition Empty = "."

delimit :: [a] -> [[a]] -> [[a]]
delimit v [] = []
delimit v (xs:xss) = xs : (foldr (\xs yss -> (v++xs) : yss) []) xss
strBoardRow :: [BoardPosition] -> String
strBoardRow xs = "  " ++ concat (delimit "--" (map strBoardPosition xs) )
rowMarks n
  | n==9 || n==2 = f "\\|/ "
  | n==8 || n==1 = f "/|\\ "
  | n==5 = ""
  | otherwise = "  " ++ concat (replicate 9 "|  ")
  where f str = "  " ++ concat (replicate 3 "|  ") ++ "| " ++ str ++ concat (replicate 4 "|  ")
strBoard :: Board -> String
strBoard [] = ""
strBoard (xs:xss) = strBoardRow xs ++ "\n" ++
  if length xss == 0 then "" 
                     else rowMarks (length xss) ++ "\n" ++ strBoard xss
  
showBoard :: Board -> IO()
showBoard xss = do putStrLn (strBoard xss)

-- Update the board
changeBoardPosition :: Board -> Pos -> BoardPosition -> Board
changeBoardPosition (xs:xss) (0,y) bp 
  = ( take y xs ++ (bp : drop (y+1) xs) ) : xss
changeBoardPosition (xs:xss) (x,y) bp 
  = xs : changeBoardPosition xss (x-1,y) bp
addPiece :: Board -> Pos -> Piece -> Board
addPiece xss pos p = changeBoardPosition xss pos (On p)
removePiece :: Board -> Pos -> Board
removePiece xss pos = changeBoardPosition xss pos Empty
addPieces :: Board -> [(Pos,Piece)] -> Board
addPieces b [] = b
addPieces b ((pos,piece):xs) = addPiece (addPieces b xs) pos piece

-- Starting positions on the board
startBoard :: Board
startBoard = addPieces emptyBoard 
  (concat [initialPieces isLeft pcol | 
            isLeft <- [True, False], pcol <- [Black, Red] ] )
  where
    initialPieces isLeft pcol =
      [ ((f 0, g 4),  (pcol, General)),
        ((f 0, g 3),  (pcol, Guard)),
      ((f 0, g 2),  (pcol, Elephant)),
      ((f 0, g 1),  (pcol, Horse)),
      ((f 0, g 0),  (pcol, Chariot)),
      ((f 2, g 1),  (pcol, Cannon)),
      ((f 3, g 0),  (pcol, Soldier)),
      ((f 3, g 2),  (pcol, Soldier)),
        ((f 3, g 4),  (pcol, Soldier))     ]
      where
        f x = if pcol==Black then x else 9-x
        g y = if isLeft then y else 8-y
    
-- Read board position
getPos :: Board -> Pos -> BoardPosition
getPos b (x,y) = (b!!x)!!y

-- Check if a board position is empty
isEmpty :: Board -> Pos -> Bool
isEmpty b pos = getPos b pos == Empty

-- get piece from a position, assuming it is occupied
getPiece :: Board -> Pos -> Piece
getPiece b pos = p
  where
    (On p) = getPos b pos

-- Check if a position is valid on the Chess board
isValidPos :: Pos -> Bool
isValidPos (x,y) = x>=0 && x<10 && y>=0 && y<9

-- Check if a position is in the palace
isPalace :: Pos -> Bool
isPalace (x,y) = (x<=2 || x>=7) && (y>=3 && y<=5)

-- Find a general of a particular colour on the given vertical;
-- return [] if it cannot be found on the vertical.
findGeneralOnVertical :: Board -> Int -> PieceColour -> [Pos]
findGeneralOnVertical b y pcol 
  = [(x,y) | x <- [0..9], not(isEmpty b (x,y)), 
                          getPiece b (x,y) == (pcol, General) ]

-- List of orthogonal directions
orthogonalDir :: [(Int,Int)]
orthogonalDir = [(-1,0), (1,0), (0,-1), (0,1)]

diDir :: [(Int, Int)]
diDir = [(-1, -1), (-1, 1), (1, -1), (1, 1)]

-- All orthogonal moves from a given position, in one direction, 
-- such that piece does not jump any other.
orthogonalMoves :: Board -> Pos -> (Int,Int) -> Path
orthogonalMoves b pos1 direction 
  | not (isValidPos pos2) = [] 
  | isEmpty b pos2 = pos2:orthogonalMoves b pos2 direction
  | otherwise      = [pos2]
  where
    pos2 = addPos pos1 direction

-- These are base moves for each piece, 
-- before considering if base move is valid.
baseMoves :: Board -> Piece -> Pos -> [Pos]
baseMoves b (pcol, General) from 
  = map (addPos from) orthogonalDir
    ++ findGeneralOnVertical b (snd from) (oppositeColour pcol)
baseMoves _ (Black,Soldier) from 
  | fst from<5 = [addPos from (1,0)]
  | otherwise = map (addPos from) [(1,0),(0,1),(0,-1)]
baseMoves _ (Red,Soldier) from 
  | fst from>=5 = [addPos from (-1,0)]
  | otherwise = map (addPos from) [(-1,0),(0,1),(0,-1)]
baseMoves b (_,Chariot) from 
  = concat (map (orthogonalMoves b from) orthogonalDir )
baseMoves b (pcol, Guard) from = 
  map (addPos from) ([(-1, -1), (-1, 1), (1, -1), (1, 1)])
baseMoves b (pcol, Elephant) from = 
  map (addPos from) ([(-2, -2), (-2, 2), (2, -2), (2, 2)])
baseMoves b (pcol, Horse) from = 
  map (addPos from) ([(-1, 2), (-1, -2), (1, 2), (1, -2), (-2, -1), (-2, 1), (2, -1), (2, 1)])
baseMoves b (pcol,Cannon) from 
  = concat (map (orthogonalMoves b from) orthogonalDir ) ++ (cannonJump b from (0, 1)) ++ (cannonJump b from (0, -1)) ++ (cannonJump b from (-1, 0)) ++ (cannonJump b from (1, 0))

  
-- Compute path of board positions for a given move, 
-- excluding first and last positions.
pathMove :: PieceType -> Move -> Path
pathMove Horse ( (x1,y1), (x2,y2) ) 
  | abs(x2-x1)==2 = [ (x1+signum(x2-x1),y1) ]
  | otherwise     = [ (x1,y1+signum(y2-y1)) ]
pathMove _  m = tail (pathMoveMain m)
  where
    pathMoveMain ((x1,y1),(x2,y2))
      | x1==x2 && y1==y2 = []
      | otherwise = (x1,y1):
        (pathMoveMain ((x1+signum(x2-x1), y1+signum(y2-y1) ), (x2,y2) ) )
 
-- Check if a path is empty
emptyPath :: Board -> PieceType -> Move -> Bool
emptyPath b pt m = all (isEmpty b) (pathMove pt m)

-- check validity of moves, which is special for each piece type
checkMove :: Board -> PieceType -> Move -> Bool
checkMove b General  m = emptyPath b General m && isPalace (snd m)
checkMove b Soldier ((x1, y1), (x2, y2)) =
  if (crossRiver ((x1, y1), (x2, y2)) && isValidPos (x2, y2)) 
    then if (isEmpty b (x2, y2))
      then if (x1 == x2 && y1 - y2 == 1 || y1 == y2 && x2 - x1 == 1 || x1 == x2 && y2 - y1 == 1)
        then True 
        else False  
    else if (hasSameColour b (x1, y1) (x2, y2))
      then if (x1 == x2 && y1 - y2 == 1 || y1 == y2 && x2 - x1 == 1 || x1 == x2 && y2 - y1 == 1)
        then True 
        else False
    else False 
  else if not (crossRiver ((x1, y1), (x2, y2)) && isValidPos (x2, y2)) then
    if (isEmpty b (x2, y2)) then
      if (y1 == y2 && x2 - x1 == 1) then
        True 
      else 
        False 
    else if (hasSameColour b (x1, y1) (x2, y2)) then
      if (y1 == y2 && x2 - x1 == 1) then
        True 
      else
        False
    else
      False 
  else
    False 
checkMove b Chariot ((x1, y1), (x2, y2)) = 
  if (isValidPos (x2, y2)) then
    if (isEmpty b (x2, y2)) then
      True 
    else if (hasSameColour b (x1, y1) (x2, y2)) then
      True 
    else 
      False 
  else
    False 
checkMove b Guard ((x1, y1), (x2, y2)) = 
  if (isValidPos (x2, y2) && isPalace (x2, y2)) then
    if (isEmpty b (x2, y2)) then 
      True 
    else if (hasSameColour b (x1, y1) (x2, y2)) then
      True 
    else
      False 
  else
    False 
checkMove b Elephant ((x1, y1), (x2, y2)) = 
  if (isValidPos (x2, y2) && isEmpty b (findMidElephant b (x1, y1) (x2, y2))) then 
    if (isEmpty b (x2, y2)) then
      True 
    else if (hasSameColour b (x1, y1) (x2, y2)) then
      True 
    else 
      False 
  else 
    False 
checkMove b Horse ((x1, y1), (x2, y2)) = 
  if (isValidPos (x2, y2) && isEmpty b (findMidHorse b (x1, y1) (x2, y2))) then
    if (isEmpty b (x2, y2)) then
      True 
    else if (hasSameColour b (x1, y1) (x2, y2)) then
      True 
    else 
      False 
  else
    False 





checkMove _ _ _ = True -- no further special check needed, as default

-- assume two positions are occupied and see if they are same colour
hasSameColour :: Board -> Pos -> Pos -> Bool
hasSameColour b pos1 pos2 = fst (getPiece b pos1) == fst (getPiece b pos2)
  
-- A move is valid if
-- (a) move is on board;
-- (b) move is possible for the piece;
-- (c) the end position is empty or has a different colour
-- Note that this function assumes a position is occupied
validMove :: Board -> Pos -> Pos -> Bool
validMove b from to = isValidPos to &&
                      checkMove b pt (from,to) && 
                      (isEmpty b to || not (hasSameColour b from to ) )
  where
    pt = snd (getPiece b from)

-- Generate a list of valid moves for the piece at the given position
validMoves :: Board -> Pos -> [Pos]
validMoves b pos 
  | getPos b pos == Empty = []
  | otherwise = filter (validMove b pos) (baseMoves b (getPiece b pos) pos)

-- Test checkmate scenarios
b4 = addPieces emptyBoard
  [( (1,3), (Black, General) ), 
   ( (0,3), (Black, Guard)   ),
   ( (2,1), (Red,   Horse) ),
   ( (9,4), (Red,   Chariot)  ),
   ( (3,3), (Red,   Soldier)  ),
   ( (8,5), (Red,   General)  )]
b5 = addPiece b4 (4,1) (Black, Chariot)
b6 = addPiece b5 (1,8) (Red,   Cannon)
b7 = addPiece b6 (1,5) (Black, Guard)
-- Tests:
-- isCheckmate b4 Black -- should be True
-- isCheckmate b5 Black -- should be False
-- isCheckmate b6 Black -- should be False
-- isCheckmate b7 Black -- should be True


crossRiver :: Move -> Bool 
crossRiver ((x1, _), (x2, _)) 
  | (x1 >= 0 && x1 <= 4) && (x2 >= 5 && x2 <= 9) = True
  | (x2 >= 0 && x2 <= 4) && (x1 >= 5 && x1 <= 9) = True 
  | otherwise  = False 


cannonJump :: Board -> Pos -> (Int, Int) -> [Pos]
cannonJump bd (x, y) (dir_a, dir_b) = 
  if (dir_a == -1 && dir_b == 0 && (firstPiece bd (x, y) (-1, 0)) /= (-1, -1)) 
    then if (firstPiece bd (firstPiece bd (x, y) (-1, 0)) (-1, 0) /= (-1, -1)) 
      then if not (hasSameColour bd (x, y) (firstPiece bd (firstPiece bd (x, y) (-1, 0)) (-1, 0))) 
        then [(firstPiece bd (firstPiece bd (x, y) (-1, 0)) (-1, 0))] 
        else []
      else []
  else if (dir_a == 1 && dir_b == 0 && (firstPiece bd (x, y) (1, 0)) /= (-1, -1))
    then if (firstPiece bd (firstPiece bd (x, y) (1, 0)) (1, 0) /= (-1, -1))
      then if not (hasSameColour bd (x, y) (firstPiece bd (firstPiece bd (x, y) (1, 0)) (1, 0))) 
        then [(firstPiece bd (firstPiece bd (x, y) (1, 0)) (1, 0))]
        else []
      else []
  else if (dir_a == 0 && dir_b == -1 && (firstPiece bd (x, y) (0, -1)) /= (-1, -1))
    then if (firstPiece bd (firstPiece bd (x, y) (0, -1)) (0, -1) /= (-1, -1))
      then if not (hasSameColour bd (x, y) (firstPiece bd (firstPiece bd (x, y) (0, -1)) (0, -1))) 
        then [(firstPiece bd (firstPiece bd (x, y) (0, -1)) (0, -1))]
        else []
      else []
  else if (dir_a == 0 && dir_b == 1 && (firstPiece bd (x, y) (0, 1)) /= (-1, -1))
    then if (firstPiece bd (firstPiece bd (x, y) (0, 1)) (0, 1) /= (-1, -1))
      then if not (hasSameColour bd (x, y) (firstPiece bd (firstPiece bd (x, y) (0, 1)) (0, 1))) 
        then [(firstPiece bd (firstPiece bd (x, y) (0, 1)) (0, 1))]
        else []
      else []
  else []


-- find first piece in given dir.
firstPiece :: Board -> Pos -> (Int, Int) -> (Int, Int)
firstPiece bd (x, y) (dir_a, dir_b) = 
  if (dir_a == -1 && dir_b == 0 && isValidPos (x-1, y)) then (if not (isEmpty bd (x-1, y)) then (x-1, y) else firstPiece bd (x-1, y) (dir_a, dir_b))
  else if (dir_a == 1 && dir_b == 0 && isValidPos (x+1, y)) then (if not (isEmpty bd (x+1, y)) then (x+1, y) else firstPiece bd (x+1, y) (dir_a, dir_b))
  else if (dir_a == 0 && dir_b == -1 && isValidPos (x, y-1)) then (if not (isEmpty bd (x, y-1)) then (x, y-1) else firstPiece bd (x, y-1) (dir_a, dir_b))
  else if (dir_a == 0 && dir_b == 1 && isValidPos (x, y+1)) then (if not (isEmpty bd (x, y+1)) then (x, y+1) else firstPiece bd (x, y+1) (dir_a, dir_b))
  else (-1, -1)


findMidElephant :: Board -> Pos -> Pos -> Pos
findMidElephant bd (x1, y1) (x2, y2) = 
  if (x1 - x2 > 0 && y1 - y2 > 0) then
    (x1-1, y1-1)
  else if (x1 - x2 > 0 && y1 - y2 < 0) then
    (x1-1, y1+1)
  else if (x1 - x2 < 0 && y1 - y2 > 0) then
    (x1+1, y1-1)
  else if (x1 - x2 < 0 && y1 - y2 < 0) then
    (x1+1, y1+1)
  else
    (x1, y1)


findMidHorse :: Board -> Pos -> Pos -> Pos
findMidHorse bd (x1, y1) (x2, y2) = 
  if (x1 - x2 == 2) then
    (x1-1, y1)
  else if (x1 - x2 == -2) then
    (x1+1, y1)
  else if (y1 - y2 == 2) then
    (x1, y1-1)
  else if (y1 - y2 == -2) then
    (x1, y1+1)
  else 
    (x1, y1)

-- red x: 7-9 y: 3-5 black x:0-2 y:3-5
findGeneral :: Board -> PieceColour -> Pos
findGeneral b pcl = 
  if (pcl == Red) then 
    if (checkPosGeneral b (7, 3)) then
      (7, 3)
    else if (checkPosGeneral b (7, 4)) then
      (7, 4)
    else if (checkPosGeneral b (7, 5)) then
      (7, 5)
    else if (checkPosGeneral b (8, 3)) then
      (8, 3)
    else if (checkPosGeneral b (8, 4)) then
      (8, 4)
    else if (checkPosGeneral b (8, 5)) then
      (8, 5)
    else if (checkPosGeneral b (9, 3)) then
      (9, 3)
    else if (checkPosGeneral b (9, 4)) then
      (9, 4)
    else if (checkPosGeneral b (9, 5)) then
      (9, 5)
    else (-1, -1)
  else
    if (checkPosGeneral b (0, 3)) then
      (0, 3)
    else if (checkPosGeneral b (0, 4)) then
      (0, 4)
    else if (checkPosGeneral b (0, 5)) then
      (0, 5)
    else if (checkPosGeneral b (1, 3)) then
      (1, 3)
    else if (checkPosGeneral b (1, 4)) then
      (1, 4)
    else if (checkPosGeneral b (1, 5)) then
      (1, 5)
    else if (checkPosGeneral b (2, 3)) then
      (2, 3)
    else if (checkPosGeneral b (2, 4)) then
      (2, 4)
    else if (checkPosGeneral b (2, 5)) then
      (2, 5)
    else (-1, -1)




checkPosGeneral :: Board -> Pos -> Bool
checkPosGeneral b (x, y) = 
  if (not (isEmpty b (x, y))) then
    if (checkPieceType (getPiece b (x, y)) == General) then
      True 
    else False 
  else 
    False 

checkPieceType :: Piece -> PieceType
checkPieceType (pcl, pty) = pty