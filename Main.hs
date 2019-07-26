module Main where
import Control.Monad.Loops (iterateM_)
import Control.Monad (join)
import Data.Foldable (fold)
import Data.List (intercalate, transpose, findIndex, find)
import Data.Maybe (isNothing, isJust, fromMaybe)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import System.Exit (exitSuccess)

data Piece = Red | Yellow deriving (Show, Eq)

antipode :: Piece -> Piece
antipode Red = Yellow
antipode Yellow = Red

type Seven t = (t, t, t, t, t, t, t)

type SevenBySeven t = Seven (Seven t)

type BoardLine = Seven (Maybe Piece)

type BoardMatrix = Seven BoardLine

data BoardIndex = One | Two | Three | Four | Five | Six | Seven deriving (Show, Enum, Bounded)

type BoardPosition = (BoardIndex, BoardIndex)

atIndex :: BoardIndex -> Seven t -> t
atIndex One   (a, _, _, _, _, _, _) = a
atIndex Two   (_, a, _, _, _, _, _) = a
atIndex Three (_, _, a, _, _, _, _) = a
atIndex Four  (_, _, _, a, _, _, _) = a
atIndex Five  (_, _, _, _, a, _, _) = a
atIndex Six   (_, _, _, _, _, a, _) = a
atIndex Seven (_, _, _, _, _, _, a) = a

atPos :: BoardPosition -> SevenBySeven t -> t
atPos (x, y) = atIndex y . atIndex x

mirror :: BoardIndex -> BoardIndex
mirror One = Seven
mirror Two = Six
mirror Three = Five
mirror Four = Four
mirror Five = Three
mirror Six = Two
mirror Seven = One

boardIndices :: [BoardIndex]
boardIndices = [One .. Seven]

boardIndex :: Int -> BoardIndex
boardIndex = (boardIndices !!)

updateTuple :: BoardIndex -> t -> Seven t -> Seven t
updateTuple One   n (a, b, c, d, e, f, z) = (n, b, c, d, e, f, z)
updateTuple Two   n (a, b, c, d, e, f, z) = (a, n, c, d, e, f, z)
updateTuple Three n (a, b, c, d, e, f, z) = (a, b, n, d, e, f, z)
updateTuple Four  n (a, b, c, d, e, f, z) = (a, b, c, n, e, f, z)
updateTuple Five  n (a, b, c, d, e, f, z) = (a, b, c, d, n, f, z)
updateTuple Six   n (a, b, c, d, e, f, z) = (a, b, c, d, e, n, z)
updateTuple Seven n (a, b, c, d, e, f, z) = (a, b, c, d, e, f, n)

updateBoard :: BoardPosition -> t -> SevenBySeven t -> SevenBySeven t
updateBoard (x, y) piece board = updateTuple y (updateTuple x piece (atIndex y board)) board

boardIndexToInt :: BoardIndex -> Int
boardIndexToInt ix = atIndex ix (1, 2, 3, 4, 5, 6, 7)

boardIndexToListIndex :: BoardIndex -> Int
boardIndexToListIndex ix = atIndex ix (0, 1, 2, 3, 4, 5, 6)

boardMatrixToList :: SevenBySeven t -> [[t]]
boardMatrixToList = map tupleToList . tupleToList

columns :: SevenBySeven t -> [[t]]
columns = transpose . boardMatrixToList

tupleToList :: Seven t -> [t]
tupleToList (a, b, c, d, e, f, g) = [ a, b, c, d, e, f, g ]

emptyLine :: BoardLine
emptyLine = (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

emptyBoard :: BoardMatrix
emptyBoard = (emptyLine, emptyLine, emptyLine, emptyLine, emptyLine, emptyLine, emptyLine)

showBoard :: BoardMatrix -> String
showBoard board = intercalate "\n" $ map lineToString (tupleToList board) where
  lineToString :: BoardLine -> String
  lineToString line = intercalate " | " $ map optPieceToString (tupleToList line)

  optPieceToString :: Maybe Piece -> String
  optPieceToString Nothing = " "
  optPieceToString (Just Yellow) = "Y"
  optPieceToString (Just Red) = "R"

insertPiece :: BoardIndex -> Piece -> BoardMatrix -> Maybe BoardMatrix
insertPiece columnIndex piece board = let
    boardList = boardMatrixToList board
    column = (transpose boardList) !! boardIndexToListIndex columnIndex
    availableIndex :: Maybe BoardIndex
    availableIndex = boardIndex . ((length column - 1) -) <$> findIndex isNothing (reverse column)
    newBoard = (\ yPos -> updateBoard (columnIndex, yPos) (Just piece) board) <$> availableIndex
  in newBoard

insertPieceIfPossible :: BoardIndex -> Piece -> BoardMatrix -> BoardMatrix
insertPieceIfPossible ix p b = fromMaybe b (insertPiece ix p b)

updateBoardMulti :: BoardMatrix -> [(Maybe Piece, BoardPosition)] -> BoardMatrix
updateBoardMulti board = foldl (\ board (piece, pos) -> updateBoard pos piece board) board

checkWinLine :: [Maybe Piece] -> Maybe Piece
checkWinLine ps = checkWinLine_ ps 0 Red where
  checkWinLine_ :: [Maybe Piece] -> Int -> Piece -> Maybe Piece
  checkWinLine_ _             4            streakCol = Just streakCol
  checkWinLine_ []            _            _         = Nothing
  checkWinLine_ (Nothing:ps)  _            _         = checkWinLine ps
  checkWinLine_ ((Just c):ps) streakCount  streakCol
    | c == streakCol = checkWinLine_ ps (streakCount+1) streakCol
    | otherwise = checkWinLine_ ps 1 c

diagonalCoords :: [[BoardPosition]]
diagonalCoords = (concatMap diagsRightUp [minBound .. maxBound]) ++ (concatMap diagsRightDown [minBound .. maxBound]) where
  diagsRightUp ix = [zip [minBound..ix] (reverse [minBound..ix]), zip [ix..maxBound] (reverse [ix..maxBound])]
  diagsRightDown ix = [zip [minBound..ix] [mirror ix..maxBound], zip [ix..maxBound] [minBound..mirror ix]]

diagonals :: BoardMatrix -> [[Maybe Piece]]
diagonals board = map (map $ flip atPos board) diagonalCoords

checkWin :: BoardMatrix -> Maybe Piece
checkWin board = join $ find isJust $ map checkWinLine (boardMatrixToList board ++ columns board ++ diagonals board)

main :: IO ()
main = iterateM_ step (Yellow, emptyBoard) where
  step (cur, board) = do
    putStrLn $ showBoard board
    pos <- readLn
    let ix = boardIndex pos
    let nextState = case insertPiece ix cur board of
          Nothing -> (cur, board)
          Just b -> (antipode cur, b)
    case checkWin $ snd nextState of
      Nothing -> return nextState
      Just col -> putStrLn ((show col) ++ " wins!") *> exitSuccess
