module Main where
import Control.Monad.Loops (iterateM_)
import Data.List (intercalate, transpose, findIndex)
import Data.Maybe (isNothing)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)

data Piece = Red | Yellow deriving Show

antipode :: Piece -> Piece
antipode Red = Yellow
antipode Yellow = Red

type BoardLine = (Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece)

type BoardMatrix = (BoardLine, BoardLine, BoardLine, BoardLine, BoardLine, BoardLine, BoardLine)

data BoardIndex = One | Two | Three | Four | Five | Six | Seven deriving (Show, Enum)

type BoardPosition = (BoardIndex, BoardIndex)

at :: BoardIndex -> (t, t, t, t, t, t, t) -> t
at One   (a, _, _, _, _, _, _) = a
at Two   (_, a, _, _, _, _, _) = a
at Three (_, _, a, _, _, _, _) = a
at Four  (_, _, _, a, _, _, _) = a
at Five  (_, _, _, _, a, _, _) = a
at Six   (_, _, _, _, _, a, _) = a
at Seven (_, _, _, _, _, _, a) = a

boardIndices :: [BoardIndex]
boardIndices = [One .. Seven]
boardIndex :: Int -> BoardIndex
boardIndex = (boardIndices !!)

updateTuple :: BoardIndex -> t -> (t, t, t, t, t, t, t) -> (t, t, t, t, t, t, t)
updateTuple One   n (a, b, c, d, e, f, z) = (n, b, c, d, e, f, z)
updateTuple Two   n (a, b, c, d, e, f, z) = (a, n, c, d, e, f, z)
updateTuple Three n (a, b, c, d, e, f, z) = (a, b, n, d, e, f, z)
updateTuple Four  n (a, b, c, d, e, f, z) = (a, b, c, n, e, f, z)
updateTuple Five  n (a, b, c, d, e, f, z) = (a, b, c, d, n, f, z)
updateTuple Six   n (a, b, c, d, e, f, z) = (a, b, c, d, e, n, z)
updateTuple Seven n (a, b, c, d, e, f, z) = (a, b, c, d, e, f, n)

updateBoard :: BoardPosition -> Maybe Piece -> BoardMatrix -> BoardMatrix
updateBoard (x, y) piece board = updateTuple x (updateTuple y piece (at y board)) board

boardIndexToInt :: BoardIndex -> Int
boardIndexToInt ix = at ix (1, 2, 3, 4, 5, 6, 7)

boardIndexToListIndex :: BoardIndex -> Int
boardIndexToListIndex ix = at ix (0, 1, 2, 3, 4, 5, 6)

boardMatrixToList :: BoardMatrix -> [[Maybe Piece]]
boardMatrixToList = map tupleToList . tupleToList

tupleToList :: (t, t, t, t, t, t, t) -> [t]
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
    column = boardList !! boardIndexToListIndex columnIndex
    availableIndex :: Maybe Int
    availableIndex = findIndex isNothing (column)
    newBoard = (\ yPos -> updateBoard (columnIndex, boardIndex yPos) (Just piece) board) <$> availableIndex
  in newBoard

main :: IO ()
main = do
  flip iterateM_ (Yellow, emptyBoard) (\(cur, board) -> do
    putStrLn $ showBoard board
    pos <- readLn
    let ix = boardIndex pos
    return $ case insertPiece ix cur board of
      Nothing -> (cur, board)
      Just b -> (antipode cur, b))

