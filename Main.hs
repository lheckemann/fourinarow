module Main where
import Data.List (intercalate)

data Piece = Red | Yellow deriving Show

type BoardLine = (Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece)

type BoardMatrix = (BoardLine, BoardLine, BoardLine, BoardLine, BoardLine, BoardLine, BoardLine)

data BoardIndex = One | Two | Three | Four | Five | Six | Seven

type BoardPosition = (BoardIndex, BoardIndex)

at :: BoardIndex -> (t, t, t, t, t, t, t) -> t
at One   (a, _, _, _, _, _, _) = a
at Two   (_, a, _, _, _, _, _) = a
at Three (_, _, a, _, _, _, _) = a
at Four  (_, _, _, a, _, _, _) = a
at Five  (_, _, _, _, a, _, _) = a
at Six   (_, _, _, _, _, a, _) = a
at Seven (_, _, _, _, _, _, a) = a

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

--insertPiece :: BoardIndex -> Piece -> BoardMatrix -> Maybe BoardMatrix
--insertPiece column piece board = _

main :: IO ()
main = putStrLn "Hello, world!"
