module Main where

data Piece = Red | Yellow deriving Show

type BoardLine = (Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece)

type BoardMatrix = (BoardLine, BoardLine, BoardLine, BoardLine, BoardLine, BoardLine, BoardLine)

tupleToList :: (t, t, t, t, t, t, t) -> [t]
tupleToList (a, b, c, d, e, f, g) = [ a, b, c, d, e, f, g ]

emptyLine :: BoardLine
emptyLine = (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

emptyBoard :: BoardMatrix
emptyBoard = (emptyLine, emptyLine, emptyLine, emptyLine, emptyLine, emptyLine, emptyLine)

main :: IO ()
main = putStrLn "Hello, world!"
