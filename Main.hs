module Main where

data Piece = Red | Yellow deriving Show

type BoardLine = (Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece)

type BoardMatrix = (BoardLine, BoardLine, BoardLine, BoardLine, BoardLine, BoardLine, BoardLine)


main :: IO ()
main = putStrLn "Hello, world!"
