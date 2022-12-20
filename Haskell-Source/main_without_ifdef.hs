{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}


import Data.ByteString (find)
import Debug.Trace ( trace )
import Data.List ( sort )


import Sudoku.Types
import Sudoku.IO
import Sudoku.Utils


debug :: a -> String -> a
debug thing message = message `trace` thing

data SolveState = Finish | Wrong deriving (Enum, Eq, Show)

solve :: SudokuFlatBoard -> Coordinate -> SudokuValue -> (SudokuFlatBoard, SolveState)
solve flatBoard coordinate value
    | isFinished flatBoard = (flatBoard, Finish) -- Only last node have chance to execute this brance, returing answer and Finish flag to father
    | value > 9 = ([], Wrong) -- No way to go further, returing unmodified board and Wrong flag to request father try another number (Father has initial board, does not necessarryly to return a board here)
    | not $ isSafe flatBoard coordinate value = incrementTrial -- Not a valid trial, try another one
    | hasNextSolution == Wrong = incrementTrial -- Child has no way to go further, try another value now
    | hasNextSolution == Finish = nextTrial -- Child has the answer, get the answer and propagate both answer and Finish flag
    | otherwise = nextTrial -- Found a temporaryly safe value, tell children to try further
        where 
            newBoard = setRecord flatBoard coordinate value
            nextTrial = solve newBoard (findUnassigned newBoard) 1
            hasNextSolution = snd nextTrial
            incrementTrial = solve flatBoard coordinate (value + 1)

solveSudoku :: SudokuFlatBoard -> SudokuFlatBoard
solveSudoku flatBoard = sort $ fst (solve flatBoard (findUnassigned flatBoard) 1)


main :: IO ()
main = do
    question <- getQuestion
    let questionFlatBoard = creatFlatBoard question
    let answerFlatBoard = solveSudoku questionFlatBoard
    let answer = getAnswerString answerFlatBoard
    writeAnswer answer