{-# LANGUAGE CPP, ScopedTypeVariables, FlexibleInstances #-}


import System.IO
import Data.ByteString (find)
import Debug.Trace ( trace )
import Data.List ( sort )


type SudokuValue = Int
type SudokuIndex = Int
type SudokuRowIndex = Int
type SudokuColIndex = Int
type SudokuCellIndex = Int
type SudokuQuestion = [[SudokuValue]]
type Coordinate = (Int, Int)
type SudokuRecord = (Coordinate, SudokuValue)
type SudokuRow = [SudokuRecord]
type SudokuCol = [SudokuRecord]
type SudokuCell = [SudokuRecord]
type SudokuFlatBoard = [SudokuRecord]
type SudokuBoard = [[SudokuRecord]]


indexQuestion :: [SudokuValue] -> [(SudokuIndex, SudokuValue)]
indexQuestion = zip (iterate (+ 1) 0)

indexedToBoard :: [(SudokuIndex, SudokuValue)] -> [(Coordinate, SudokuValue)]
indexedToBoard = map (\(index, value) -> ((index `div` 9 + 1, index `mod` 9 + 1), value))

creatFlatBoard :: [SudokuValue] -> SudokuFlatBoard
creatFlatBoard question = indexedToBoard (indexQuestion question)

flatBoardToAnswer :: SudokuFlatBoard -> [SudokuValue]
flatBoardToAnswer = map snd

answerToString :: [SudokuValue] -> String
answerToString [] = ""
answerToString [x] = show x
answerToString (x:xs) = show x ++ " " ++ answerToString xs

getAnswerString :: SudokuFlatBoard -> String
getAnswerString flatBoard = answerToString (flatBoardToAnswer flatBoard)

getQuestion :: IO [Int]
getQuestion = do
    inputFile <- openFile "question.txt" ReadMode
    questionString <- hGetLine inputFile
    let question = (map read $ words questionString :: [Int])
    hClose inputFile
    return question

writeAnswer :: String -> IO ()
writeAnswer answer = do
    outputFile <- openFile "answer.txt" WriteMode
    hPutStrLn outputFile answer
    hClose outputFile

questionBoard :: SudokuQuestion = [
        [0, 0, 0, 0, 0, 8, 3, 0, 0],
        [0, 0, 0, 0, 2, 4, 0, 9, 0],
        [0, 0, 4, 0, 7, 0, 0, 0, 6],
        [0, 0, 0, 0, 0, 3, 0, 7, 9],
        [7, 5, 0, 0, 0, 0, 0, 8, 4],
        [9, 2, 0, 5, 0, 0, 0, 0, 0],
        [4, 0, 0, 0, 9, 0, 1, 0, 0],
        [0, 3, 0, 4, 6, 0, 0, 0, 0],
        [0, 0, 5, 8, 0, 0, 0, 0, 0]]

originalBoard :: SudokuBoard
originalBoard :: SudokuBoard = 
    zipWith
    (\rowIndex rowValues -> 
        zipWith 
        (\colIndex value -> ((rowIndex, colIndex), value))
        [1..9] rowValues)
    [1..9] questionBoard

originalFlatBoard :: SudokuFlatBoard
originalFlatBoard :: SudokuFlatBoard = concat originalBoard

class Locatable a where
    getRowIndex :: a -> SudokuRowIndex
    getColIndex :: a -> SudokuColIndex
    getCellIndex :: a -> SudokuCellIndex

    onRowOf :: SudokuRowIndex -> a -> Bool
    onColOf :: SudokuColIndex -> a -> Bool
    onCellOf :: SudokuCellIndex -> a -> Bool

instance Locatable Coordinate where
    getRowIndex = fst
    getColIndex = snd
    getCellIndex coordinate = 3 * ((getRowIndex coordinate - 1) `div` 3) + ((getColIndex coordinate - 1) `div` 3) + 1

    onRowOf row coordinate = fst coordinate == row
    onColOf col coordinate = snd coordinate == col
    onCellOf cell coordinate = getCellIndex coordinate == cell
    

instance Locatable SudokuRecord where
    getRowIndex = fst . getRecordCoordinate
    getColIndex = snd . getRecordCoordinate
    getCellIndex record = 3 * ((getRowIndex record - 1) `div` 3) + ((getColIndex record - 1) `div` 3) + 1

    onRowOf row record = fst (getRecordCoordinate record) == row
    onColOf col record = snd (getRecordCoordinate record) == col
    onCellOf cell record = getCellIndex record == cell

isZero :: Integral a => a -> Bool
isZero x = x == 0

isNotZero :: Integral a => a -> Bool
isNotZero = not . isZero

recordIsZero :: SudokuRecord -> Bool
recordIsZero = isZero . snd

recordIsNotZero :: SudokuRecord -> Bool
recordIsNotZero = isNotZero . snd

getRecordValue :: SudokuRecord -> SudokuValue
getRecordValue = snd

getRecordCoordinate :: SudokuRecord -> Coordinate
getRecordCoordinate = fst

isFinished :: SudokuFlatBoard -> Bool
isFinished = all recordIsNotZero

isNotFinished :: SudokuFlatBoard -> Bool
isNotFinished = not . isFinished

isInvalidCoordinate :: Coordinate -> Bool
isInvalidCoordinate = (==) (0, 0)

findUnassigned :: SudokuFlatBoard -> Coordinate
findUnassigned [] = (0, 0)
findUnassigned (record: rest)
    | recordIsZero record = getRecordCoordinate record
#ifdef DEBUG
        `debug` ("non-0 record: " ++ show record)
#endif
    | otherwise = findUnassigned rest

getRow :: SudokuFlatBoard -> SudokuRowIndex -> SudokuRow
getRow flatBoard row = filter (onRowOf row) flatBoard

getCol :: SudokuFlatBoard -> SudokuColIndex -> SudokuCol
getCol flatBoard col = filter (onColOf col) flatBoard

getCell :: SudokuFlatBoard -> SudokuCellIndex -> SudokuCell
getCell flatBoard cell = filter (onCellOf cell) flatBoard

findRecord :: SudokuFlatBoard -> Coordinate -> SudokuRecord
findRecord [] coordinate = error "WTF"
findRecord (record:rest) coordinate
    | getRecordCoordinate record == coordinate = record
    | otherwise = findRecord rest coordinate

recordHasValueWith :: SudokuValue -> SudokuRecord -> Bool
recordHasValueWith value record = value == getRecordValue record

rowHasValueWith :: SudokuValue -> SudokuRow -> Bool
rowHasValueWith value = any (recordHasValueWith value)

colHasValueWith :: SudokuValue -> SudokuCol -> Bool
colHasValueWith value = any (recordHasValueWith value)

cellHasValueWith :: SudokuValue -> SudokuCell -> Bool
cellHasValueWith value = any (recordHasValueWith value)

isSafe :: SudokuFlatBoard -> Coordinate -> SudokuValue -> Bool
isSafe board coordinate value
    | rowHasValueWith value (getRow board $ getRowIndex coordinate) = False
    | colHasValueWith value (getCol board $ getColIndex coordinate) = False
    | cellHasValueWith value (getCell board $ getCellIndex coordinate) = False
    | otherwise = True

setRecord :: SudokuFlatBoard -> Coordinate -> SudokuValue -> SudokuFlatBoard
setRecord flatBoard coordinate value = (coordinate, value): filter (\record -> getRecordCoordinate record /= coordinate) flatBoard


debug :: a -> String -> a
debug thing message = message `trace` thing

data SolveState = Finish | Wrong deriving (Enum, Eq, Show)

solve :: SudokuFlatBoard -> Coordinate -> SudokuValue -> (SudokuFlatBoard, SolveState)
solve flatBoard coordinate value
    | isFinished flatBoard = (flatBoard, Finish) -- Only last node have chance to execute this brance, returing answer and Finish flag to father
#ifdef DEBUG
        `debug` "Finish"
#endif 
    | value > 9 = ([], Wrong) -- No way to go further, returing unmodified board and Wrong flag to request father try another number (Father has initial board, does not necessarryly to return a board here)
#ifdef DEBUG
        `debug` ("Ge9" ++ show (coordinate, value))
#endif
    | not $ isSafe flatBoard coordinate value = incrementTrial -- Not a valid trial, try another one
#ifdef DEBUG
        `debug` ("Not safe: " ++ show (coordinate, value))
#endif
    | hasNextSolution == Wrong = incrementTrial -- Child has no way to go further, try another value now
#ifdef DEBUG
        `debug` "Go back"
#endif 
    | hasNextSolution == Finish = nextTrial -- Child has the answer, get the answer and propagate both answer and Finish flag
#ifdef DEBUG
        `debug` "Finish from deep"
#endif
    | otherwise = nextTrial -- Found a temporaryly safe value, tell children to try further
#ifdef DEBUG
        `debug` ("Safe: " ++ show (coordinate, value))
#endif
        where 
            newBoard = setRecord flatBoard coordinate value
            nextTrial = solve newBoard (findUnassigned newBoard) 1
            hasNextSolution = snd nextTrial
#ifdef DEBUG
                `debug` "Try next"
#endif
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