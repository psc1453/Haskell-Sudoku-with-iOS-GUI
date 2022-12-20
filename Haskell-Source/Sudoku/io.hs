module Sudoku.IO (
    indexQuestion,
    indexedToBoard,
    creatFlatBoard,
    flatBoardToAnswer,
    answerToString,
    getAnswerString,
    getQuestion,
    writeAnswer
) where


    import System.IO
    import Sudoku.Types


    indexQuestion :: [SudokuValue] -> [(SudokuIndex, SudokuValue)]
    indexQuestion question = zip (iterate (+ 1) 0) question

    indexedToBoard :: [(SudokuIndex, SudokuValue)] -> [(Coordinate, SudokuValue)]
    indexedToBoard questionWithIndex = map (\(index, value) -> ((index `div` 9 + 1, index `mod` 9 + 1), value)) questionWithIndex

    creatFlatBoard :: [SudokuValue] -> SudokuFlatBoard
    creatFlatBoard question = indexedToBoard (indexQuestion question)

    flatBoardToAnswer :: SudokuFlatBoard -> [SudokuValue]
    flatBoardToAnswer flatBoard = map snd flatBoard

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