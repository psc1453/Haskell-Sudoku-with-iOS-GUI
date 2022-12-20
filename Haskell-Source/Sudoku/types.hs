module Sudoku.Types (
    SudokuValue,
    SudokuIndex,
    SudokuRowIndex,
    SudokuColIndex,
    SudokuCellIndex,
    SudokuQuestion,
    Coordinate,
    SudokuRecord,
    SudokuRow,
    SudokuCol,
    SudokuCell,
    SudokuFlatBoard,
    SudokuBoard
) where
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