{-# LANGUAGE CPP, ScopedTypeVariables, FlexibleInstances #-}


module Sudoku.Utils (
    isSafe,
    isFinished,
    findUnassigned,
    setRecord
) where


    import Sudoku.Types


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
    isNotZero x = (not . isZero) x

    recordIsZero :: SudokuRecord -> Bool
    recordIsZero = isZero . snd

    recordIsNotZero :: SudokuRecord -> Bool
    recordIsNotZero = isNotZero . snd

    getRecordValue :: SudokuRecord -> SudokuValue
    getRecordValue = snd

    getRecordCoordinate :: SudokuRecord -> Coordinate
    getRecordCoordinate = fst

    isFinished :: SudokuFlatBoard -> Bool
    isFinished flatBoard = all recordIsNotZero flatBoard

    isNotFinished :: SudokuFlatBoard -> Bool
    isNotFinished flatBoard = (not . isFinished) flatBoard

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
    rowHasValueWith value row = any (recordHasValueWith value) row

    colHasValueWith :: SudokuValue -> SudokuCol -> Bool
    colHasValueWith value col = any (recordHasValueWith value) col

    cellHasValueWith :: SudokuValue -> SudokuCell -> Bool
    cellHasValueWith value cell = any (recordHasValueWith value) cell

    isSafe :: SudokuFlatBoard -> Coordinate -> SudokuValue -> Bool
    isSafe board coordinate value
        | rowHasValueWith value (getRow board $ getRowIndex coordinate) = False
        | colHasValueWith value (getCol board $ getColIndex coordinate) = False
        | cellHasValueWith value (getCell board $ getCellIndex coordinate) = False
        | otherwise = True

    setRecord :: SudokuFlatBoard -> Coordinate -> SudokuValue -> SudokuFlatBoard
    setRecord flatBoard coordinate value = (coordinate, value): filter (\record -> getRecordCoordinate record /= coordinate) flatBoard
