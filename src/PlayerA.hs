module PlayerA where

import Value
import Cell
import Board
import BoardUtil
import Ship


-- | This is the function stub you have to improve. Currently it plays the first
-- empty cell it finds.
--
-- The grid has the following coordinate system:
--
-- >  (i,j)
-- > + - - + - - + - - +
-- > | 0,0 | 0,1 | 0,2 |
-- > + - - + - - + - - +
-- > | 1,0 | 1,1 | 1,2 |
-- > + - - + - - + - - +
-- > | 2,0 | 2,1 | 2,2 |
-- > + - - + - - + - - +
play :: Board -> Cell
play board = optimalSolution board
-- play (Board _ board) = playing board (Cell 0 0)
--   where
--     playing :: [Row] -> Cell -> Cell
--     playing [] _             = error "No free cell in the board."
--     playing (row: rows) cell = playRow row cell
--       where
--         playRow :: Row -> Cell -> Cell
--         -- No free cell in this row
--         playRow [] (Cell i _) = playing rows (Cell (i + 1) 0)
--         playRow (nextCell: cells) c@(Cell i j)
--           | nextCell == U    = c
--           | otherwise = playRow cells (Cell i (j + 1))

-- | You can provide your own starting board, just make sure it is valid.
startingBoard :: Monad m => m Board
startingBoard = do
  pure clusteredBoard

-- | This is a more advanced version of the previous function, allowing you to
-- use Monads. Use wisely.
mplay :: (Monad m) => Board -> m Cell
mplay board = return (play board)
