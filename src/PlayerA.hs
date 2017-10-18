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

--------------------------------------------------------
-- Strategy Idea

-- A placement of a ship is a place where a ship with any rotation can be placed in.
-- A valid placement of a ship is a placement that we can actually place a ship in which means that placment only contains hit shots and empty or undefined cells
-- The strategy is divided into two cases
--  Case 1: If there is no hit shot on the board which means that all the shot are either miss shot or represent a part of a revealed ship. In this case I will choose the cell with the most impact to shoot. The impact of a cell is calculate by the number of pairs (aShip, aPivot) where aShip is a valid placement of a any kind of ship with any rotation and aPivot is a cell which can be shot to attack aShip
--  Case 2: If there is at lease a hit shot on the board which means a hit shot that does not yet reveal openent ship. In this case, I will choose a valid placement of any kind of ship with any rotation. The placment has the largest number of hit shots. In that ship, I find a cell that has not been shot and has the smallest Manhattan distance to the nearest hit shot.

--------------------------------------------------------
-- Strategy implementation

maxInt :: Int
maxInt = 1000000000

-- This type is used to stored tactic information for each cell
-- pos: The coordinate of the pivot cell
-- coverNum: The sevirity of the shot at pos in terms of other shot in the future
-- hitNum: The number of hit shot in a possibility of a valid placement with the largest hit shot
-- hitDist: The smallest Mahattan distance from the pivot cell to any hitshot from any valid placement
data CellInfo = CellInfo {pos :: Cell, coverNum :: Int, hitNum :: Int,  hitDist :: Int}
    deriving (Show)

rotation :: [Rotation]
rotation = [Rotation0, Rotation1, Rotation2, Rotation3]

-- This function negate a vector coordinate
negateCoordinate :: Cell -> Cell
negateCoordinate (Cell i j) = Cell (-i) (-j)

-- This variable contains all the possible placement vector of a ship that contain a pivot cell
-- A placement vector is a list of coordinate vector with the pivot cell at the origin
-- For example, the variable will consider the pivot cell as some point in the middle of the ship and calculate the coordinate vector of all orther part of the ship based on coordinate of the pivot point
allShips :: [Ship]
allShips = (rotateShip <$> rotation <*> standardShips) >>= expand where
    -- For each ship find a list of suitable placements (e.g. where is the pivot cell inside the ship)
    expand :: Ship -> [Ship]
    expand (Ship value cells) = map expandMap cells where
        -- For each relative position, move the origin to that point and calculate all the coordinate vector
        expandMap :: Cell -> Ship
        expandMap pivot = Ship value (moveOrigin <$> cells) where
            moveOrigin :: Cell -> Cell
            moveOrigin = (offsetCell pivot).negateCoordinate
        
-- This function will get the tactic information for each cell
getStrategyInfo :: Board -> [CellInfo]
getStrategyInfo board = map getInfo cellArr where
    -- cellArr contains all the cells of the board in a list with the format of (Cell, Value)
    cellArr :: [(Cell, Value)]
    cellArr = identifiedCells board

    -- getInfo calculate tactic information for each Cell and construct into CellInfo
    getInfo :: (Cell, Value) -> CellInfo
    getInfo tCell = CellInfo cell coverNum hitNum hitDist where 
        -- cell is the position of the current cell
        cell :: Cell
        cell = fst tCell

        -- value is the value of the current cell
        value :: Value
        value = snd tCell

        -- shipInfo calculate all the coordinates of a placement that contains the current cell a long with detecting whether a placement is a valid placement
        shipInfo :: [([(Cell, Value)], Bool)]
        shipInfo = map shipFold allShips where
            shipFold :: Ship -> ([(Cell, Value)], Bool)
            shipFold ship = (cells, newValid)  where
                nCells :: [Cell]
                nCells = (offsetCell cell) <$> (shipCells ship)
                cells :: [(Cell, Value)]
                cells = filter cellFilter cellArr where
                    cellFilter :: (Cell, Value) -> Bool
                    cellFilter c = contain && validValue where
                        contain = elem (fst c) nCells
                        validValue
                            | value == E = True
                            | value == X = True
                            | value == U = True
                            | otherwise = False
                            where
                                value = snd c
                newValid :: Bool
                newValid = (length nCells) == (length cells)

        -- validShips is a list of valid placements containing current cell
        validShips :: [ ( [ (Cell, Value) ] , Bool ) ]
        validShips = filter ((==True).snd) shipInfo

        -- coverNum has the same meaning as described in CellInfo
        coverNum :: Int
        coverNum = foldr ((+).length.fst) 0 validShips

        -- hitNum has the same meaning as described in CellInfo
        hitNum :: Int
        hitNum
            | value == U || value == E = foldr hitFold 0 validShips 
            | otherwise = 0
            where
                hitFold = max.length.getHitCells.fst where
                    getHitCells :: [(Cell, Value)] -> [(Cell, Value)]
                    getHitCells x = filter ((==X).snd) x

        -- hitDist has the same meaning as described in CellInfo
        hitDist
            | hitNum == 0 = 0
            | otherwise = foldr distFold maxInt shipInfo
            where
                distFold :: ([(Cell,Value)], Bool) -> Int -> Int
                distFold info minDist = min (calMinDist.fst $ info) minDist

        -- calDist calculate Manhattan distance between two cells
        calDist :: Cell -> Cell -> Int
        calDist (Cell x y) (Cell u v) = abs (x - u) + abs (y - v) where
            abs :: Int -> Int
            abs x = if x < 0 then (-x) else x

        -- calMinDist calculate the smallest Manhattan distance from current cell to any hitShot of a placement
        calMinDist :: [(Cell, Value)] -> Int
        calMinDist l = foldr getMin maxInt hitCells where
            hitCells :: [(Cell, Value)]
            hitCells = filter ((==X).snd) l
            getMin :: (Cell, Value) -> Int -> Int
            getMin c minDist = min (calDist cell (fst c)) minDist


-- optimalSolution choose a cell to shoot according to the state of the current board and the strategy described above
optimalSolution :: Board -> Cell
optimalSolution board
    | hitNum maxHit == 0 =
        case coverNum maxCover of
            0 -> error "There is no valid cell"
            x ->  pos maxCover
    | otherwise = pos maxHit
    where

    strategyInfo :: [CellInfo]
    strategyInfo = getStrategyInfo board

    initCell :: CellInfo
    initCell = CellInfo (Cell 0 0) 0 0 maxInt

    maxHit :: CellInfo
    maxHit = foldr getMaxHit initCell strategyInfo where
        getMaxHit :: CellInfo -> CellInfo -> CellInfo
        getMaxHit cell maxVal
            | (hitNum cell) > (hitNum maxVal) = cell
            | (hitNum cell) < (hitNum maxVal) = maxVal
            | (hitNum cell) == (hitNum maxVal) =
                case (hitDist maxVal) > (hitDist cell) of
                    True -> cell
                    False -> maxVal

    maxCover :: CellInfo
    maxCover = foldr getMaxCover initCell strategyInfo where
        getMaxCover :: CellInfo -> CellInfo -> CellInfo
        getMaxCover cell maxVal
            | (coverNum cell) > (coverNum maxVal) = cell
            | otherwise = maxVal
