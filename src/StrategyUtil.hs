module StrategyUtil where

import Board
import BoardUtil
import Cell
import Ship
import Value

maxInt :: Int
maxInt = 1000000000

data CellInfo = CellInfo {pos :: Cell, coverNum :: Int, hitNum :: Int,  hitDist :: Int}
    deriving (Show)

rotation :: [Rotation]
rotation = [Rotation0, Rotation1, Rotation2, Rotation3]

negateCoordinate :: Cell -> Cell
negateCoordinate (Cell i j) = Cell (-i) (-j)

allShips :: [Ship]
allShips = (rotateShip <$> rotation <*> standardShips) >>= expand where
    expand :: Ship -> [Ship]
    expand (Ship value cells) = map expandMap cells where
        expandMap :: Cell -> Ship
        expandMap pivot = Ship value (moveOrigin <$> cells) where
            moveOrigin :: Cell -> Cell
            moveOrigin = (offsetCell pivot).negateCoordinate
        
getStrategyInfo :: Board -> [CellInfo]
getStrategyInfo board = map getInfo cellArr where
    cellArr :: [(Cell, Value)]
    cellArr = identifiedCells board
    getInfo :: (Cell, Value) -> CellInfo
    getInfo tCell = CellInfo cell coverNum hitNum distance where 
        cell = fst tCell
        value = snd tCell
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

        validShips = filter ((==True).snd) shipInfo

        coverNum =  foldr ((+).length.fst) 0 validShips

        hitNum
            | value == U || value == E = foldr hitFold 0 validShips 
            | otherwise = 0
            where
                hitFold = max.length.getHitCells.fst where
                    getHitCells :: [(Cell, Value)] -> [(Cell, Value)]
                    getHitCells x = filter ((==X).snd) x

        distance
            | hitNum == 0 = 0
            | otherwise = foldr distFold maxInt shipInfo
            where
                distFold :: ([(Cell,Value)], Bool) -> Int -> Int
                distFold info minDist = min (calMinDist.fst $ info) minDist

        calDist :: Cell -> Cell -> Int
        calDist (Cell x y) (Cell u v) = abs (x - u) + abs (y - v) where
            abs :: Int -> Int
            abs x = if x < 0 then (-x) else x

        calMinDist :: [(Cell, Value)] -> Int
        calMinDist l = foldr getMin maxInt hitCells where
            hitCells :: [(Cell, Value)]
            hitCells = filter ((==X).snd) l
            getMin :: (Cell, Value) -> Int -> Int
            getMin c minDist = min (calDist cell (fst c)) minDist


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
            
