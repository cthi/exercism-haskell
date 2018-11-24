module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot { dir :: Bearing
                   , coords :: (Integer, Integer) 
                   }

bearing :: Robot -> Bearing
bearing robot = dir robot 

coordinates :: Robot -> (Integer, Integer)
coordinates robot = coords robot

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot dir coords = Robot dir coords

move :: Robot -> String -> Robot
move robot instructions = foldl stateChange robot instructions
  where
    stateChange (Robot dir coords) 'R' = Robot (rotateRight dir) coords
    stateChange (Robot dir coords) 'L' = Robot (rotateLeft dir) coords
    stateChange robot 'A' = advance robot

    rotateLeft North = West
    rotateLeft East  = North
    rotateLeft South = East
    rotateLeft West  = South

    rotateRight North = East
    rotateRight East  = South
    rotateRight South = West
    rotateRight West  = North

    advance (Robot dir (x, y)) = case dir of
      North -> Robot dir (x, y + 1)
      East  -> Robot dir (x + 1, y)
      South -> Robot dir (x, y - 1)
      West  -> Robot dir (x - 1, y)
