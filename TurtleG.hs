module TurtleG where

import Graphics.Gloss.Data.Picture
import Control.Monad.Trans.State

-- |Turtle datatype
data Turtle = T {
      pos :: Point          -- |Current position
    , dir :: Float          -- |Current direction
    , stepSize :: Float     -- |Current default step length
    , paths :: [[Point]]    -- |Current paths taken by the turtle
    }
  deriving Show

-- |Default Turtle
defTurtle = T {pos = (0,0), dir = 0, stepSize = 10, paths = [[]]}

-- |Lift the turtle off the paper. Subsequent move commands wont draw
penUp :: State Turtle ()
penUp = state penUp'
  where penUp' tur@(T {paths=([]:xs)}) = ((), tur)
        penUp' tur@(T {paths=xs}) = ((), tur {paths = []:xs})

-- |Place the turtle on the paper. Subsequent move commands will draw
penDown :: State Turtle ()
penDown = state penDown'
  where penDown' tur@(T {pos=p, paths=([]:xs)}) = ((), tur {paths = [p]:xs})
        penDown' tur@(T {pos=p, paths=(x:xs)}) = ((), tur)

-- |Turn the turtle left by the specified angle in degrees
left :: Float -> State Turtle ()
left f = state (left' f)
  where left' theta tur@(T {dir=d}) = ((), tur {dir = d + degToRad theta})

-- |Turn the turtle right by the specified angle in degrees
right :: Float -> State Turtle ()
right f = state (right' f)
  where right' theta tur@(T {dir=d}) = ((), tur {dir = d - degToRad theta})


-- |Move the turtle in the current direction by the specified amount
moveBy :: Float -> State Turtle ()
moveBy f = state (move' f)
  where move' dist tur@(T {pos=(px,py), dir=d, paths=(x:xs)})
            | null x    = ((), tur {pos = p'})
            | otherwise = ((), tur {pos = p', paths = (p':x):xs})
          where p' = (px + dist * cos d, py + dist * sin d)

-- |Move the turtle in the current direction by the stepSize amount
move :: State Turtle ()
move = state move'
  where move' tur@(T {pos=(px,py), dir=d, stepSize=s, paths=(x:xs)})
            | null x    = ((), tur {pos = p'})
            | otherwise = ((), tur {pos = p', paths = (p':x):xs})
          where p' = (px + s * cos d, py + s * sin d)

-- |Get the current position of the turtle
getPos :: State Turtle Point
getPos = state (\tur@(T {pos=p}) -> (p,tur))

-- |Set the current position of the turtle
setPos :: Point -> State Turtle ()
setPos pt = state $ (\a -> ((),a)) . set'
  where set' tur@(T {paths=[]:_}) = tur {pos=pt}
        set' tur@(T {paths=x:xs}) = tur {pos=pt, paths=(pt:x):xs}

-- |Get the current direction of the turtle
getDir :: State Turtle Float
getDir = state (\tur@(T {dir=d}) -> (d,tur))

-- |Set the current direction of the turtle
setDir :: Float -> State Turtle ()
setDir d = state (\tur -> ((),tur {dir = d}))

-- |Get the current step length of the turtle
getStep :: State Turtle Float
getStep = state (\tur@(T {stepSize=s}) -> (s,tur))

-- |Set the current step length of the turtle
setStep :: Float -> State Turtle ()
setStep s = state (\tur -> ((),tur {stepSize = s}))

-- |Run the turtle action and return a gloss picture of the paths
runTurtle :: Turtle -> State Turtle a -> Picture
runTurtle tur st = Pictures $ map Line $ paths $ execState st tur

-- |Convert degrees to radians
degToRad theta = theta*pi/180
