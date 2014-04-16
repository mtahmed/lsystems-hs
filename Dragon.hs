-- Run: cabal run dragon <iterations> <initial step length>

import LSystem
import TurtleG

import Control.Monad        ( forM_ )
import Graphics.Gloss       ( display, Display(InWindow), white, Picture)
import System.Environment   ( getArgs )

dragon = lsystem [('L', "CL"),('X',"X+YF"),('Y',"FX-Y")] "LFX"

main = do
    [iterS, stepS] <- getArgs
    let state = stepN' (read iterS) dragon
    putStrLn state
    display (InWindow "Winged Dragon" (800,600) (0,0)) white
        $ drawDragon (read stepS) state

drawDragon :: Float -> String -> Picture
drawDragon s xs = runTurtle (defTurtle {pos =(-200,-150) ,stepSize = s})
    $ do  penDown
          forM_ xs draw
  where draw 'C' = do
            left 45
            s <- getStep
            setStep (s/(sqrt 2))
        draw 'F' = move
        draw '+' = right 90
        draw '-' = left 90
        draw _ = return ()
