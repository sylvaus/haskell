{--------------------------------------------------------------------------------
  Bouncing Balls demo
--------------------------------------------------------------------------------}
module Main where
  
import Graphics.UI.WXCore

-- Vector2D
data Vector2D = Vector2D { x :: Float, y :: Float } deriving(Show)

-- Planet
data Planet = Planet {index :: Int, radius :: Int, position :: Vector2D, velocity :: Vector2D, mass :: Float } deriving(Show)  

sign :: Float -> Float
sign x = if x < 0 then -1 else 1

normVector2D :: Vector2D -> Float
normVector2D vector = sqrt (x vector * x vector + y vector * y vector)

addVector2D :: Vector2D -> Vector2D -> Vector2D
addVector2D vector_left vector_right = Vector2D {x = (x vector_left + x vector_right), y =  (y vector_left + y vector_right)}

subVector2D :: Vector2D -> Vector2D -> Vector2D
subVector2D vector_left vector_right = Vector2D {x = (x vector_left - x vector_right), y =  (y vector_left - y vector_right)}

scaleVector2D :: Vector2D -> Float -> Vector2D
scaleVector2D vector scale = Vector2D {x = (x vector * scale), y =  (y vector * scale)}

computeForce :: Planet -> Planet -> Vector2D
computeForce planet planed_acted_on
  = force
  where
    r = subVector2D (position planet) (position planed_acted_on)
    r_norm = (normVector2D r)
    r3 = ((maximum [abs(r_norm), radiusF * 2.0]) ** 3) * sign r_norm
    force = scaleVector2D r (gravityCoeff * mass planet * mass planed_acted_on / r3)

applyAttraction :: Float -> Planet -> [Planet] -> Planet
applyAttraction delta_time planet planets
    = new_planet
    where 
      forces = [computeForce other_planet planet | other_planet <- planets, (index planet /= index other_planet)]
      force = foldr addVector2D (Vector2D 0 0) (forces)
      new_velocity = addVector2D (velocity planet) (scaleVector2D force (delta_time / mass planet))
      new_position = addVector2D (position planet) (scaleVector2D new_velocity delta_time)
      new_planet = Planet (index planet) (radius planet) new_position new_velocity (mass planet)

main :: IO ()
main
 = run ballsFrame

ballsFrame :: IO ()
ballsFrame
  = do -- a list of balls, where each ball is represented by a list of all future Y positions.
    vballs <- varCreate [(Planet 0 radiusI (Vector2D (maxXF/2) (maxYF/2)) (Vector2D 0 0) 10000.0)]

    -- create a non-user-resizable top-level (orphan) frame.
    f <- frameCreate objectNull idAny "Bouncing balls" rectNull
                      ( wxMINIMIZE_BOX + wxSYSTEM_MENU + wxCAPTION + wxNO_FULL_REPAINT_ON_RESIZE
                      + wxCLIP_CHILDREN + wxCLOSE_BOX)
    
                      -- add a panel to draw on, nice grey color.
    p <- panelCreate f idAny rectNull 0 -- (wxNO_FULL_REPAINT_ON_RESIZE)

    _ <- windowSetBackgroundColour f $ colorSystem Color3DFace
    windowSetLayout f (column 1 [ minsize (sz maxX maxY) (widget p)])
    
                                -- create a timer, on each tick it advances all the balls to their next position
    t <- windowTimerCreate f
    timerOnCommand t (nextBalls p vballs)
    
    -- paint the balls unbuffered
    windowOnPaintRaw p (paintBalls vballs)
    
    -- left-click: new ball, right-click: new window
    windowOnMouse p False {- no motion events -} (onMouse p vballs)
    
    -- show the frame
    _ <- windowShow f
    windowRaise f
    
    -- and start the timer (25 msec).
    _ <- timerStart t deltaTimeMs False {- one-shot timer? -}
    return ()
  
  where
    -- react on mouse events
    onMouse w vballs mouse
      = case mouse of
          MouseLeftDown  pt_ _mods -> addPlanet w vballs pt_ -- new ball
          MouseRightDown _pt _mods -> ballsFrame            -- new window with bouncing balls
          _other                   -> skipCurrentEvent      -- unprocessed event: send up the window chain
  
    updateInterval t f
      = do i <- timerGetInterval t
           timerStop t
           _ <- timerStart t (f i) False
           return ()
  
    -- advance all the balls to their next position
    nextBalls w vballs
      = do
        planets <- varGet vballs
        varUpdate vballs (map (\planet -> applyAttraction (fromIntegral deltaTimeMs * 0.001) planet planets)) >> windowRefresh w False
  
    -- add a new ball
    addPlanet w vballs pt_
      = do 
        balls <- varGet vballs
        varUpdate vballs ((Planet (length balls) radiusI position velocity 100.0):) >> windowRefresh w False
      where 
        x = fromIntegral (pointX pt_)
        y = fromIntegral (pointY pt_)
        position = Vector2D x y
        velocity = Vector2D 0 0

  
    -- paint the balls
    paintBalls vballs dc _viewRect _updateAreas
      = do dcClear dc
           balls <- varGet vballs
           dcWithBrushStyle dc (BrushStyle BrushSolid red) $
             mapM_ (drawBall dc) balls
  
    drawBall dc planet
      = dcDrawCircle dc point (radius planet)
      where 
        pose = position planet
        x_pose = x pose
        y_pose = y pose
        point = Point (round x_pose) (round y_pose) 
  

-- radius the ball, and the maximal x and y coordinates
deltaTimeMs, radiusI, maxX, maxY :: Int
deltaTimeMs = 10
radiusI = 10
maxY   = 800
maxX   = 1000

gravityCoeff, maxXF, maxYF :: Float
gravityCoeff = 1000.0
radiusF = fromIntegral radiusI
maxYF   = fromIntegral maxY
maxXF   = fromIntegral maxX