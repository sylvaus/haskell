{--------------------------------------------------------------------------------
  Attraction planets demo
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
computeForce (Planet _ _ planet_position _ planet_mass) (Planet _ _ other_planet_position _ other_planet_mass)
  = force
  where
    r = subVector2D planet_position other_planet_position
    r_norm = (normVector2D r)
    r3 = ((maximum [abs(r_norm), (fromIntegral planetRadius) * 2.0]) ** 3) * sign r_norm
    force = scaleVector2D r (gravityCoeff * planet_mass * other_planet_mass / r3)


applyAttraction :: Float -> Planet -> [Planet] -> Planet
applyAttraction delta_time planet@(Planet index' radius' position' velocity' mass') planets
    = new_planet
    where 
      forces = [computeForce other_planet planet | other_planet <- planets, (index' /= index other_planet)]
      force = foldr addVector2D (Vector2D 0 0) (forces)
      new_velocity = addVector2D velocity' (scaleVector2D force (delta_time / mass'))
      new_position = addVector2D position' (scaleVector2D new_velocity delta_time)
      new_planet = Planet index' radius' new_position new_velocity mass'

main :: IO ()
main
 = run planetsFrame

planetsFrame :: IO ()
planetsFrame
  = do -- a list of planets, where each ball is represented by a list of all future Y positions.
    vplanets <- varCreate [(Planet 0 planetRadius (Vector2D (fromIntegral maxX / 2) (fromIntegral maxY / 2)) (Vector2D 0 0) 10000.0)]

    -- create a non-user-resizable top-level (orphan) frame.
    f <- frameCreate objectNull idAny "Planets Attraction" rectNull
                      ( wxMINIMIZE_BOX + wxSYSTEM_MENU + wxCAPTION + wxNO_FULL_REPAINT_ON_RESIZE
                      + wxCLIP_CHILDREN + wxCLOSE_BOX)
    
                      -- add a panel to draw on, nice grey color.
    p <- panelCreate f idAny rectNull 0 -- (wxNO_FULL_REPAINT_ON_RESIZE)

    _ <- windowSetBackgroundColour f $ colorSystem Color3DFace
    windowSetLayout f (column 1 [ minsize (sz maxX maxY) (widget p)])
    
                                -- create a timer, on each tick it advances all the planets to their next position
    t <- windowTimerCreate f
    timerOnCommand t (nextPlanets p vplanets)
    
    -- paint the planets unbuffered
    windowOnPaintRaw p (paintplanets vplanets)
    
    -- left-click: new ball, right-click: new window
    windowOnMouse p False {- no motion events -} (onMouse p vplanets)
    
    -- show the frame
    _ <- windowShow f
    windowRaise f
    
    -- and start the timer (25 msec).
    _ <- timerStart t deltaTimeMs False {- one-shot timer? -}
    return ()
  
  where
    -- react on mouse events
    onMouse w vplanets mouse
      = case mouse of
          MouseLeftDown  pt_ _mods -> addPlanet w vplanets pt_ -- new ball
          MouseRightDown _pt _mods -> planetsFrame            -- new window with bouncing planets
          _other                   -> skipCurrentEvent      -- unprocessed event: send up the window chain
  
    updateInterval t f
      = do i <- timerGetInterval t
           timerStop t
           _ <- timerStart t (f i) False
           return ()
  
    -- Update planet positions
    nextPlanets w vplanets
      = do
        planets <- varGet vplanets
        varUpdate vplanets (map (\planet -> applyAttraction (fromIntegral deltaTimeMs * 0.001) planet planets)) >> windowRefresh w False
  
    -- add a new ball
    addPlanet w vplanets (Point x' y')
      = do 
        planets <- varGet vplanets
        varUpdate vplanets ((Planet (length planets) planetRadius position velocity 100.0):) >> windowRefresh w False
      where 
        position = Vector2D (fromIntegral x') (fromIntegral y')
        velocity = Vector2D 0 0

  
    -- paint the planets
    paintplanets vplanets dc _viewRect _updateAreas
      = do dcClear dc
           planets <- varGet vplanets
           dcWithBrushStyle dc (BrushStyle BrushSolid red) $
             mapM_ (drawBall dc) planets
  
    drawBall dc (Planet _ radius' (Vector2D x' y') _ _)
      = dcDrawCircle dc point radius'
      where 
        point = Point (round x') (round y') 
  

-- radius the ball, and the maximal x and y coordinates
deltaTimeMs, planetRadius, maxX, maxY :: Int
deltaTimeMs = 10
planetRadius = 10
maxY   = 800
maxX   = 1000

gravityCoeff :: Float
gravityCoeff = 1000.0
