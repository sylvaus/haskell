-- Point
data Vector2D = Vector2D { x :: Float, y :: Float } deriving(Show)


-- Planet
data Planet = Planet {index :: Int, radius :: Int, position :: Vector2D, velocity :: Vector2D, mass :: Float } deriving(Show)  

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
    r3 = (normVector2D r) ** 3
    force = scaleVector2D r (mass planet * mass planed_acted_on / r3)

applyAttraction :: Float -> Planet -> [Planet] -> Planet
applyAttraction delta_time planet planets
    = new_planet
    where 
      other_planets = filter (\other_planet -> (index planet == index other_planet)) planets
      forces = map (\other_planet -> computeForce other_planet planet) (other_planets)
      force = foldr addVector2D (Vector2D 0 0) (forces)
      new_velocity = addVector2D (velocity planet) (scaleVector2D force (delta_time / mass planet))
      new_position = addVector2D (position planet) (scaleVector2D new_velocity delta_time)
      new_planet = Planet (index planet) (radius planet) new_position new_velocity (mass planet)

main :: IO ()
main 
  = do
    print planet
    print added
    return ()
  where 
    center = Vector2D {x = 4, y = 5}
    vel = Vector2D {x=4.5, y=45}
    added = addVector2D center vel
    planet = Planet 0 4 center vel 4.5