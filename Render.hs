module Render where


import VecMat
import Color
import PPMUtils -- Img

import Debug.Trace

-- Camera has 4 parameters  :   lookFrom    , lookAt    , up. and FOV (in Y directon)
-- These are sometimes referred to as (eye, center , up)

data Camera =   Camera {
                    lookFrom :: Vec3, 
                    lookAt   :: Vec3, 
                    up       :: Vec3,
                    fov      :: Double
                } deriving (Show)

data CameraN =   CameraN {
                    uN   :: Vec3, 
                    vN   :: Vec3, 
                    wN   :: Vec3,
                    fovN:: Double
                } deriving (Show)

data Ray    =   Ray {
                    ori :: Vec3,
                    dir :: Vec3
                } deriving (Show)

data Light  =   Directional { direction :: Vec3 , color :: Color}
            |   Point       { location  :: Vec3 , color :: Color}
            |   Ambient     { color     :: Color}
                deriving (Show)

data Material   =   Material {
                        ambient  :: Color, 
                        diffuse  :: Color,
                        specular :: Color
                    }
                    deriving (Show)

--data Object = Sphere  Vec3 Double Material
data Object = Sphere    { center :: Vec3, rad :: Double, material :: Material}
            | Tri       { v1 :: Vec3, v2 :: Vec3, v3 :: Vec3, material :: Material, normal :: Vec3}             -- w/ self computed normal
            | TriN      { v1 :: Vec3, v2 :: Vec3, v3 :: Vec3, material :: Material, 
                         v1n :: Vec3,v2n :: Vec3,v3n :: Vec3}  -- w/ given normals at each Vertex
            deriving (Show)


data World = World {
                imgs      :: [Img],
                cameras   :: [Camera],
                objects   :: [Object],
                lights    :: [Light]
             }  --deriving (Show)
instance Show World where
    show (World i c o l) = unlines [ show i , show c, show o , show l]






render :: World -> String
render myWorld = trace ("World entering Render::  " ++ show(myWorld)) $
                 createPPM (rayTrace myWorld)


rayTrace :: World -> Img
rayTrace myWorld = Img w h pixels
                   where pixels = map (getColorForRay myWorld) raysFromPixels
                         --trace "rayTrace:: w1,h1=" ++ show w1 ++" , "++show h1
                         raysFromPixels = map (rayFromPixel cam (w,h) ) [ (xs,ys) |
                                            ys  <- [h1-0.5,h1-1.5..0.5],
                                            xs  <- [0.5,1.5..w1]
                                        ]
                         cam = head $ cameras myWorld
                         w   = width $ head $ imgs myWorld
                         h   = height $ head $ imgs myWorld
                         w1  = fromIntegral (w-1) :: Double   
                         h1  = fromIntegral (h-1) :: Double   
--                  pixel_list = [ get_color_for_ray scene (ray_for_pixel w h x y) | 
--                         y <- map fromInteger [0..height - 1], x <- map fromInteger [0..width - 1]]

--                   where pixels = (map vec3Fromf [ x | x<-[0.0,0.001..1], y<-[1..480]] )
-- rayTrace myWorld = Img 101 400 (map vec3Fromf [ x | x<-[0.0,0.01..1], y<-[1..400]] )

tCamera = Camera (Vec3 (0,0,-4)) (Vec3 (0,0,0)) (Vec3 (0,1,0)) (45*pi/180)  
tw = 200
th = 400
-- testRayFromPixel (0,0)  ==> lookFrom_vector  -lookFrom_vector

testRayFromPixel :: (Double,Double) -> Ray
testRayFromPixel (x,y) = rayFromPixel tCamera (tw,th) (x,y)

rayFromPixel :: Camera -> (Int,Int) -> (Double,Double) -> Ray
rayFromPixel c (w,h) (x,y) =  {-if (x==50.5) then
                                    trace ("rayFromPixel: " ++ "x,y=" ++ show x ++ " , " ++ show y ++ " Ray:: ori=" ++ 
                                    show (lookFrom c) ++ " dir=" ++ show ( roundVec3100 $ normalizeVec3 ( ru+rv+rw) )) $
                                Ray (lookFrom c) ( normalizeVec3 ( ru+rv+rw) )
                              else-}
                                Ray (lookFrom c) ( normalizeVec3 ( ru+rv+rw) )
--                                 ( normalizeVec3 (  subVec3 
--                                                   ( addVec3 (mulVec3 ((tan (fovx/2))*(x/w-0.5))  (u normCamera)) 
--                                                           ((tan (fovy/2))*(y/h-0.5) * (v normCamera)) )
--                                                   (w normCamera) ) )
                        where
                            normCamera = normalizeCamera c
                            fovy = fov c
                            w' = fromIntegral w
                            h' = fromIntegral h
                            --fovx = (fovy/h') * w'
                            fovx = atan  ( (tan (fovy)) * w'/h')
                            ru = mulVec3 ((tan ((fovx)/2))*(x/w'-0.5)*2)   (uN normCamera)
                            rv = mulVec3 ((tan ((fovy)/2))*(y/h'-0.5)*2)   (vN normCamera)
                            rw = negate $ wN normCamera

normalizeCamera :: Camera -> CameraN
normalizeCamera (Camera eye center up fov ) = CameraN u v w fov
                    where
                        a = subVec3 eye center
                        b = up
                        w = normalizeVec3 a
                        u = normalizeVec3 ( crossVec3 b w)
                        v = crossVec3 w u


getColorForRay :: World ->Ray -> Vec3
getColorForRay w r = if  ( dist ) <0 then (Vec3 (0,0,0))  -- Background Black
                                     else getColorForRayObject r obj
    where
        objDist = findMinPositive (getRayIntersections w r)
        obj  = fst objDist
        dist = snd objDist


getColorForRayObject :: Ray -> Object -> Vec3 
getColorForRayObject ray (Sphere cent rad m)     = ambient m
getColorForRayObject ray (Tri v1 v2 v3 m norm)   = ambient m


findMinPositive :: [(Object,Double)] -> (Object,Double)
findMinPositive xs = if miny>0  then head ([(x,y) | (x,y) <- xs, y==miny])
								else ((fst$ head xs),-1) 
	where
		miny = findMinPositive' xs


findMinPositive' :: [(Object,Double)] -> Double
findMinPositive' xs = minimum' ([y | (x,y) <- xs, y>0])

minimum':: [Double] -> Double
minimum' []  = -1.0
minimum' xs  = minimum xs


getRayIntersections :: World -> Ray -> [(Object, Double)]
getRayIntersections  world ray =  --trace ("In getRayIntersections:" ++ show intersections ) $ 
                                    intersections
    where intersections = [ (obj, rayObjectIntersect ray obj) | obj <- (objects world) ]

-- We'll return the closest positive intersection, or -1 if none
rayObjectIntersect :: Ray -> Object -> Double
rayObjectIntersect ray (Sphere cent rad mat) = raySphereIntersect ray cent rad 
rayObjectIntersect ray (Tri v1 v2 v3 mat norm)  = rayTriIntersect ray (v1,v2,v3)


epsilon =  0.0000001

raySphereIntersect :: Ray -> Vec3 -> Double -> Double
raySphereIntersect (Ray rx rv) sx rad  = minimum' intersections
    where intersections = filter (> epsilon) (solveQuadratic a b c)
-- (rx --> orig  , rv --> dir) (sx --> orig , rad <-- radius )
          se = (sx - rx)
          b  = (-2.0) * (dotVec3 rv  se)
          a  = dotVec3 rv  rv
          c  = (dotVec3 se  se) - (rad * rad)



-- solve_quadratic gives the solutions to the given quadratic equation
-- ax^2 + bx + c == 0

solveQuadratic :: Double -> Double -> Double -> [Double]
solveQuadratic    a         b         c         =  solutions
    where solutions | discriminant < 0 = []
                    | otherwise        = [(-b + sqrt_d) / (2.0 * a),
                                           (-b - sqrt_d) / (2.0 * a)]
          discriminant                  = (b * b) - (4.0 * a * c)
          sqrt_d                        = sqrt discriminant 


-- to test rayTriIntersect
z0=1;
tOri = Vec3 (0,0,z0)
tDir = normalizeVec3 ( Vec3 (-0.8,-0.9,-1) )
tRay = Ray tOri tDir

tv1 = Vec3 (-1,-1,0)
tv2 = Vec3 ( 1,-1,0)
tv3 = Vec3 ( 1, 1,0)

-- rayTriIntersect tRay (tv1,tv2,tv3)

-- We'll return the closest positive intersection, or -1 if none
rayTriIntersect :: Ray -> (Vec3,Vec3,Vec3) -> Double
rayTriIntersect (Ray ori dir) (v1,v2,v3) =    findDistBary ori v1 v2 v3 bCoords
                                            where
                                                bCoords = baryCoords v1 v2 v3 (rayPlaneIntersect ori dir v1 v2 v3)


findDistBary :: Vec3->Vec3->Vec3->Vec3->(Double,Double,Double)->Double
findDistBary    ori   v1    v2    v3    (alpha , beta , gama) =
                                            if ( (alpha>=0) && (beta>=0) && (gama>=0) ) then 
                                                --trace ("In findDistBAry:: then is " ++  show(alpha) ++ show(beta)++show(gama)) $ 
                                                normVec3 ( (mulVec3 alpha v1) + (mulVec3 beta v2) + (mulVec3 gama v3) - ori)
                                            else 
                                                --trace ("In findDistBAry:: else is " ++ "ori=" ++ show(ori) ++
                                                --"  v1=" ++ show(v1) ++ " ;; " ++ show(alpha) ++ " , " ++ show(beta)++ " , " ++ show(gama)) $ 
                                                (-1.0)


baryCoords :: Vec3->Vec3->Vec3->Vec3->(Double,Double,Double)
baryCoords    v1     v2    v3    p   = --trace ("In baryCoords:: " ++ "v1=" ++ show v1 ++
                                       --         "  v2=" ++ show v2 ++ "  v3=" ++ show v3 ++ "  p=" ++ show p ++ " ;; " ++ show(alpha) ++ " , " ++ show(beta)++ " , " ++ show(gama)  ) $ 
                                        (alpha,beta,gama)
                                        where
                                                aa = p - v1
                                                bb = v2 - v1
                                                cc = v3 - v1

                                                sol = solveBary' aa bb cc
                                                alpha = fst sol
                                                beta = snd sol
                                                gama = 1 - alpha - beta

solveBary' :: Vec3 -> Vec3 -> Vec3 -> (Double,Double)
solveBary' aa bb cc = if (det1==0) then if (det2==0) then (alpha3,beta3)
                                                    else (alpha2,beta2) 
                                    else (alpha1,beta1)
    where
        a1=vec3x aa
        a2=vec3y aa
        a3=vec3z aa
        b1=vec3x bb
        b2=vec3y bb
        b3=vec3z bb
        c1=vec3x cc
        c2=vec3y cc
        c3=vec3z cc
        det1 = (b1*c2-b2*c1)
        beta1 = (a1*c2 - a2*c1)/det1
        gama1 = (b1*a2 - b2*a1)/det1
        alpha1 = 1 - beta1 - gama1

        det2 = (b1*c3 - b3*c1)
        beta2 = (a1*c3 - a3*c1)/det2
        gama2 = (b1*a3 - b3*a1)/det2
        alpha2 = 1 - beta2 - gama2
           

        det3 = (b3*c2-b2*c3)
        beta3 = (a3*c2 - a2*c3)/det3
        gama3 = (b3*a2 - b2*a3)/det3
        alpha3 = 1 - beta3 - gama3                        

rayPlaneIntersect :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3
rayPlaneIntersect    ori     dir     v1      v2      v3 =  --trace ("In rayPlaneIntersect:" ++ "ori=" ++ show ori ++ "  dir=" ++ show dir ++ " point: " ++ show (ori + (mulVec3 t dir)) ) $ 
                                                            ori + (mulVec3 t dir)
                                                            where 
                                                                n' = crossVec3 (v3-v1) (v2-v1)
                                                                n  = normalizeVec3 n'
                                                                t  = ( (dotVec3 v1 n) - (dotVec3 ori n) ) / (dotVec3 dir n)
    
{-}
-- Work out what surfaces the ray intersects and the distance at which it
-- first intersects.
-- Return value is a list of (surface, Just distance) or (surface, Nothing) 
get_ray_intersections :: Scene -> Ray -> [(Surface, [Scalar])]
get_ray_intersections    scene    ray =  intersections
    where intersections = [ (obj, ray_surface_intersect ray obj) | obj <- fst scene ]


-- To detect the color of the ray, we find the first object it intersects 
-- (if any),
-- then we use the transparency, refractive index, reflectivity, emission
-- characterists of that object and some recursion upon rays that bounce off it
-- to determine a suitable color
get_color_for_ray :: Scene -> Ray -> Color
get_color_for_ray    scene    ray =  srdetectcolor' scene ray (srintersect scene ray)



      -}

