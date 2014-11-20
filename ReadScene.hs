module ReadScene where

import Debug.Trace
import Data.List (foldl', isSuffixOf)
import Data.Maybe
import Data.Array
import System.Process
import System.Directory
import Text.Printf
--import Control.Parallel
--import Control.Parallel.Strategies

import LinearAlgebra
import WritePPM

data Token = AmbientTok Vec3
           | AttenuationTok Vec3
           | CameraTok Vec3 Vec3 Vec3 Double
           | DiffuseTok Vec3
           | DirectionalTok Vec3 Vec3
           | EmissionTok Vec3
           | MaxDepthTok Int
           | MaxvertsTok Int
           | OutputTok FilePath
           | PointTok Vec3 Vec3
           | PopTransformTok
           | PushTransformTok
           | RotateTok Vec3 Double
           | ScaleTok Vec3
           | ShininessTok Double
           | SizeTok Int Int
           | SpecularTok Vec3
           | SphereTok Vec3 Double
           | TranslateTok Vec3
           | TriTok Int3
           | VertexTok Vec3
           deriving (Show)

newtype Camera = Camera (Vec3, Vec3, Vec3, Double)
                 deriving (Show)

newtype Ray = Ray (Vec3, Vec3)
              deriving Show

cameraRay :: State -> Double -> Double -> Ray 
cameraRay state r_ c_ =
  let c = camera state
      (width_, height_) = size state
  in
   rayFromPixel c width_ height_ r_ c_

rayFromPixel c width_ height_ y x =
  let Camera (eye, center, up, fov) = c
      width = fromIntegral width_
      height = fromIntegral height_
      a = eye `vMinus` center
      b = up
      w = vNorm a
      u = vNorm $ b `vCross` w
      v = w `vCross` u
      tangent = tan (fov / 2)
      alpha = tangent * width/height * (x/width - 0.5) * 2
      beta = tangent * (y/height-0.5) * 2
      rayDirection = vNorm (((alpha `vScale` u) `vPlus` (beta `vScale` v)) `vMinus` w)
      ray = eye `vPlus` rayDirection
      mag = vMag rayDirection
      result =  Ray (eye, rayDirection)
      msg = show $ (vNorm a) `vDot` (vNorm b)
      msg2 = "(u,v,w) :: " ++ show (roundVec3100 u) ++ " , " ++ show (roundVec3100 v) ++ " , " ++ show (roundVec3100 w)
  in {-trace msg2 $-} result

quadraticRoots a b c =
  let disc = b^2 - 4*a*c
      discRoot = sqrt disc
      root1 = (-b - discRoot) / (2*a)
      root2 = (-b + discRoot) / (2*a)
  in case signum disc of
    -1 -> []
    0 -> [root1]
    1 -> [root1, root2]

translationMatrix (Vec3 (tx, ty, tz)) =
  matrixFromList 4 4 [1, 0, 0, tx,
                      0, 1, 0, ty,
                      0, 0, 1, tz,
                      0, 0, 0, 1]

scaleMatrix (Vec3 (sx, sy, sz)) =
  matrixFromList 4 4 [sx, 0, 0, 0,
                      0, sy, 0, 0,
                      0, 0, sz, 0,
                      0, 0, 0, 1]

rotationMatrix (Vec3 (x, y, z)) theta =
  let (Matrix m3) =
        --(cos theta *. identityMatrix 3)
        (cos (theta*pi/180) *. identityMatrix 3)
        .+.
        --((1 - cos theta) *. matrixFromList 3 3 [x^2, x*y, x*z,
        ((1 - cos (theta*pi/180) ) *. matrixFromList 3 3 [x^2, x*y, x*z,
                                                x*y, y^2, y*z,
                                                x*z, y*z, z^2])
        .+.
        --(sin theta *. matrixFromList 3 3 [0, -z, y,
        (sin (theta*pi/180 )*. matrixFromList 3 3 [0, -z, y,
                                          z, 0, -x,
                                          -y, x, 0])
      spec = assocs m3 ++ [((4, 1), 0), ((4, 2), 0), ((4, 3), 0),
                           ((4, 4), 1),
                           ((1, 4), 0), ((2, 4), 0), ((3, 4), 0)]
  in Matrix $ array ((1, 1), (4, 4)) spec

data State = State {ambient, attenuation, diffuse, emission, specular :: Vec3,
                    shininess :: Double,
                    maxDepth :: Int,
                    matrixStack :: [(Matrix, Matrix, Matrix)],
                    camera :: Camera,
                    output :: FilePath,
                    size :: (Int, Int),
                    vertices :: Array Int Vec3,
                    vertexList :: [Vec3],
                    maxVerts :: Int
                   } deriving (Show)

newtype World = World (State, [Light], [Geometry])
              deriving (Show)

render :: World -> PPM
render world@(World (state, lights, geometry)) =
  let (width, height) = size state
      ppm = PPM (listArray ((1, 1), (height, width)) pixels)
      pixels = [findColor world (fromIntegral r - 0.5) (fromIntegral c - 0.5) |
                r <- [1 .. height],
                c <- [1 .. width]]
      (Camera (eye, center, up, fov)) = camera state
      msg = show $ (eye `vMinus` center) `vDot` up
  in {-trace msg-} ppm

findColor world@(World (state, lights, geometry)) r c =
  let ray = {- trace ("from CameraRay: r,c = ("++ show r ++ " , " ++ show c ++ ")  ::Ray=" ++ showRay100 ( cameraRay state r c) )$ -}
  				cameraRay state r c
      hit = raySceneIntersect ray world
  in case hit of
    Hit (Just (distance, point, object@(Geometry(shape, objectState)))) ->
      let Vec3 rgb = ambientLight objectState  `vPlus` emissionLight objectState 
                        `vPlus` diffusedLight object point ray world lights
                        `vPlus` specularLight object point ray world lights
      in Pixel rgb
    Hit Nothing -> Pixel (0, 0, 0)




ambientLight :: State -> Vec3
ambientLight objState = ambient objState

emissionLight :: State -> Vec3
emissionLight objState = emission objState


specularLight :: Geometry->Vec3->Ray->World->[Light]-> Vec3
specularLight    geom      p0    ray  world   ls   = 
      foldl  vPlus (Vec3 (0,0,0)) $ map (specularLight' geom p0 ray world) ls

specularLight' :: Geometry ->                   Vec3-> Ray->               World-> Light                                ->Vec3
specularLight'    (Geometry(shape,objectState)) p0     ray@(Ray (ori,dir)) world   (Light (PointLight v0 rgb0, state) ) = 
  let dir' = vNorm $ v0 `vMinus` p0
      ori' = p0 `vPlus` ( 0.01 `vScale` dir')
      ray' = Ray ( ori',dir')   -- ray' is from point-on-surface to Light

      hit = {-trace ("diffusedLight':: ray' is=" ++ showRay100 ray') $-} raySceneIntersect ray' world
      
      (xform, inverseXform, inverseTransposeXform) = head $ matrixStack objectState
      
      p0' = fromH $ inverseXform .*. (hPoint p0)
      --p1 = fromH $ inverseXform .*. (hVector rayDirection)
      

      shapeN' = case shape of 
        Sphere s0 r -> vNorm (p0' `vMinus` s0)
        Tri q0 q1 q2 -> vNorm ( ( q2 `vMinus` q1) `vCross` (q0 `vMinus` q1) )

      --shapeN = shapeN'  
      --shapeN = vNorm $ fromH $ (transpose inverseXform) .*. (hVector shapeN')
      shapeN = vNorm $ fromH $ inverseTransposeXform .*. (hVector shapeN')
      
      -- Flipp the ray about the Normal
      _p1 = (dir' `vDot` shapeN ) `vScale` shapeN
      _p2 = dir' `vMinus` _p1 

      -- Flipped ray
      dir'' = _p1 `vMinus` _p2
      ray'' = Ray (ori', _p1 `vMinus` _p2)
      -- Cosine of angle between light-flipped-ray and observer
      shin = dir'' `vDot` dir
      --- Alternative way to calculate in the slides.


  in case hit of
    Hit (Just (distance, point, object@(Geometry(shape, objectState)))) ->
        {-trace ("diffusedLight':: hit") $-} Vec3  (0, 0, 0) 
    Hit Nothing -> --rgb0 {- trace ("diffusedLight':: Nothing") $ -} ---Vec3 (0,0,1)  
                 ( max 0 ( shin ** (shininess objectState) ) ) `vScale` (rgb0 `vElemProd` (specular objectState ) ) 
        
specularLight' _ _ _ _ _ = Vec3 (0,0,0)







diffusedLight :: Geometry->Vec3->Ray->World->[Light]-> Vec3
diffusedLight    geom      p0    ray  world   ls   = 
      foldl  vPlus (Vec3 (0,0,0)) $ map (diffusedLight' geom p0 ray world) ls

diffusedLight' :: Geometry ->                   Vec3-> Ray->               World-> Light                                ->Vec3
diffusedLight'    (Geometry(shape,objectState)) p0     ray@(Ray (ori,dir)) world   (Light (PointLight v0 rgb0, state) ) = 
  let dir' = vNorm $ v0 `vMinus` p0
      ori' = p0 `vPlus` ( 0.01 `vScale` dir')
      ray' = Ray ( ori',dir')


      hit = {-trace ("diffusedLight':: ray' is=" ++ showRay100 ray') $-} raySceneIntersect ray' world
      
      (xform, inverseXform, inverseTransposeXform) = head $ matrixStack objectState
      
      p0' = fromH $ inverseXform .*. (hPoint p0)
      --p1 = fromH $ inverseXform .*. (hVector rayDirection)
      

      shapeN' = case shape of 
        Sphere s0 r -> vNorm (p0' `vMinus` s0)
        Tri q0 q1 q2 -> vNorm ( ( q2 `vMinus` q1) `vCross` (q0 `vMinus` q1) )

      --shapeN = shapeN'  
      shapeN = vNorm $ fromH $ (transpose inverseXform) .*. (hVector shapeN')
      
      --shapeN = fromH $ (transpose xform) .*. (hVector shapeN')
      --shapeN = fromH $ inverseTransposeXform .*. (hVector shapeN')
      
  in case hit of
    Hit (Just (distance, point, object@(Geometry(shape, objectState)))) ->
        {-trace ("diffusedLight':: hit") $-} Vec3  (0, 0, 0) 
    Hit Nothing -> --rgb0 {- trace ("diffusedLight':: Nothing") $ -} ---Vec3 (0,0,1)  
                 ( max 0 (shapeN  `vDot` (vNorm dir')) ) `vScale` (rgb0 `vElemProd` (diffuse objectState ) ) 
        
diffuseLight' _ _ _ _ _ = Vec3 (0,0,0)





showRay100 :: Ray -> String
showRay100 (Ray (v1,v2)) = show (roundVec3100 v1) ++ show (roundVec3100 v2)


roundVec3100 :: Vec3 -> Vec3
roundVec3100 (Vec3 (x,y,z)) = Vec3 (round100 x, round100 y,round100 z)

round100 :: Double -> Double
round100 x = (fromIntegral $ round (100*x))/100

raySceneIntersect ray (World (state, lights, geometry)) =
  {- trace ("raySceneIntersect:: ray is=" ++ showRay100 ray) $ -}
  foldl' (rayObjectIntersect ray) hit0 geometry

closer d prevD = case prevD of Nothing -> True
                               Just d' -> d < d'
--ZZ                               Just d' -> d > d'

rayObjectIntersect :: Ray -> Hit -> Geometry -> Hit
rayObjectIntersect ray h@(Hit previousHit) object =
  case rayGeometryIntersect ray object of 
    Nothing -> h
    Just (d, pt) ->
      case previousHit of
        Nothing -> {-trace("rayObjectIntersect:: ray=" ++ showRay100 ray ++ " :: Point=" ++ show (roundVec3100 pt)) $-} Hit (Just (d, pt, object))
    	{-trace ("rayObjectIntersect. Just d=" ++ show d ++ "; ray="++ (showRay100 ray)  {- ++ " Object: " ++ (show object) -}) $ -}
        Just (prevd, prevPoint, prevObj) ->
          if d < prevd then Hit (Just (d, pt, object)) else h

rayGeometryIntersect :: Ray -> Geometry -> Maybe (Double, Vec3)

rayGeometryIntersect (Ray (rayPoint, rayDirection)) (Geometry (Sphere c r, state)) =
  let (xform, inverseXform, inverseTransposeXform) = head $ matrixStack state
      p0 = fromH $ inverseXform .*. (hPoint rayPoint)
      p1 = fromH $ inverseXform .*. (hVector rayDirection)
      
      p0_c = p0 `vMinus` c
      a0 = p1 `vDot` p1
      a1 = 2*(p1 `vDot` p0_c)
      a2 = (p0_c `vDot` p0_c) - r^2
      roots = quadraticRoots a0 a1 a2
      dist r = let rPoint = p0 `vPlus` (vScale r p1)
                   rPointWorld = (fromH $ xform .*. (hPoint rPoint))
               in (rayPoint `vDist` rPointWorld, rPointWorld)
 in case roots of
    [r1, r2] | r1 > 0 -> Just $ dist r1
    [r1, r2] | r1 < 0 && r2 > 0 -> Just $ dist r2
    _ -> Nothing


-- Need to be changed to return similar to the case for Sphere
rayGeometryIntersect (Ray (r_orig_, r_dir_)) (Geometry (Tri v0 v1 v2, state)) = 
  let (xform, inverseXform, inverseTransposeXform) = head $ matrixStack state
      r_orig = fromH $ inverseXform .*. (hPoint r_orig_)
      r_dir = fromH $ inverseXform .*. (hVector r_dir_)

{-
      v0v1 = v1 `vMinus` v0
      v0v2 = v2 `vMinus` v0
      n = v0v1 `vCross` v0v2
      nDotRay = n `vDot` r_dir
      edgeOn = nDotRay == 0
      --ZZ
      noHit' =  -- trace ("nohit = " ++ show (edgeOn || behind || outside0 || outside1 || outside2) ) $
         edgeOn || behind || outside0 || outside1 || outside2
      d = n `vDot` v0
      t =  -(n `vDot` r_orig) / nDotRay
      -- behind = trace ("rayGeometryIntersect:Tri. t=" ++ show t ++ " ;; orig,dir=" ++
      --  show r_orig ++ show r_dir ++ "  ;; vo,v1,v2="++show [v0,v1,v2] )$ t < 0
      behind = t < 0
      pHit = r_orig `vPlus` (t `vScale` r_dir)

      v0p = pHit `vMinus` v0
      v = n `vDot` (v0v1 `vCross` v0p)
      outside0 = v < 0
      -- outside0 = trace ("outside0 = " ++ show (v<0) ) $v < 0
      
      v1p = pHit `vMinus` v1
      v1v2 = v2 `vMinus` v1
      w = n `vDot` (v1v2 `vCross` v1p)
      outside1 = w < 0
      -- outside1 = trace ("outside1 = " ++ show (w<0) ) $w < 0

      v2p = pHit `vMinus` v2
      v2v0 = v0 `vMinus` v2
      u = n `vDot` (v2v0 `vCross` v2p)
      outside2 = u < 0
      -- outside2 = trace ("outside2 = " ++ show (u<0) ) $u < 0
      -- rPoint = r_orig `vPlus` (vScale t r_dir)
-}

      -- ZZ
      zdist = rayTriIntersect' r_orig r_dir (v0,v1,v2)
      msg = "rayGeometryIntersect:: zdist=" ++ show zdist ++ " r_roig,r_dir=" ++ show (roundVec3100 r_orig) ++ " , "++ show (roundVec3100 r_dir) 
      noHit = {- trace  msg $-}  zdist < 0
	   
      zPoint = fst (rayPlaneIntersect r_orig r_dir v0 v1 v2)

      rPointWorld = fromH $ xform .*. (hPoint zPoint)
      tDist = r_orig_ `vDist` rPointWorld
	  --tDist  =  r_orig_ `vDist` (fromH $ xform .*. (hPoint rPoint))

  in if noHit then Nothing else Just (tDist, rPointWorld)



newtype Hit = Hit (Maybe (Double, Vec3, Geometry))
              deriving Show
hit0 = Hit Nothing

white = Vec3 (1, 1, 1)
black = Vec3 (0, 0, 0)
zero = black
id4 = identityMatrix 4
state0 = State {ambient = black, attenuation = black, diffuse = black, emission = black, specular = black,
                shininess = 0,
                maxDepth = 0,
                matrixStack = [(id4, id4, id4)],
                camera = Camera (zero, zero, zero, 0),
                output = "",
                size = (0, 0),
                vertices = array (0, -1) [],
                vertexList = [],
                maxVerts = 0
               }

world0 = World (state0, [], [])

buildWorld tokens = foldl' interpret world0 tokens

xs +. x = xs ++ [x]

interpret (World (state, lights, geometry)) (AmbientTok v) =
  World (state {ambient = v}, lights, geometry)
interpret (World (state, lights, geometry)) (AttenuationTok v) =
  World (state {attenuation = v}, lights, geometry)
interpret (World (state, lights, geometry)) (CameraTok eye center up fov) =
  World (state {camera = Camera (eye, center, up, fov * pi / 180)}, lights, geometry)
interpret (World (state, lights, geometry)) (DiffuseTok v) =
  World (state {diffuse = v}, lights, geometry)
interpret (World (state, lights, geometry)) (DirectionalTok u v) =
  World (state,  lights +. Light (DirectionalLight u v, state), geometry)
interpret (World (state, lights, geometry)) (EmissionTok v) =
  World (state {emission = v}, lights, geometry)
interpret (World (state, lights, geometry)) (MaxDepthTok i) =
  World (state {maxDepth = i}, lights, geometry)
interpret (World (state, lights, geometry)) (MaxvertsTok n) =
  World (state {maxVerts = n}, lights, geometry)
interpret (World (state, lights, geometry)) (OutputTok filePath) =
  World (state {output = filePath}, lights, geometry)
interpret (World (state, lights, geometry)) (PointTok u v) =
  World (state, lights +. Light (PointLight u v, state), geometry)
interpret (World (state, lights, geometry)) PopTransformTok =
  World (state {matrixStack = tail $ matrixStack state}, lights, geometry)
interpret (World (state, lights, geometry)) PushTransformTok =
  World (state {matrixStack = let m : ms = matrixStack state
                              in m : m : ms}, lights, geometry)
interpret (World (state, lights, geometry)) (RotateTok axis angle) =
  World (applyRotation state axis angle, lights, geometry)
interpret (World (state, lights, geometry)) (ScaleTok v) =
  World (applyScale state v, lights, geometry)
interpret (World (state, lights, geometry)) (ShininessTok s) =
  World (state {shininess = s}, lights, geometry)
interpret (World (state, lights, geometry)) (SizeTok w h) =
  World (state {size = (w, h)}, lights, geometry)
interpret (World (state, lights, geometry)) (SpecularTok v) =
  World (state {specular = v}, lights, geometry)
interpret (World (state, lights, geometry)) (SphereTok center radius) =
  World (state,  lights, geometry +. Geometry (Sphere center radius, state))
interpret (World (state, lights, geometry)) (TranslateTok v) =
  World (applyTranslation state v, lights, geometry)
interpret (World (state, lights, geometry)) (TriTok (Int3 (i, j, k))) =
  let v = vertices state
      tri = Geometry (Tri (v!i) (v!j) (v!k), state)
  in World (state, lights, geometry +. tri)
interpret (World (state, lights, geometry)) (VertexTok v) =
  let state' = state {vertexList = vertexList state +. v,
                      maxVerts = maxVerts state - 1}
      vl = vertexList state'
      nvl = length vl
      state'' = if maxVerts state' == 0 then
                  state' {vertices = listArray (0, nvl - 1) vl,
                          vertexList = []}
                else
                  state'
  in World (state'', lights, geometry)

state .*=. mat =
  let ((m, mInv, mInvTrans) : ms) = matrixStack state
      m' = m .*. mat
      m'Inv = inverse m'
      m'InvTrans = transpose m'Inv
  in state {matrixStack = (m', m'Inv, m'InvTrans) : ms}

applyRotation state axis angle =
  state .*=. rotationMatrix axis angle

applyTranslation state v =
  state .*=. translationMatrix v

applyScale state v =
  state .*=. scaleMatrix v

applyCamera state u v w fov =
  state .*=. cameraMatrix u v w fov (size state)

cameraMatrix u v w fov (width, height) = undefined

newtype Light = Light (LightShape, State) deriving (Show)
newtype Geometry = Geometry (GeometryShape, State) deriving (Show)

data LightShape = AmbientLight Vec3
                | PointLight Vec3 Vec3
                | DirectionalLight Vec3 Vec3
  deriving (Show)

data GeometryShape = Sphere Vec3 Double
                   | Tri Vec3 Vec3 Vec3
                   deriving (Show)

tokenizeLine line = tokenizeWords $ words line

tokenizeWords words = case words of [] ->
                                      Nothing
                                    (('#' : _) : _) ->
                                      Nothing
                                    ["ambient", r, g, b] ->
                                      Just $ AmbientTok (readVec3 r g b)
                                    ["attenuation", x, y, z] ->
                                      Just $ AttenuationTok (readVec3 x y z)
                                    ["camera", x1, y1, z1, x2, y2, z2, x3, y3, z3, a] ->
                                      Just $ CameraTok (readVec3 x1 y1 z1) (readVec3 x2 y2 z2) (readVec3 x3 y3 z3) (read a)
                                    ["diffuse", r, g, b] ->
                                      Just $ DiffuseTok (readVec3 r g b)
                                    ["directional", x1, y1, z1, x2, y2, z2] ->
                                      Just $ DirectionalTok (readVec3 x1 y1 z1) (readVec3 x2 y2 z2)
                                    ["emission", r, g, b] ->
                                      Just $ EmissionTok (readVec3 r g b)
                                    ["maxdepth", d] ->
                                      Just $ MaxDepthTok (readNumber d)
                                    ["maxverts", m] ->
                                      Just $ MaxvertsTok (read m)
                                    ["output", fname] ->
                                      Just $ OutputTok fname
                                    ["point", x1, y1, z1, x2, y2, z2] ->
                                      Just $ PointTok (readVec3 x1 y1 z1) (readVec3 x2 y2 z2)
                                    ["popTransform"] ->
                                      Just $ PopTransformTok
                                    ["pushTransform"] ->
                                      Just $ PushTransformTok
                                    ["rotate", x, y, z, a] ->
                                      Just $ RotateTok (readVec3 x y z) (readNumber a)
                                    ["scale", x, y, z] ->
                                      Just $ ScaleTok (readVec3 x y z)
                                    ["shininess", a] ->
                                      Just $ ShininessTok (read a)
                                    ["specular", r, g, b] ->
                                      Just $ SpecularTok (readVec3 r g b)
                                    ["sphere", x, y, z, r] ->
                                      Just $ SphereTok (readVec3 x y z) (readNumber r)
                                    ["size", s1, s2] ->
                                      Just $ SizeTok (readNumber s1) (readNumber s2)
                                    ["translate", x, y, z] ->
                                      Just $ TranslateTok (readVec3 x y z)
                                    ["tri", i, j, k] ->
                                      Just $ TriTok (readInt3 i j k)
                                    ["vertex", x, y, z] ->
                                      Just $ VertexTok (readVec3 x y z)
                                    _ -> error $ "can't tokenize line " ++ show words

tokenizeFile fname = readFile fname >>= return . tokenizeScene

tokenizeScene = catMaybes . map tokenizeLine . lines
                  
readNumber s = case s of ('+' : cs) -> readNumber cs
                         ('-' : cs) -> - readNumber cs
                         ('.' : cs) -> readNumber $ '0' : s
                         _          -> read s

readVec3 x y z = Vec3 (readNumber x, readNumber y, readNumber z)

readInt3 i j k = Int3 (readNumber i, readNumber j, readNumber k)

test = do files <- getDirectoryContents "."
          let files' = [file | file <- files, ".test" `isSuffixOf` file]
          print files'
          mapM_ renderScene files'

renderScene filename =
  do putStrLn filename
     tokens <- tokenizeFile filename
     let world@(World (state, lights, geometry)) = buildWorld tokens
         pngFileName = (output state)
         fname = pngFileName ++ ".ppm"
         ppm = render world
         -- command = unwords ["gm convert", fname, pngFileName]
     trace ( "Showing World: " ++ show world) $ writeFile fname (show ppm)
     --putStrLn command
     --exitCode <- system $ command
     --print exitCode
                       

          

{-=================================-}
{-   Zachi Baricentric Coordinates -}
{-=================================-}


-- We'll return the closest positive intersection, or -1 if none
rayTriIntersect' :: Vec3 -> Vec3 -> (Vec3,Vec3,Vec3) -> Double
rayTriIntersect'    ori     dir     (v1,v2,v3) =  if t>0  then {-trace msg2 $-} findDistBary ori v1 v2 v3 bCoords
                                                  else -1
                                            where
                                                t = snd $ (rayPlaneIntersect ori dir v1 v2 v3)
                                                bCoords = baryCoords v1 v2 v3  $ fst (rayPlaneIntersect ori dir v1 v2 v3)
                                                msg2 = "In ray TriIntersect:"++ "Ray(ori,dir)=" ++ show (roundVec3100 ori) ++ " , " ++ show (roundVec3100 dir) ++"dist=" ++ show (findDistBary ori v1 v2 v3 bCoords)
                                           

findDistBary :: Vec3->Vec3->Vec3->Vec3->(Double,Double,Double)->Double
findDistBary    ori   v1    v2    v3    (alpha , beta , gama) =
                                            if ( (alpha>=0) && (beta>=0) && (gama>=0) ) then 
                                                --trace ("In findDistBAry:: then is " ++  show(alpha) ++ show(beta)++show(gama)) $ 
                                                vMag ( (alpha `vScale` v1) `vPlus` (beta `vScale` v2) `vPlus` (gama `vScale` v3) `vMinus` ori)
                                            else 
                                                --trace ("In findDistBAry:: else is " ++ "ori=" ++ show(ori) ++
                                                --"  v1=" ++ show(v1) ++ " ;; " ++ show(alpha) ++ " , " ++ show(beta)++ " , " ++ show(gama)) $ 
                                                (-1.0)


baryCoords :: Vec3->Vec3->Vec3->Vec3->(Double,Double,Double)
baryCoords    v1     v2    v3    p   = --trace ("In baryCoords:: " ++ "v1=" ++ show v1 ++
                                       --         "  v2=" ++ show v2 ++ "  v3=" ++ show v3 ++ "  p=" ++ show p ++ " ;; " ++ show(alpha) ++ " , " ++ show(beta)++ " , " ++ show(gama)  ) $ 
                                        (alpha,beta,gama)
                                        where
                                                aa = p `vMinus` v1
                                                bb = v2 `vMinus` v1
                                                cc = v3 `vMinus` v1

                                                sol = solveBary' aa bb cc
                                                alpha = fst sol
                                                beta = snd sol
                                                gama = 1 - alpha - beta

solveBary' :: Vec3 -> Vec3 -> Vec3 -> (Double,Double)
solveBary' (Vec3(a1,a2,a3)) (Vec3(b1,b2,b3)) (Vec3(c1,c2,c3)) = if (det1==0) then if (det2==0) then (alpha3,beta3)
                                                    else (alpha2,beta2) 
                                    else (alpha1,beta1)
    where
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

rayPlaneIntersect :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> (Vec3,Double)
rayPlaneIntersect    ori     dir     v1      v2      v3 =  {- trace ("In rayPlaneIntersect:" ++ "ori=" ++ show (roundVec3100 ori) ++ "  dir=" ++ show (roundVec3100 dir) ++ " point: " ++ 
                                                        show (roundVec3100(ori `vPlus` ( t `vScale` dir)) ) ) $  -}
                                                            (ori `vPlus` (t `vScale` dir) , t)
                                                            where 
                                                                n' =  (v3 `vMinus` v1) `vCross` (v2 `vMinus` v1)
                                                                n  =  vNorm n'
                                                                t  = ( (v1 `vDot` n) - (ori `vDot` n) ) / (dir `vDot` n)

