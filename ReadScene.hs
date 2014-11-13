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

cameraRay :: State -> Int -> Int -> Ray 
cameraRay state r_ c_ =
  let x = fromIntegral c_
      y = fromIntegral r_
      (Camera (eye, center, up, fov)) = camera state
      (width_, height_) = size state
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
  in {-trace msg-} result

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
        (cos theta *. identityMatrix 3)
        .+.
        ((1 - cos theta) *. matrixFromList 3 3 [x^2, x*y, x*z,
                                                x*y, y^2, y*z,
                                                x*z, y*z, z^2])
        .+.
        (sin theta *. matrixFromList 3 3 [0, -z, y,
                                          z, 0, -x,
                                          -y, x, 0])
      spec = assocs m3 ++ [((4, 1), 0), ((4, 2), 0), ((4, 3), 0),
                           ((4, 4), 1),
                           ((1, 4), 0), ((2, 4), 0), ((3, 4), 0)]
  in Matrix $ array ((1, 1), (4, 4)) spec

data State = State {ambient, attenuation, diffuse, emission, specular :: Vec3,
                    shininess :: Double,
                    maxDepth :: Int,
                    matrixStack :: [(Matrix, Matrix)],
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
      ppm = PPM (listArray ((0, 0), (height, width)) pixels)
      pixels = [findColor world r c |
                r <- [0 .. height],
                c <- [0 .. width]]
      (Camera (eye, center, up, fov)) = camera state
      msg = show $ (eye `vMinus` center) `vDot` up
  in trace msg ppm

findColor world@(World (state, lights, geometry)) r c =
  let ray = cameraRay state r c
      Hit (distance, object) = raySceneIntersect ray world
  in case object of Just (Geometry (shape, objectState)) ->
                      let Vec3 rgb = ambient objectState
                          in Pixel rgb
                    Nothing -> Pixel (0, 0, 0)

raySceneIntersect ray (World (state, lights, geometry)) =
  foldl' (rayObjectIntersect ray) hit0 geometry

closer d prevD = case prevD of Nothing -> True
                               Just d' -> d < d'

rayObjectIntersect :: Ray -> Hit -> Geometry -> Hit
rayObjectIntersect ray previousHit@(Hit (distance, hitObject)) object =
  case rayGeometryIntersect ray object of
    Nothing -> previousHit
    Just d -> if closer d distance then Hit (Just d, Just object) else previousHit

rayGeometryIntersect :: Ray -> Geometry -> Maybe Double

rayGeometryIntersect (Ray (rayPoint, rayDirection)) (Geometry (Sphere c r, state)) =
  let (xform, inverseXform) = head $ matrixStack state
      p0 = fromH $ inverseXform .*. (hPoint rayPoint)
      p1 = fromH $ inverseXform .*. (hVector rayDirection)
      p0_c = p0 `vMinus` c
      a0 = p1 `vDot` p1
      a1 = 2*(p1 `vDot` p0_c)
      a2 = (p0_c `vDot` p0_c) - r^2
      roots = quadraticRoots a0 a1 a2
 in case roots of
    [r1, r2] | r1 > 0 -> Just r1
    [r1, r2] | r1 < 0 && r2 > 0 -> Just r2
    _ -> Nothing

rayGeometryIntersect (Ray (r_orig, r_dir)) (Geometry (Tri v0 v1 v2, state)) = 
  let v0v1 = v1 `vMinus` v0
      v0v2 = v2 `vMinus` v0
      n = v0v1 `vCross` v0v2
      nDotRay = n `vDot` r_dir
      edgeOn = nDotRay == 0
      noHit = edgeOn || behind || outside0 || outside1 || outside2
      d = n `vDot` v0
      t = -(n `vDot` r_orig) / nDotRay
      behind = t < 0
      pHit = r_orig `vPlus` (t `vScale` r_dir)

      v0p = pHit `vMinus` v0
      v = n `vDot` (v0v1 `vCross` v0p)
      outside0 = v < 0
      
      v1p = pHit `vMinus` v1
      v1v2 = v2 `vMinus` v1
      w = n `vDot` (v1v2 `vCross` v1p)
      outside1 = w < 0

      v2p = pHit `vMinus` v2
      v2v0 = v0 `vMinus` v2
      u = n `vDot` (v2v0 `vCross` v2p)
      outside2 = u < 0

  in if noHit then Nothing else Just t

newtype Hit = Hit (Maybe Double, Maybe Geometry)
              deriving Show
hit0 = Hit (Nothing, Nothing)

white = Vec3 (1, 1, 1)
black = Vec3 (0, 0, 0)
zero = black
id4 = identityMatrix 4
state0 = State {ambient = black, attenuation = black, diffuse = black, emission = black, specular = black,
                shininess = 0,
                maxDepth = 0,
                matrixStack = [(id4, id4)],
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
  let ((m, mInv) : ms) = matrixStack state
      m' = m .*. mat
      m'Inv = inverse m'
  in state {matrixStack = (m', m'Inv) : ms}

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
         command = unwords ["gm convert", fname, pngFileName]
     writeFile fname (show ppm)
     putStrLn command
     exitCode <- system $ command
     print exitCode
                       

          


