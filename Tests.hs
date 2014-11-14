module Tests where

import System.Random

import qualified Render as R
import qualified ReadScene as RS
import qualified LinearAlgebra as LA
import qualified VecMat as V

testCamera = let eye = LA.Vec3 (0, 4, -4)
                 eye' = V.Vec3 (0, 4, -4)
                 center = LA.Vec3 (0, 0, 0)
                 center' = V.Vec3 (0, 0, 0)
                 up = LA.Vec3 (1, 1, 0)
                 up' = V.Vec3 (1, 1, 0)

                 fov = 45
                 width = 480
                 height = 360
                 r = 200
                 c = 300
                 RS.Ray (origin, direction) = RS.rayFromPixel (RS.Camera (eye, center, up, fov * pi / 180)) width height c r

                 c' = R.Camera eye' center' up' $ fov * pi / 180
                 R.Ray origin' direction' = R.rayFromPixel c' (width, height) (fromIntegral r, fromIntegral c)
             in do print origin
                   print origin'
                   print direction
                   print direction'
                       
testCross = let v1 = LA.Vec3 (0.1, 0.2, 0.3)
                v1' = V.Vec3 (0.1, 0.2, 0.3)

                v2 = LA.Vec3 (32.2, -0.32, 0.24)
                v2' = V.Vec3 (32.2, -0.32, 0.24)

                cross = v1 `LA.vCross` v2
                cross' = V.crossVec3 v1' v2'
            in (cross, cross')
             
stdUniform :: IO Double
stdUniform = randomIO

testCross2 = do v1x <- stdUniform
                v1y <- stdUniform
                v1z <- stdUniform
                v2x <- stdUniform
                v2y <- stdUniform
                v2z <- stdUniform
                let v1 = LA.Vec3 (v1x, v1y, v1z)
                    v1' = V.Vec3 (v1x, v1y, v1z)
                    v2 = LA.Vec3 (v2x, v2y, v2z)
                    v2' = V.Vec3 (v2x, v2y, v2z)
                    cross = v1 `LA.vCross` v2
                    cross' = V.crossVec3 v1' v2'
                print cross
                print cross'
             

