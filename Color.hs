module Color where

import VecMat

-- Integer color and Float color
type Color  =   Vec3
colorR :: Color -> Double
colorR ( Vec3 (x,_,_) ) = x
colorG :: Color -> Double
colorG ( Vec3 (_,y,_) ) = y
colorB :: Color -> Double
colorB ( Vec3 (_,_,z) ) = z