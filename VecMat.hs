module VecMat where

import Debug.Trace

-- How do i make Vec3 general, to work on ANY number?
data Vec3   =   Vec3 (Double,Double,Double)
                deriving (Show)

roundVec3100 :: Vec3 -> Vec3
roundVec3100 (Vec3 (x,y,z)) = Vec3 (round100 x, round100 y,round100 z)

round100 :: Double -> Double
round100 x = (fromIntegral $ round (100*x))/100

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)

vec3x :: Vec3 -> Double
vec3x ( Vec3 (x,_,_) ) = x
vec3y :: Vec3 -> Double
vec3y ( Vec3 (_,y,_) ) = y
vec3z :: Vec3 -> Double
vec3z ( Vec3 (_,_,z) ) = z

vec30 = Vec3 (0,0,0)
vecT1 = Vec3 (1,2,3)
vecT2 = Vec3 (4,5,6)

vec3FromInt :: Int -> Vec3
vec3FromInt x = Vec3 (y,y,y)
                where y = fromIntegral x
vec3Fromf :: Double -> Vec3
vec3Fromf x = Vec3 (x,x,x)
             
vec3ToList :: Vec3 -> [Double]
vec3ToList (Vec3 (x,y,z)) = [x,y,z]                

dotVec3 :: Vec3 -> Vec3 -> Double
dotVec3 (Vec3 (a,b,c)) (Vec3 (d,e,f)) = a*d + b*e + c*f

crossVec3 :: Vec3 -> Vec3 -> Vec3
crossVec3 (Vec3 (a,b,c)) (Vec3 (d,e,f)) = Vec3 (b*f-c*e, -a*f+c*d, a*e-b*d) 

normVec3 :: Vec3 -> Double
normVec3 v = sqrt $ dotVec3 v v

normalizeVec3 :: Vec3 -> Vec3
normalizeVec3 v = Vec3 ( (vec3x v)/normV , (vec3y v)/normV , (vec3z v)/normV )
                where normV = normVec3 v
subVec3 :: Vec3 -> Vec3 -> Vec3
subVec3 (Vec3 (a,b,c)) (Vec3 (d,e,f)) = Vec3 (a-d,b-e,c-f)
addVec3 :: Vec3 -> Vec3 -> Vec3
addVec3 (Vec3 (a,b,c)) (Vec3 (d,e,f)) = Vec3 (a+d,b+e,c+f)
divVec3 :: Vec3 -> Double -> Vec3
divVec3 (Vec3 (a,b,c)) s              = Vec3 (a/s, b/s, c/s)
mulVec3 :: Double -> Vec3 -> Vec3
mulVec3 s (Vec3 (a,b,c))              = Vec3 (a*s, b*s, c*s)

-- Hope the below would work!
-- you can add, subtract, check equality of and multiply vectors term-by-term
instance Num Vec3 where
    (+) x y = addVec3 x y
    (-) x y = subVec3 x y
--    (*) x y = dotVec3 x y
    negate x =  subVec3 vec30 x
--    abs x   = normVec3 x 
    abs x      = error "abs vec3 :: would be nice to have!"
    (*) x      = error "(*) vec3 :: would be nice to have!"
    signum x      = error "signum vec3 :: you probably didn't mean to do that"
    fromInteger x = error "fromInteger vec3 :: you probably didn't mean to do that"

-- We'll just do it as list. Not limiting to only 9 elements!
-- Row-Major order!!  (so rows are continous in memory)
data Mat3   =   Mat3 [Double]
                deriving (Show)

transposeMat3 :: Mat3 -> Mat3
transposeMat3 (Mat3 [ a, b, c, 
                      d, e, f, 
                      g, h, i]) = Mat3 [a,d,g,b,e,h,c,f,i]

mulMat3Vec3 :: Mat3 -> Vec3 -> Vec3
mulMat3Vec3 (Mat3 [ a, b, c, 
                    d, e, f, 
                    g, h, i]) (Vec3 (x,y,z)) = 
                Vec3 ( a*x +b*y+c*z , d*x+e*y+f*z , g*x+h*y+i*z )

mulMat3Mat3 :: Mat3 -> Mat3 -> Mat3
mulMat3Mat3 (Mat3 [ a, b, c, 
                    d, e, f, 
                    g, h, i]) 
            (Mat3 [ a', b', c', 
                    d', e', f', 
                    g', h', i'])  =
                    Mat3 [ dotVec3 r1 c1, dotVec3 r1 c2, dotVec3 r1 c3,
                           dotVec3 r2 c1, dotVec3 r2 c2, dotVec3 r2 c3,
                           dotVec3 r3 c1, dotVec3 r3 c2, dotVec3 r3 c3 ]
                    where
                        r1 = Vec3(a,b,c) 
                        r2 = Vec3(d,e,f) 
                        r3 = Vec3(g,h,i) 
                        c1 = Vec3(a',d',g') 
                        c2 = Vec3(b',e',h') 
                        c3 = Vec3(c',f',i') 

mulMat3 :: Double -> Mat3 -> Mat3
mulMat3 s (Mat3 m) = Mat3 (map (s*) m)


aInv = Mat3 [1,2,3,4,6,5,9,8,7]
testInvMat3a = mulMat3Mat3 aInv (invMat3 aInv)


invMat3 :: Mat3 -> Mat3
invMat3 (Mat3 [ a11, a12, a13, 
                a21, a22, a23, 
                a31, a32, a33]) =
                    mulMat3 (1.0/det) (Mat3 [
                            a22*a33-a23*a32, a13*a32-a12*a33, a12*a23-a13*a22,
                            a23*a31-a21*a33, a11*a33-a13*a31, a13*a21-a11*a23,
                            a21*a32-a22*a31, a12*a31-a11*a32, a11*a22-a12*a21
                            ])
                    where
                        det =   a11*(a22*a33-a23*a32) - 
                                a12*(a21*a33-a23*a31) + 
                                a13*(a21*a32-a22*a31)

data Vec4   =   Vec4 (Double,Double,Double,Double)
                deriving (Show)



tuplify4 :: [a] -> (a,a,a,a)
tuplify4 [x,y,z,w] = (x,y,z,w)



