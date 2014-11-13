module LinearAlgebra where

import Data.Array

newtype Vec3 = Vec3 (Double, Double, Double)
             deriving (Show, Eq, Ord)

vScale a (Vec3 (x, y, z)) = Vec3 (a*x, a*y, a*z)

vNorm v = (1 / vMag v) `vScale` v

vMag v = sqrt $ v `vDot` v

(Vec3 (x1, y1, z1)) `vPlus` (Vec3 (x2, y2, z2)) = Vec3 (x1 + x2, y1 + y2, z1 + z2)

(Vec3 (x1, y1, z1)) `vMinus` (Vec3 (x2, y2, z2)) = Vec3 (x1 - x2, y1 - y2, z1 - z2)

Vec3 (u1, u2, u3) `vCross` Vec3 (v1, v2, v3) =
  Vec3 (u2*v3 - v2*u3, u1*v3 - v1*u3, u1*v2 - v1*u2)

(Vec3 (x1, y1, z1)) `vDot` (Vec3 (x2, y2, z2)) = x1*x2 + y1*y2 + z1*z2

newtype Vec4 = Vec4 (Double, Double, Double, Double)
             deriving (Show, Eq, Ord)

newtype Int3 = Int3 (Int, Int, Int)
             deriving (Show, Eq, Ord)

type MatrixData = Array (Int, Int) Double
newtype Matrix = Matrix (MatrixData) deriving (Eq, Ord)

instance Show Matrix where
  show (Matrix arr) =
    let ((rmin, cmin), (rmax, cmax)) = bounds arr
        showRow r = unwords $ map show [arr ! (r, c) | c <- [cmin .. cmax]]
    in unlines $ map showRow [rmin .. rmax]
                  
matrixFromList r c = Matrix . listArray ((1, 1), (r, c)) 

identityMatrix n = Matrix $ array ((1, 1), (n, n)) [((r, c), if r == c then 1 else 0)
                                                   | r <- [1 .. n], c <- [1 .. n]]

(Matrix md1) .*. (Matrix md2) = Matrix (md1 `mm` md2)
(Matrix md1) .+. (Matrix md2) = Matrix (md1 `ma` md2)
a *. (Matrix m) = Matrix (a `sm` m)

mm :: MatrixData -> MatrixData -> MatrixData
mm m1 m2 = let ((rmin1, cmin1), (rmax1, cmax1)) = bounds m1
               ((rmin2, cmin2), (rmax2, cmax2)) = bounds m2
               assocs = [((r, c), sum [m1 ! (r, i) * m2 ! (i, c) | i <- [cmin1 .. cmax1]])
                         | r <- [rmin1 .. rmax1],
                           c <- [cmin2 .. cmax2]]
           in array ((rmin1, cmin2), (rmax1, cmax2)) assocs

sm :: Double -> MatrixData -> MatrixData
sm a m = listArray (bounds m) $ map (a *) $ elems m

ma :: MatrixData -> MatrixData -> MatrixData
ma m1 m2 = listArray (bounds m1) $ [x + y | (x, y) <- zip (elems m1) (elems m2)]

toH h (Vec3 (x, y, z)) = matrixFromList 4 1 [x, y, z, h]
hVector = toH 0
hPoint = toH 1

fromH (Matrix mat) =
  let x : y : z : _ = elems mat
  in Vec3 (x, y, z)

det2 a b
     c d = a*d - b*c

det3 a b c
     d e f
     g h i = a*(det2 e f h i) - b*(det2 d f g i) + c*(det2 d e g h)

det4 a b c d
     e f g h
     i j k l
     m n o p =   a*(det3 f g h j k l n o p)
               - b*(det3 e g h i k l m o p)
               + c*(det3 e f h i j l m n p)
               - d*(det3 e f g i j k m n o)

det (Matrix mat) =
  let (_, (n, n')) = bounds mat
  in if n /= n' then error "can't get determinant of non-square matrix"
     else case n of
       1 -> let [a] = elems mat
            in a
       2 -> let [a, b, c, d] = elems mat
            in det2 a b c d
       3 -> let [a, b, c, d, e, f, g, h, i] = elems mat
            in det3 a b c d e f g h i
       4 -> let [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] = elems mat
            in det4 a b c d e f g h i j k l m n o p
       _ -> error $ "can't get determinant of matrix of rank " ++ show n


transpose (Matrix mat) =
  let b@((rMin, cMin), (rMax, cMax)) = bounds mat
      elements = [mat ! (c, r) | r <- [rMin .. rMax], c <- [cMin .. cMax]]
  in Matrix (listArray b elements)

cofactor (Matrix mat) i j =
  let ((rMin, cMin), (rMax, cMax)) = bounds mat
      elements = [mat ! (r, c)
                  | r <- [rMin .. rMax],
                    c <- [cMin .. cMax],
                    r /= i,
                    c /= j]
      minor = Matrix (listArray ((rMin, cMin), (rMax - 1, cMax - 1)) elements)
  in (-1)^(i + j) * det minor

inverse m@(Matrix mat) =
  let b@((rMin, cMin), (rMax, cMax)) = bounds mat
      cofactors = [cofactor m r c
                  | r <- [rMin .. rMax],
                    c <- [cMin .. cMax]]
      cofactorMatrix = matrixFromList rMax cMax cofactors
      determinant = det m
  in (1 / determinant) *. transpose cofactorMatrix
