module WritePPM where

import Data.Array

newtype Pixel = Pixel (Double, Double, Double)

instance Show Pixel where
  show (Pixel (r, g, b)) =
    let r' = round $ 255 * r
        g' = round $ 255 * g
        b' = round $ 255 * b
    in unwords $ map show [r', g', b']

newtype PPM = PPM (Array (Int, Int) Pixel)

instance Show PPM where
  show (PPM pixels) =
    let ((rMin, cMin), (rMax, cMax)) = bounds pixels
        fileLines = headerLine : dataLines
        headerLine = unwords ["P3", show $ cMax - cMin + 1, show $ rMax - rMin + 1, "255"]
        dataLines = [unwords [show (pixels!(r, c)) | c <- [cMin .. cMax]]
                    | r <- [rMin .. rMax]]
    in unlines fileLines

test = writeFile "test.ppm" (show (PPM (listArray ((0, 0), (10, 20)) (repeat (Pixel (0.21, 0.41, 0.61))))))
