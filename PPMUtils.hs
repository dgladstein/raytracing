module PPMUtils where

import VecMat
import Color


data Img =  Img {
                width   :: Int,
                height  :: Int,
                pixels  :: [Color]
            } deriving (Show)





{-******************************************-}
{-    PPMutils  -}
{-******************************************-}


-- Taken almost verbatim from : http://www.ryanlothian.com/projects/haskell_raytracer/


-- Read on PPM format here:  http://en.wikipedia.org/wiki/Netpbm_format
-- Version P3: ASCII, 0-255 RGB
-- Version P6: Similar to P3, Binary.

-- Images are stored and amnipulated left-to-right, to-to-bottom.

-- Color is represented by a list of Double.
-- Image is width, height, and list of Colors


-- This doesn't capture the fact that:
--  Image has exactly width*height velues of Color.
-- Haskell puts alrady the 'get' functions, like   (width Img)

 
 -- Does not cpature the fact there are ONLY 3 colors.
-- But, anyway, this is just an internal PPM variable

data ColorPPM = ColorPPM [Int]
                deriving (Show)

-- Create a string suitable for saving as PPM file

createPPM :: Img -> String
createPPM (Img w h im) = join' "\n" lineList ++ "\n"
    where lineList = (["P3", show w ++ " " ++ show h, "255"] ++ 
                            map (join'  " ".map show) (scaleToImgPPM im)  )

-- Converts list of colors in range [0,1], 
-- to list of [list of integers] in the range [0,255]
-- aa=[ vec3Fromf 0.1,  vec3Fromf 0.2,  vec3Fromf 0.8]
-- bb=scaleToImgPPM aa

scaleToImgPPM :: [Color] -> [[Int]]
scaleToImgPPM []     = []
scaleToImgPPM (c:cs) =  [ colorToColorPPM c ] ++ (scaleToImgPPM cs)


colorToColorPPM :: Color -> [Int]
colorToColorPPM (Vec3 (r,g,b)) = [round (255*r) :: Int, round (255*g) :: Int, round (255*b) :: Int]


{-
colorToListPPM :: Color -> [Int]
colorToListPPM (Vec3 (r,g,b)) = map (round $ (*)255) [r,g,b]


-}
-- Join a list of strings using a separator string.
join' :: String -> [String] ->String
join' sep [x]    = x
join' sep (x:xs) = x ++ sep ++ join' sep xs

{-
-- Test for "createPpm"
testCreatePpm :: Bool
testCreatePpm =  createPpm (2, 3, [[11, 12, 13], [22, 23, 24],
                                      [33, 34, 35], [44, 45, 46],
                                      [55, 56, 57], [66, 67, 68]]) ==
                   "P3\n2 3\n255\n11 12 13\n22 23 24\n33 34 35\n44 45 46\n" ++
                   "55 56 57\n66 67 68\n"


-- Produce a sample file "test0.ppm"

produceSamplePpmGr = writeFile "test0Gr.ppm" filedata
                    where 
                        mkcol x = [x,x,x]
                        filedata = createPpm (16, 16, map mkcol [0..255])

produceSamplePpmR = writeFile "test0R.ppm" filedata
                    where 
                        filedata = createPpm (16, 16, map (\x -> [x,0,0]) [0..255])


produceSamplePpmG = writeFile "test0G.ppm" filedata
                    where 
                        filedata = createPpm (16, 16, map (\x -> [0,x,0]) [0..255])

produceSamplePpmB = writeFile "test0B.ppm" filedata
                    where 
                        filedata = createPpm (16, 16, map (\x -> [0,0,x]) [0..255])


-}
-- Tet for "join"
testJoin :: Bool
testJoin = join' "t" ["12", "3", "", "xy"]  == "12t3ttxy"


{-******************************************-}
{-******************************************-}


