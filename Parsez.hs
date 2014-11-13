module Parsez where

import VecMat
import Color
import PPMUtils
import Render

import Debug.Trace



data Token =  EmptyToken
            | Comment   String
            | EmptyLine 
            | TImg      Img
            | TCamera   Camera 
            | TDirectional  Light
            | TPoint    Light
            | TAmbient  Color
            | TDiffuse  Color
            | TSpecular Color
            | TMaxverts Int
            | TVertex   Vec3
            | TTri      Vec3
            | TSphere   Vec3 Double
            | UnKnown   String
            deriving (Show)



textToWorld :: String -> World 
textToWorld text = tokensToWorld $ textToTokens text

textToTokens :: String -> [Token]
textToTokens text = map  (wordsToToken.wordsz) (lines text)

-- Takes care of ".1" rather than "0.1", and "1" instead of "+1"
wordsz :: String -> [String]
wordsz s =  map (add0.remP) (words s)
add0 :: String -> String
add0 [] = []
add0 ('.':s)  = "0."++s 
add0 ('-':s)  = if head(s)=='.' then "-0"++s else '-':s
add0 s = s

remP :: String -> String
remP [] = []
remP s  = if head(s)=='+' then tail s else s


wordsToToken :: [String] -> Token
wordsToToken [] = EmptyToken

wordsToToken ws | head(head(ws))=='#'   = Comment   (unwords ws)
                | head(head(ws))==' '   = EmptyLine
                | head(ws)=="size"      = TImg      (Img (read (ws!!1)) (read (ws!!2)) [] )
                | head(ws)=="camera"    = TCamera   (Camera     (Vec3 $ tuplify3(map read (take 3 (drop 1 ws)   ))) 
                                                            (Vec3 $ tuplify3(map read (take 3 (drop 4 ws) )))
                                                            (Vec3 $ tuplify3(map read (take 3 (drop 7 ws) )))
                                                            ((read (head(drop 10 ws)))*pi/180) )
                -- Lights
                | head(ws)=="directional" = TDirectional    (Directional (Vec3  $ tuplify3(map read (take 3 (drop 1 ws)   )))
                                                                (Vec3 $ tuplify3(map read (take 3 (drop 4 ws)   ))) )
                | head(ws)=="point"     = TPoint    (Point  (Vec3  $ tuplify3(map read (take 3 (drop 1 ws)   )))
                                                                (Vec3 $ tuplify3(map read (take 3 (drop 4 ws)   ))) )
                -- Materials
                | head(ws)=="ambient"   = TAmbient  ( Vec3 $ tuplify3(map read (take 3 (drop 1 ws)   )))
                | head(ws)=="diffuse"   = TDiffuse  ( Vec3 $ tuplify3(map read (take 3 (drop 1 ws)   ))) 
                | head(ws)=="specular"  = TSpecular ( Vec3 $ tuplify3(map read (take 3 (drop 1 ws)   ))) 
                -- Geometry
                | head(ws)=="maxverts"  = TMaxverts ( read  (head (drop 1 ws) )  )
                | head(ws)=="vertex"    = TVertex   ( Vec3 $ tuplify3(map read (take 3 (drop 1 ws)   ))) 
                | head(ws)=="tri"       = TTri      ( Vec3 $ tuplify3(map read (take 3 (drop 1 ws)   ))) 
                | head(ws)=="sphere"    = TSphere   ( Vec3 $ tuplify3(map read (take 3 (drop 1 ws)   )))  (read (head (drop 4 ws))) 

                | otherwise             = UnKnown (unwords ws)




-- That's one way to set it. We are doing it above with State machine.
tokensToWorld :: [Token] -> World
tokensToWorld t = World [x | TImg x <- t] [x | TCamera x <- t] (createObject t) (isLightToken t)


isLightToken :: [Token] -> [Light]
isLightToken t =( [x | TPoint x <- t] ++ [x | TDirectional x <- t])


createObject :: [Token] -> [Object]
createObject t =    (createTri t (Material vec30 vec30 vec30) [vec30]) ++ 
                    (createSphere t (Material vec30 vec30 vec30))

createTri :: [Token] -> Material -> [Vec3] -> [Object]
createTri [] _ _ = []
createTri ((TSpecular v):ts) m vlist    = trace "In Tri Specular" $ createTri ts (Material (ambient m) (diffuse m) v ) vlist
createTri ((TDiffuse  v):ts) m vlist    = trace "In Tri Diffuse"  $ createTri ts (Material (ambient m) v (specular m)) vlist
createTri ((TAmbient  v):ts) m vlist    = trace "In Tri Ambient"  $ createTri ts (Material v (diffuse m) (specular m)) vlist
createTri ((TMaxverts s):ts) m vlist    = createTri ts m []
createTri ((TVertex   v):ts) m vlist    = createTri ts m (vlist++[v])
createTri ((TTri      v):ts) m vlist    = trace "In Tri"  $ 
            (Tri (vlist!!( round $ vec3x v)) (vlist!!( round $ vec3y v)) (vlist!!( round $ vec3z v)) m vec30) :
                                                                     createTri ts m vlist
createTri (t:ts) m vlist                = createTri ts m vlist

--createSphere :: [Token] -> Material -> [Object]
--createSphere _ _ = []


createSphere :: [Token] -> Material -> [Object]
createSphere [] _ = []
--createSphere t m = trace "In Sphere1" $ [ Sphere x y (Material vec30 vec30 vec30) | TSphere x y <- t]
createSphere ((TSpecular v):ts) m   =  createSphere ts (Material (ambient m) (diffuse m) v )
createSphere ((TDiffuse v):ts) m    =  createSphere ts (Material (ambient m) v (specular m))
createSphere ((TAmbient v):ts) m    =  createSphere ts (Material v (diffuse m) (specular m))
createSphere ((TSphere c r):ts) m   = trace "In Sphere" $ (Sphere c r m) : createSphere ts m
createSphere (t:ts) m = createSphere ts m




{-
processTokens :: [Token] -> World
processTokens t = processTokensWithStates t [] [] []

processTokensWithStates :: [Token] ->Material  -> World
processTokensWithStates t a _ _ = if ambient  processTokensWithStates 

-}

--World camera [object] [light]




{-
parseLines :: [String] -> Bool
parseLines [] = True
parseLines (x:xs) = undefined

parseImg :: lineList -> Img
parseImg (x:xs) | head(w) == "size" = Img (read w!!1) (read w!!2) []
                | otherwise         = parseImg xs
                where
                    w = words x

-}