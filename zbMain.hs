module Main where
import System.Environment (getArgs)


import Debug.Trace
-- func a b    = trace ("In Tri Specular" ++ show a ++ ahow b) $ a+b


import Data.List
import Data.Maybe

import VecMat
import Color
import PPMUtils   -- (Img, createPPM)


import Render
import Parsez




main :: IO ()
main = do args <- getArgs
          let (infln,outfln) = case args of
                [t,t'] -> (t,t')
                [t]  -> (t,"qq")
                _ -> error "Need one or two arguments: SceneFile  [OutFile]"
          text <- readFile infln
                    
          --print $ text
          --putStr $ show (renderFromText text )

          let ppmImg = renderFromText text
          writeFile outfln ppmImg
          print $ "Done with rendering:"



--renderFromText :: String -> [Token]
--renderFromText text = textToTokens text


-- renderFromText :: String -> World
-- renderFromText text = tokensToWorld $ textToTokens text

renderFromText :: String -> String
renderFromText text = render $ textToWorld text

