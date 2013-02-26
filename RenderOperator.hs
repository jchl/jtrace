module RenderOperator (render) where

import Types
import System.IO
import Ppm
import Utils
import qualified Render

teamName = "James's awesome team"

doRender :: String -> Int -> Int -> Double -> Int -> Object -> [Light] -> Point -> IO ()
doRender file h w fov d o lights amb =
   do let ps = Render.render h w fov d o lights amb
      h <- openBinaryFile file WriteMode
      writePpm h teamName (map (map pointToPixel) ps)
      hClose h
   where
      pointToPixel (r, g, b) = (scale r, scale g, scale b)
      scale v = (floor . (* (256.0 - epsilon)) . clamp) v -- fails when written pointfree!
      epsilon = 1e-13 -- XXX bit of a hack

render :: Stack -> IO Stack
render ((VString file):(VInt h):(VInt w):(VReal fov):(VInt d):(VObject o):(VArray lightArray):(VPoint amb):xs) =
   do putStrLn $ "Rendering " ++ show o ++ " to " ++ file
      let lights = [l | VLight l <- lightArray]
      doRender file h w fov d o lights amb
      return xs
render s = error $ "Error in render: " ++ show s

