module Utils (clamp, degToRad, radToDeg) where

clamp :: Double -> Double
clamp r | r < 0.0 = 0.0
        | r > 1.0 = 1.0
        | otherwise = r

degToRad :: Double -> Double
degToRad x = (x / 180.0) * pi

radToDeg :: Double -> Double
radToDeg x = (x / pi) * 180.0
