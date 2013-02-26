module Ppm (Pixel, writePpm) where

import System.IO

type Pixel = (Int, Int, Int)

magic = "P6"
maxColorComponent = 255

writePpmHeader :: Handle -> Int -> Int -> String -> IO ()
writePpmHeader h width height comment =
   do hPutStrLn h $ magic
      hPutStrLn h $ "#" ++ comment
      hPutStrLn h $ show width ++ " " ++ show height
      hPutStrLn h $ show maxColorComponent

writePpmDatum :: Handle -> Pixel -> IO ()
writePpmDatum h (r, g, b) =
   do hPutChar h $ toEnum r
      hPutChar h $ toEnum g
      hPutChar h $ toEnum b

writePpmData :: Handle -> [Pixel] -> IO ()
writePpmData h = mapM_ (writePpmDatum h)

writePpm :: Handle -> String -> [[Pixel]] -> IO ()
writePpm h comment d = do hSetBinaryMode h True
                          writePpmHeader h width height comment
                          writePpmData h (concat d)
   where
      height = length d
      width = length (head d)
