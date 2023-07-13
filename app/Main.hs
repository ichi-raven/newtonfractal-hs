module Main where

import qualified Codec.Picture as Pic
import Data.Complex
import Data.List

-- user definition
-- target (complex) func
kf :: Complex Double -> Complex Double
kf z = z ** 3 - 1

-- derivative of target func
kf' :: Complex Double -> Complex Double
kf' z = 3.0 * (z ** 2)

-- analytic solutions
kRoots :: [Complex Double]
kRoots = [1.0 :+ 0.0, (-0.5) :+ (sqrt 3.0 / 2.0), (-0.5) :+ (-sqrt 3.0 / 2.0)]

-- must be same dimentions with roots
kColors :: [Pic.PixelRGB8]
kColors = [red, green, blue]
    where 
        red     = Pic.PixelRGB8 220 15 15
        green   = Pic.PixelRGB8 15 220 15
        blue    = Pic.PixelRGB8 15 15 220

kColorMap :: [(Complex Double, Pic.PixelRGB8)]
kColorMap = zip kRoots kColors
-- end of user definition

newton::Fractional a => (a -> a) -> (a -> a) -> a -> Int -> a
newton f f' now step | step <= 0 = now
                     | otherwise = newton f f' next (step - 1)
                                where next = now - (f now / f' now)

mapPixelToCoord :: Int -> Int -> Int -> Int -> Double -> (Double, Double) -> (Double, Double)
mapPixelToCoord x y w h scale (lax, lay) = (px, py)
                        where normalizer v m = (fromIntegral v - 0.5 * fromIntegral m) / fromIntegral m
                              px = scale * normalizer x w + lax
                              py = scale * normalizer y h - lay

getNearestRootsColor :: (Complex Double) -> [(Complex Double, Pic.PixelRGB8)] -> Pic.PixelRGB8
getNearestRootsColor aps colorMap = snd $ foldl1' findNearest colorMap
                                where findNearest (r1, c1) (r2, c2) = if magnitude (r1 - aps) < magnitude (r2 - aps) then (r1, c1) else (r2, c2)

generateImg :: Int -> Int -> Double -> (Double, Double) -> Int -> Pic.DynamicImage
generateImg w h scale lookAt iteration = Pic.ImageRGB8 (Pic.generateImage generator w h)
                    where generator x y = getNearestRootsColor aps kColorMap
                                        where 
                                        (px, py) = mapPixelToCoord x y w h scale lookAt
                                        aps      = newton kf kf' (px :+ py) iteration

main :: IO ()
main = do
  let path      = "output.png"
      width     = 1920
      height    = 1080
      scale     = 0.5
      lookAt    = (0.1, 0.1) 
      iteration = 50
  putStrLn $ "start processing..."
  Pic.savePngImage path $ generateImg width height scale lookAt iteration  
  putStrLn "finished"
