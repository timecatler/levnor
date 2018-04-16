module Lib
    ( someFunc
    ) where

import           GHC.Float
import           System.Environment
import           Codec.Picture
import           Codec.Picture.Types
import           Statistics.Distribution        (quantile)
import           Statistics.Distribution.Normal
import           Flow

-- Вытаскивает пиксели
pixels2List :: (Pixel a) => Image a -> [a]
pixels2List = pixelFold (\pxs _ _ px -> px : pxs) []

extractRawLuminance :: (LumaPlaneExtractable a) => Image a -> [PixelBaseComponent a]
extractRawLuminance = pixels2List . extractLumaPlane
-- extractRawLuminance (ImageRGB8 img) = img |> extractPixels |> map demotePixel

histForList xs = map (\x -> length $ filter (== x) xs) [0 .. 255]

normalizeList xs = map (/ sum xs) xs

computePdfLUT xs = xs |> histForList |> map fromIntegral |> normalizeList

computeCdfLUT ys = ys |> computePdfLUT |> summate
  where
    summate (x : xs) = x : summate' x xs
      where
        summate' x (x' : xs) | x >= 1.0    = 1.0 : summate' 1.0 xs
                             | otherwise = (x + x') : summate' (x + x') xs
        summate' x []        = [1.0]

getTransformationCoeff :: (Int -> Double) -> Int -> Double
getTransformationCoeff transformation x | x == 0    = (transformation x)
                                        | otherwise = (transformation x) / (fromIntegral x)

-- genTransformation :: Double -> Double -> [PixelF] -> (PixelF -> PixelF)
genTransformation m s rawLum = inverseSmirnov . directSmirnov
  where
    directSmirnov  = (!!) (computeCdfLUT rawLum)
    -- Костыль, чтобы компенсировать погрешность вычислений с плавающей точкой
    inverseSmirnov x | x >= 1.0  = quantile (normalDistr m s) 1.0
                     | otherwise = quantile (normalDistr m s) x

normalizeGrayscale :: Double -> Double -> Image Pixel8 -> Image Pixel8
normalizeGrayscale m s img = pixelMap (correct . genTransformation m s (extractRawLuminance img) . fromIntegral) img
  where
    correct x | x < 0     = 0
              | x > 255   = 255
              | otherwise = round x

normalizeColored :: Double -> Double -> Image PixelRGB8 -> Image PixelRGB8
normalizeColored m s img = pixelMapXY (transformWithAt (genTransformation m s (extractRawLuminance img))) img
  where
    transformWithAt transformation x y = transformWith (getTransformationCoeff transformation $ fromIntegral $ pixelAt srcLum x y)
    srcLum = extractLumaPlane img
    transformWith coef (PixelRGB8 r g b) = PixelRGB8
      (correct (fromIntegral r * coef))
      (correct (fromIntegral g * coef))
      (correct (fromIntegral b * coef))
    correct lvl | lvl < 0   = 0
                | lvl > 255 = 255
                | otherwise = round lvl

processImage res m s img = writeBitmap res $ (normalizeColored m s . convertRGB8) img
-- transformImage 128 32 (ImageY8 $ )

someFunc :: IO ()
someFunc = do
    src:res:m:s:_ <- getArgs
    readImage src >>= either putStrLn (processImage res (read m) (read s))

