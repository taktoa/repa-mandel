module Main where

import Data.Complex
import Data.Word
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.BMP
import Data.Configurator

data Render =   Render {
                    xi :: Double,
                    xf :: Double,
                    yi :: Double,
                    yf :: Double,
                    res :: Int
                } deriving (Show)

type MTuple = ((Bool, Int), Complex Double, Complex Double)

type W8 = Word8

xr, yr :: Render -> Int
xr (Render i f _ _ r) = round ((f - i) * (fromIntegral r))
yr (Render _ _ i f r) = round ((f - i) * (fromIntegral r))

dx, dy :: Render -> Double
dx a = (xf a - xi a) / (fromIntegral (xr a))
dy a = (yf a - yi a) / (fromIntegral (yr a))

mxs, mys :: Render -> [Double]
mxs a = [(xi a), ((xi a) + (dx a)) .. (xf a)]
mys a = [(yi a), ((yi a) + (dy a)) .. (yf a)]

genGrid :: Render -> [Complex Double]
genGrid r = concatMap (\(a, b) -> map (\x -> a :+ x) b) pts
    where
    pts = zip (mxs r) (repeat (mys r))

genMTuples :: Render -> [MTuple]
genMTuples r = map (\z -> ((False, 0), 0 :+ 0, z)) (genGrid r)

checkBulb :: Complex Double -> Bool
checkBulb (x :+ y) = a || b
    where
    x' = x - 0.25
    y' = y**2.0
    q = x'**2.0 + y'
    a = 4 * q * (q + x') < y'
    b = 16 * ((x + 1)**2.0 + y') < 1

checkCircle :: Complex Double -> Bool
checkCircle (x :+ y) = (x*x) + (y*y) > 4

mandelCore :: MTuple -> MTuple
mandelCore x@((True, _), _, _) = x
mandelCore ((False, i), v, l) = (ni, nv, l)
    where
    nc = (nv == v) || (checkCircle nv)
    ni = (nc, i + 1)
    nv = (v*v) + l

bulbFilter :: Int -> MTuple -> MTuple
bulbFilter mi x@((False, i), v, l)
    | checkCircle l             = ((True, i), v, l)
    | checkBulb l               = ((True, mi), v, l)
    | otherwise                 = x
bulbFilter _ x = x

generate :: Render -> Int -> R.Array R.D R.DIM2 MTuple
generate r mi = (iterate (R.map mandelCore) bulbArr) !! mi
    where
    bulbArr = R.map (bulbFilter mi) baseArr
    baseArr = R.fromListUnboxed shp (genMTuples r)
    shp = R.Z R.:. ((xr r) + 1) R.:. ((yr r) + 1)

normIteration :: MTuple -> Double
normIteration ((_, i), v, _) = (fi - log d) / fi
    where
    d = logBase 2.0 (magnitude v)
    fi = (fromIntegral i) :: Double

render :: MTuple -> (W8, W8, W8)
render x = (a, a, a)
    where a = round (255 * normIteration x) :: W8

main :: IO ()
main = do
    cfg <- load "mandel.cfg"
    cxi <- lookupDefault (-2.0)     cfg "cxi"
    cxf <- lookupDefault 0.5        cfg "cxf"
    cyi <- lookupDefault 0.0        cfg "cyi"
    cyf <- lookupDefault 1.5        cfg "cyf"
    mi  <- lookupDefault 255        cfg "iterations"
    out <- lookupDefault "test.bmp" cfg "output"
    let arr = R.map render (generate (Render cxi cxf cyi cyf res) mi)
    fa <- R.computeP arr :: IO (R.Array R.U R.DIM2 (W8, W8, W8))
    Data.Array.Repa.IO.BMP.writeImageToBMP out fa
    putStrLn ("File finished: " ++ out)
