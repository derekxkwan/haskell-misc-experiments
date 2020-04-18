{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE PatternGuards #-}

module Tinkerbell where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate


-- Main -----------------------------------------------------------------------
bgnd :: Color
bgnd = black

fps :: Int
fps = 240

histLen :: Int
histLen = 5000

initHistory :: TinkerArray
initHistory = [initState]

curDim :: (Int, Int)
curDim = (800,800)

-- bounds are from (-bd, bd) for both coords 
tinkerbellBds :: (Float, Float)
tinkerbellBds = (2.0, 2.0)

type TinkerArray = [TinkerbellMap]

data TinkerbellMap = TinkMap
  { xn :: Float
  , yn :: Float
  , aVal :: Float
  , bVal :: Float
  , cVal :: Float
  , dVal :: Float
  } deriving (Show)

initState :: TinkerbellMap
initState = TinkMap
  { xn = (-0.72)
  , yn = (-0.64)
  , aVal = 0.9
  , bVal = (-0.6013)
  , cVal = 2.0
  , dVal = 0.5
  }

coordToTranslate :: (Float, Float) -> (Float, Float)
coordToTranslate (x,y) = ( xNorm * halfW, yNorm * halfH)
  where
    xNorm = x / (fst tinkerbellBds)
    yNorm = y / (snd tinkerbellBds)
    halfW = (fromIntegral $ fst curDim) / 2
    halfH = (fromIntegral $ snd curDim) / 2
  
main :: IO ()
main = simulate window bgnd fps initHistory render updateFn

window :: Display
window = InWindow "Tinkerbell" curDim (50, 50)

pointShape :: Int -> Float -> Float -> Picture
pointShape curSize posXn posYn
  = let cs = fromIntegral curSize
        (posX,posY) = coordToTranslate (posXn, posYn)
        x1 = posX
        x2 = posX + cs
        y1 = posY
        y2 = posY + cs
    in Polygon [(x1,y1), (x1,y2), (x2, y2), (x2, y1)]

drawPoint :: Int -> Float ->Float -> Float -> Picture
drawPoint pointSize curShade posX posY = Color (greyN curShade) (pointShape pointSize posX posY)
                                      

updateState :: TinkerbellMap -> TinkerbellMap
updateState prevstate = prevstate { xn = xn', yn = yn'}
  where
    xprev = xn prevstate
    yprev = yn prevstate
    xn' = (xprev ** 2.0) - (yprev ** 2.0) + ((aVal prevstate) * xprev) + ((bVal prevstate) * yprev)
    yn' = (2 * xprev * yprev) + ((cVal prevstate) * xprev) + ((dVal prevstate) * yprev)

-- ignore viewport, float is seconds since start
updateFn :: ViewPort -> Float -> TinkerArray -> TinkerArray
updateFn _ _ = updateHistory

-- keep adding new iterations to beginning of array
updateHistory :: TinkerArray -> TinkerArray
updateHistory curhist =
  let curstate = head curhist
      newstate = updateState curstate
  in [newstate] ++ (take (histLen -1) curhist)

renderPoint :: (TinkerbellMap,Int) -> Picture
renderPoint (curmap,curNum) =
  let ptSize = 5 :: Int
      curShade = (1.0 / (fromIntegral curNum))**0.25
      xcur = xn curmap
      ycur = yn curmap
  in drawPoint ptSize curShade xcur ycur
  
render :: TinkerArray -> Picture
render curhist =
  let histWithNum = zipWith (,) curhist [1..histLen]
  in pictures $ fmap renderPoint histWithNum
      
  
