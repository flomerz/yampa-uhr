{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           FRP.Yampa

import           Graphics
import           Input
import           Shapes
import Data.Time
import Data.Fixed
import Types

-- < Constants > ---------------------------------------------------------------

greenColour    = sRGB24 0x1A 0xAF 0x5D
skyColour     = sRGB24 0xAD 0xD4 0xF4
brownColour  = sRGB24 0xCE 0xB1 0x71
blackColour   = sRGB24 0x00 0x00 0x00
redColour     = sRGB24 0xFF 0x00 0x00

winHeight, winWidth :: Double
winHeight = 600
winWidth  = 600

uhrX = 300
uhrY = 300

uhrSyncSpeedMultiplyer = 10

-- < Game State > --------------------------------------------------------------

data Uhr = Uhr { hourZeiger :: Zeiger
               , minuteZeiger :: Zeiger
               , secondZeiger :: Zeiger
               , running :: Bool
               } deriving (Show, Eq)

data Zeiger = Zeiger { posX :: Double
                     , posY :: Double
                     , length_ :: Int
                     , angle :: Double
                     } deriving (Show, Eq)

initHourZeiger hour = Zeiger 300 300 160 (timeToAngle hour hourAngleStep)
initMinZeiger minute = Zeiger 300 300 200 (timeToAngle minute minuteAngleStep)
initSecZeiger second = Zeiger 300 300 240 (timeToAngle second secondAngleStep)

initUhr = initUhrWithClockTime (ClockTime 0 0 0)
initUhrWithClockTime (ClockTime hour minute second) = Uhr (initHourZeiger hour) (initMinZeiger minute) (initSecZeiger second) False

hourAngleStep = pi / 6
minuteAngleStep = pi / 30
secondAngleStep = pi / 30

hourZeigerSpeedPerSec = -pi / 21600
minuteZeigerSpeedPerSec = -pi / 1800
secondZeigerSpeedPerSec = -pi / 30

-- < Helper > ------------------------------------------------------------------

timeToAngle :: Double -> Double -> Double
timeToAngle second step = angleClockOffset $ second * step

angleToTime :: Double -> Double -> Double
angleToTime angle step = (angleClockOffset angle) / step

angleClockOffset angle = -angle + pi

f :: Int -> Int -> Int
f x y = x + y

sadf = sin 21


-- < Game logic > --------------------------------------------------------------

uhr :: SF AppInput Uhr
uhr = uhrRun

-- uhr = do 
--   putStrLn "iuuu"
--   return (switch (uhrRun (Uhr (initSecZeiger 0) (initMinZeiger 0) (initHourZeiger 0) True)) (\_ -> (uhrRun_ (Uhr (initSecZeiger 0) (initMinZeiger minute) (initHourZeiger hour) True))))
--   --   where uhrSF <- uhr

uhrStop :: Uhr -> SF AppInput Uhr
uhrStop uhr = switch sf (\_ -> uhrSync uhr uhrRun Nothing)
        where sf = proc input -> do
                stopTriggered <- stopTrigger -< input
                returnA -< (uhr, stopTriggered)

uhrRun :: SF AppInput Uhr
uhrRun =  switch sf (\(currentUhr, targetClockTime) -> uhrStop currentUhr)
                        where 
                          sf :: SF AppInput (Uhr, Event (Uhr, ClockTime))
                          sf = proc input -> do
                                  currentUhr <- currentUhrSF -< input
                                  stopTriggered <- stopTrigger -< input
                                  returnA -< (currentUhr, stopTriggered `tag` (currentUhr, initClockTime))



uhrSync :: Uhr -> (SF AppInput Uhr) -> Maybe ClockTime -> SF AppInput Uhr
uhrSync (Uhr sourceHourZ sourceMinuteZ sourceSecondZ _) targetSF _ = switch sf (\_ -> targetSF)
        where sf = proc input -> do
                hourZeiger <- moveZeiger sourceHourZ (uhrSyncSpeedMultiplyer * hourZeigerSpeedPerSec) -< ()
                minZeiger <- moveZeiger sourceMinuteZ (uhrSyncSpeedMultiplyer * minuteZeigerSpeedPerSec) -< ()
                secZeiger <- moveZeiger sourceSecondZ (uhrSyncSpeedMultiplyer * secondZeigerSpeedPerSec) -< ()
                let nextUhr = (Uhr hourZeiger minZeiger secZeiger True)
                clockTime <- clockTimeInput -< input
                targetReached <- edge -< targetUhrReached nextUhr clockTime
                returnA -< (nextUhr, targetReached)
                where targetUhrReached :: Uhr -> ClockTime -> Bool
                      targetUhrReached currentUhr targetClockTime = and [ currentHour >= targetHour
                                                                        , currentMinute >= targetMinute
                                                                        , currentSecond >= targetSecond
                                                                        ]
                              where currentHour = (angleToTime (angle $ hourZeiger currentUhr) hourAngleStep) `mod'` 12
                                    currentMinute = (angleToTime (angle $ minuteZeiger currentUhr) minuteAngleStep) `mod'` 60
                                    currentSecond = (angleToTime (angle $ secondZeiger currentUhr) secondAngleStep) `mod'` 60
                                    targetHour = (clockHour targetClockTime) `mod'` 12
                                    targetMinute = (clockMinute targetClockTime) `mod'` 60
                                    targetSecond = (clockSecond targetClockTime) `mod'` 60

moveZeiger :: Zeiger -> Double -> SF a Zeiger
moveZeiger (Zeiger posX posY length_ angle0) angleMove = proc _ -> do
    angle <- imIntegral angle0 -< angleMove
    returnA -< (Zeiger posX posY length_ angle)

currentUhrSF :: SF AppInput Uhr
currentUhrSF = proc input -> do
        clockTime <- clockTimeInput -< input
        returnA -< initUhrWithClockTime clockTime


-- < Rendering > ---------------------------------------------------------------

class Renderer a where
  render :: a -> Object

instance Renderer Uhr where
  render (Uhr hZ mZ sZ _) = scene_ (
          map (\(Zeiger posX posY length_ angle) -> line_ length_ angle & pos_ (posX, posY) & colour_ greenColour) zeigers
          ++ [circle_ 250 & pos_ (uhrX, uhrY) & colour_ brownColour,
              line_ 100 0 & pos_ (uhrX, uhrY+150) & colour_ blackColour,
              line_ 100 0 & pos_ (uhrX, uhrY-250) & colour_ blackColour,
              line_ 100 (pi/2) & pos_ (uhrX+150, uhrY) & colour_ blackColour,
              line_ 100 (pi/2) & pos_ (uhrX-250, uhrY) & colour_ blackColour,
              line_ 50 (pi/3) & pos_ ((sinPos uhrX (pi/3) 200), (cosPos uhrX (pi/3) 200)) & colour_ blackColour,
              line_ 50 (pi/6) & pos_ ((sinPos uhrX (pi/6) 200), (cosPos uhrX (pi/6) 200)) & colour_ blackColour,
              line_ 50 (2/3*pi) & pos_ ((sinPos uhrX (2/3*pi) 200), (cosPos uhrY (2/3*pi) 200)) & colour_ blackColour,
              line_ 50 (5/6*pi) & pos_ ((sinPos uhrX (5/6*pi) 200), (cosPos uhrY (5/6*pi) 200)) & colour_ blackColour,
              line_ 50 (7/6*pi) & pos_ ((sinPos uhrX (7/6*pi) 200), (cosPos uhrY (7/6*pi) 200)) & colour_ blackColour,
              line_ 50 (4/3*pi) & pos_ ((sinPos uhrX (4/3*pi) 200), (cosPos uhrY (4/3*pi) 200)) & colour_ blackColour,
              line_ 50 (5/3*pi) & pos_ ((sinPos uhrX (5/3*pi) 200), (cosPos uhrY (5/3*pi) 200)) & colour_ blackColour,
              line_ 50 (11/6*pi) & pos_ ((sinPos uhrX (11/6*pi) 200), (cosPos uhrY (11/6*pi) 200)) & colour_ blackColour
             ] 
          ++ minuteLinie uhrX uhrY)
      & colour_ skyColour
      where zeigers = [hZ, mZ, sZ]

sinPos :: Double -> Double -> Double -> Double

sinPos pos angle length_ = pos + (sin angle * length_)

cosPos :: Double -> Double -> Double -> Double
cosPos pos angle length_ = pos + (cos angle * length_)

minuteLinie posX posY = map (\angle -> line_ 25 angle & pos_ ((sinPos posX angle 225), (cosPos posX angle 225)) & colour_ redColour) minuteWinkel

minuteWinkel = map (*pi) [ x | y <- take 12 [1/30,6/30..], x <- take 4 [y,y+(1/30)..]]


-- < Input handling > ----------------------------------------------------------

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent


stopTrigger :: SF AppInput (Event ())
stopTrigger = proc input -> do
    mouseTap    <- lbp -< input
    spacebarTap <- keyPressed ScancodeSpace -< input
    returnA -< mouseTap `lMerge` spacebarTap


-- < Main Function > -----------------------------------------------------------

main :: IO ()
main = startYampa

startYampa :: IO ()
startYampa = animate "IU Clock" (round winWidth) (round winHeight)
                      (parseWinInput >>> ((uhr >>^ render) &&& handleExit))



fa :: Int -> [(Int, Int)]
fa n = [ (x, y) | x <- [1..n], y <- [1..n], y * x == n ]

