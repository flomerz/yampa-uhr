module Types where

import FRP.Yampa

import qualified SDL


data ClockTime = ClockTime	{ clockHour :: Double
							, clockMinute :: Double
							, clockSecond :: Double
							} deriving (Show, Eq)

initClockTime :: ClockTime
initClockTime = (ClockTime 0 0 0)

type WinInput = Event WinInputEvent

type WinInputEvent = (Maybe SDL.EventPayload, ClockTime)