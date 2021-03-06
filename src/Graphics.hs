module Graphics
    ( animate
    ) where

-- import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Colour.SRGB (toSRGB24, RGB(..))
import           Data.Text (Text, unpack)
import qualified Data.Vector.Storable as Vector
import           Data.StateVar (($=))

import           FRP.Yampa

import           Linear (V2(..), V4(..))
import           Linear.Affine (Point(..))

import Foreign.C.Types
import Data.Time

import qualified SDL
import qualified SDL.Raw.Types as SDL (Color(..))
import qualified Graphics.UI.SDL.TTF as Font

import SDLHelper

import Shapes
import Types

-- | (Object to render, should the app exit)
--   TODO: Datatype
type WinOutput = (Object, Bool)

animate :: Text                -- ^ window title
        -> Int                 -- ^ window width in pixels
        -> Int                 -- ^ window height in pixels
        -> SF WinInput WinOutput -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    SDL.initialize [SDL.InitVideo]
    Font.init
    window <- SDL.createWindow title windowConf
    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

    lastInteraction <- newMVar =<< SDL.time

    let 
        getCurrentClockTime :: IO ClockTime
        getCurrentClockTime = do
            sysTime <- getCurrentTime
            sysTimeZone <- getCurrentTimeZone
            let timeOfDay = localTimeOfDay $ utcToLocalTime sysTimeZone sysTime
            let clockSecond_ = fromRational $ toRational $ todSec timeOfDay
            let clockMinute_ = (fromIntegral (todMin timeOfDay)) + (clockSecond_ / 60)
            let clockHour_ = (fromIntegral (todHour timeOfDay)) + (clockMinute_ / 60)
            return (ClockTime clockHour_ clockMinute_ clockSecond_)

        senseInput :: Bool -> IO (DTime, Maybe WinInput)
        senseInput _canBlock = do
            currentTime <- SDL.time
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent
            clockTime <- getCurrentClockTime
            return (dt, Just . Event $ (SDL.eventPayload <$> mEvent, clockTime))

        renderOutput :: Bool -> WinOutput -> IO Bool
        renderOutput changed (obj, shouldExit) = do
            when changed $ do
                renderObject renderer winHeight obj
                SDL.present renderer
            return shouldExit

    reactimate (return NoEvent) senseInput renderOutput sf

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    Font.quit
    SDL.quit

    where windowConf =
              SDL.defaultWindow { SDL.windowInitialSize = V2 (fromIntegral winWidth)
                                                             (fromIntegral winHeight) }

renderObject :: SDL.Renderer -> Int -> Object -> IO ()
renderObject renderer winHeight obj = setRenderAttrs >> renderShape
    where setRenderAttrs = do
              SDL.rendererDrawColor renderer $= V4 r g b maxBound
          renderShape = case objShape obj of
              Rectangle x y -> (SDL.fillRect renderer $ Just $
                                     SDL.Rectangle (P (V2 (toEnum $ floor px)
                                                          (toEnum $ winHeight - floor py)))
                                                   (V2 (toEnum x) (toEnum y)))
              Line _ _ -> SDL.drawLine renderer (P (V2 (n px) (toEnum $ winHeight - floor py))) (P (lineTargetV2 obj winHeight))
              TextRectangle x y txt -> do
                  font <- Font.openFont "/usr/share/fonts/truetype/ubuntu-font-family/Ubuntu-B.ttf" 18
                  textSurface <- fmap pSurface $ Font.renderTextSolid font (unpack txt) (SDL.Color r g b 0)
                  textTexture <- SDL.createTextureFromSurface renderer textSurface
                  SDL.copy renderer textTexture Nothing (Just $ SDL.Rectangle (P (V2 (toEnum $ floor px)
                                                          (toEnum $ winHeight - floor py)))
                                                          (V2 (toEnum x) (toEnum y)))
                  Font.closeFont font
                  SDL.freeSurface textSurface
              Scene objs -> do
                  SDL.clear renderer
                  mapM_ (renderObject renderer winHeight) objs
              Circle r -> SDL.drawPoints renderer $ Vector.fromList $
                                map (\(x,y) -> P (V2 (toEnum x) (toEnum y))) $
                                translate (floor px, winHeight - floor py) $
                                rasterCircle  r
          (px, py) = objPos obj
          (RGB r g b) = toSRGB24 $ objColour obj

n = toEnum . floor

lineTargetV2 :: Object -> Int -> V2 CInt
lineTargetV2 obj winHeight = (V2 (toEnum$round (px+((sin angle)*(fromIntegral length_))))
                       (toEnum$winHeight - (round (py+((cos angle)*(fromIntegral length_))))))
  where (px, py) = objPos obj
        Line length_ angle = objShape obj

-- | Get octant points for a circle of given radius.
octant :: (Num a, Ord a) => a -> [(a, a)]
octant r = takeWhile inOctant . map fst $ iterate step ((r, 0), 1 - r)
    where -- check if we are still in octant
          inOctant (x, y) = x >= y

          -- go to the next point in the circle
          step ((x, y), e)
              | e < 0     = ((x,     y + 1), e + 2 * (y + 1) + 1)
              | otherwise = ((x - 1, y + 1), e + 2 * (y - x + 2) + 1)

-- | Get quadrant points for a circle of given radius.
-- To do that we just mirror octant with respect to x = y line.
quadrant :: (Num a, Ord a) => a -> [(a, a)]
quadrant r = octant r >>= mirror
    where mirror (x, y) = [ (x, y), (y, x) ]

-- | Get points of a circle of given radius.
-- To do that we just mirror quadrant with respect to x = 0 and y = 0 lines.
rasterCircle :: (Num a, Ord a) => a -> [(a, a)]
rasterCircle r = quadrant r >>= mirror
    where mirror (x, y) = [ (u, v) | u <- [x, -x], v <- [y, -y] ]

-- | Move all points by a given vector.
translate :: (Num a, Eq a) => (a, a) -> [(a, a)] -> [(a, a)]
translate v = map (v .+)

-- | Vector addition generalized for Num
(.+) :: Num a => (a, a) -> (a, a) -> (a, a)
(x, y) .+ (u, v) = (x + u, y + v)
