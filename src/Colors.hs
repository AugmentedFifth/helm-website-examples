module Colors
  ( main
  ) where

import           Linear.V2       (V2 (V2))

import           Helm
import qualified Helm.Cmd        as Cmd
import           Helm.Color
import qualified Helm.Engine.SDL as SDL
import           Helm.Graphics2D
import qualified Helm.Sub        as Sub


data Action = DoNothing

data Model = Model


windowDims :: V2 Int
windowDims = V2 800 600

windowCenter :: V2 Double
windowCenter = (fromIntegral.(`div` 2)) <$> windowDims

red :: Color
red = rgb 1 0 0

lime :: Color
lime = rgb 0 1 0

blue :: Color
blue = rgb 0 0 1

yellow :: Color
yellow = rgb 1 1 0

cyan :: Color
cyan = rgb 0 1 1

magenta :: Color
magenta = rgb 1 0 1

maroon :: Color
maroon = rgb 0.5 0 0

navy :: Color
navy = rgb 0 0 0.5

green :: Color
green = rgb 0 0.5 0

teal :: Color
teal = rgb 0 0.5 0.5

purple :: Color
purple = rgb 0.5 0 0.5

colors :: [Color]
colors =
  [ red
  , lime
  , blue
  , yellow
  , cyan
  , magenta
  , maroon
  , navy
  , green
  , teal
  , purple
  ]

pointOnCircum :: Double -> Double -> V2 Double
pointOnCircum r theta = V2 (r * cos theta) (r * sin theta)

slice :: Int -> Form e
slice n = filled color $ polygon (path points)
  where
    color = colors !! n
    increment = 2 * pi / realToFrac (length colors)
    t1 = increment * realToFrac n
    t2 = t1 + increment
    r = 150
    points = [V2 0 0, pointOnCircum r t1, pointOnCircum r t2]

initial :: (Model, Cmd SDL.SDLEngine Action)
initial = (Model, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDL.SDLEngine Action)
update Model DoNothing = (Model, Cmd.none)

subscriptions :: Sub SDL.SDLEngine Action
subscriptions = Sub.none

view :: Model -> Graphics SDL.SDLEngine
view Model = Graphics2D
  $ center windowCenter
  $ collage [slice n | n <- [0..length colors - 1]]

main :: IO ()
main = do
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowDimensions  = windowDims
    , SDL.windowIsResizable = False
    }

  run engine GameConfig
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
