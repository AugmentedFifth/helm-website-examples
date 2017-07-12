module Gradients
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

white :: Color
white = rgb 1 1 1

black :: Color
black = rgb 0 0 0

red :: Color
red = rgb 1 0 0

yellow :: Color
yellow = rgb 1 1 0

stopsA :: [(Double, Color)]
stopsA = [(0, black), (1, white)]

stopsB :: [(Double, Color)]
stopsB = [(0, black), (0.5, red), (1, yellow)]

linearGrad :: Gradient
linearGrad = linear (0, 0) (0, 100) stopsB

radialGrad :: Gradient
radialGrad = radial (0, 0) 0 (0, 0) 64 stopsA

gradientRect :: Form e
gradientRect = move (V2 100 100) (gradient linearGrad $ rect (V2 300 100))

gradientCircle :: Form e
gradientCircle = move (V2 500 300) (gradient radialGrad $ circle 64)

initial :: (Model, Cmd SDL.SDLEngine Action)
initial = (Model, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDL.SDLEngine Action)
update Model DoNothing = (Model, Cmd.none)

subscriptions :: Sub SDL.SDLEngine Action
subscriptions = Sub.none

view :: Model -> Graphics SDL.SDLEngine
view Model = Graphics2D $ collage [gradientRect, gradientCircle]

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
