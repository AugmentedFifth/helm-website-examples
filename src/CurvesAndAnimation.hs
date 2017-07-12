{-# LANGUAGE RecordWildCards #-}

module CurvesAndAnimation
  ( main
  ) where

import           Linear.V2       (V2 (V2))

import           Helm
import qualified Helm.Cmd        as Cmd
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Engine.SDL as SDL
import           Helm.Graphics2D
import qualified Helm.Time       as Time


data Action = Animate Double

data Model = Model { angleOffset :: Double }


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

slice :: Double -> Int -> [Form e]
slice offset n =
  map (filled color) [polygon (path points), cap]
  where
    color = colors !! n
    increment = 2 * pi / realToFrac (length colors)
    t1 = offset + increment * realToFrac n
    t2 = t1 + increment
    r = 150

    points =
      [ V2 0 0
      , pointOnCircum r t1
      , pointOnCircum r t2
      ]

    cap = ArcShape (V2 0 0) t1 t2 r (V2 1 1)

initial :: (Model, Cmd SDLEngine Action)
initial = (Model { angleOffset = 0 }, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update Model{..} (Animate dt) =
  ( Model { angleOffset = dt * 2e-3 + angleOffset }
  , Cmd.none
  )

subscriptions :: Sub SDLEngine Action
subscriptions = Time.fps 60 Animate

view :: Model -> Graphics SDLEngine
view Model{..} = Graphics2D
  $ center windowCenter
  $ collage
  $ concat [slice angleOffset n | n <- [0..length colors - 1]]

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
