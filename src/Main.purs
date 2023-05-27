module Main where

import Prelude

import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Semiring ((+))
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas (Context2D, clearRect, fillRect, getCanvasElementById, getContext2D, setFillStyle)
import Data.Traversable (sequence)
import Record (disjointUnion)
import Web.HTML.HTMLEmbedElement (height, width)
import Effect.Timer (setInterval)
import Effect.Ref (modify, new, read)
import Data.Foldable (for_)
import Partial.Unsafe (unsafePartial)
import Web.HTML (window)
import Data.Number (cos, floor, pi, sin, (%))
import Effect.Random (randomRange)
import Data.Array (replicate)
import Web.HTML.Window (requestAnimationFrame)

type Movable a = { pos :: { x :: Number, y :: Number }, dir :: { x :: Number, y :: Number } | a }

world :: { x :: Number, y :: Number }
world = { x: 100.0, y: 100.0 }

unsafeJust :: forall a. Maybe a -> a
unsafeJust a = unsafePartial (fromJust a)

random_dir :: Effect { x :: Number, y :: Number }
random_dir = do
  dir <- randomRange (-pi) pi
  pure { x: cos dir, y: sin dir }

random_coli :: Effect (Movable ())
random_coli = do
  pos <- random_pos 
  dir <- random_dir
  pure { pos, dir}

random_pos :: Effect { x :: Number, y :: Number }
random_pos = do
  x <- randomRange 0.0 world.x
  y <- randomRange 0.0 world.y
  pure { x, y }
  
random_food :: Effect { x :: Number, y :: Number }
random_food = random_pos

animate :: Effect Unit -> Effect Unit
animate f = do
  w <- window
  let
    loop = do
      f
      void $ requestAnimationFrame loop w
  loop

main :: Effect Unit
main = do
  ctx <- getCtx <#> unsafeJust
  init_coli <- sequence $ replicate 500 random_coli
  init_food <- sequence $ replicate 10 random_food
  colis <- new init_coli
  foods <- new init_food
  animate do
    clearRect ctx { x: 0.0, y: 0.0, width: world.x, height: world.y }
    updated_colis <- modify (map tick) colis
    for_ updated_colis \coli -> do
      displayColi ctx coli
    read_foods <- read foods
    for_ read_foods \food -> do
      displayFood ctx food

tick :: forall a. Movable a -> Movable a
tick a = a
  { pos =
      { x: (a.pos.x + a.dir.x + world.x) % world.x
      , y: (a.pos.y + a.dir.y + world.y) % world.y
      }
  }

getCtx :: Effect (Maybe Context2D)
getCtx = do
  canvas <- getCanvasElementById "world"
  canvas
    <#> getContext2D
    # sequence

fillPixel :: Context2D -> { x :: Number, y :: Number } -> Effect Unit
fillPixel ctx { x, y } =
  fillRect ctx { x: floor x, y: floor y, width: 1.0, height: 1.0 }

displayColi :: forall a. Context2D -> { pos :: { x :: Number, y :: Number } | a } -> Effect Unit
displayColi ctx coli = do
  setFillStyle ctx "green"
  fillPixel ctx coli.pos

displayFood :: Context2D -> { x :: Number, y :: Number } -> Effect Unit
displayFood ctx food = do
  setFillStyle ctx "red"
  fillPixel ctx food
