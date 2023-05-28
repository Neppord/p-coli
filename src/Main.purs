module Main where

import Prelude

import Data.Array (catMaybes, replicate)
import Data.Either (Either(Left, Right), blush, hush)
import Data.Foldable (for_)
import Data.Maybe (Maybe, fromJust)
import Data.Number (cos, floor, pi, sin, (%))
import Data.Semiring ((+))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Random (randomInt, randomRange)
import Effect.Ref (new, read, write)
import Graphics.Canvas (Context2D, clearRect, fillRect, getCanvasElementById, getContext2D, setFillStyle)
import Partial.Unsafe (unsafePartial)
import Web.HTML (window)
import Web.HTML.Window (requestAnimationFrame)

type Movable a =
  { pos :: { x :: Number, y :: Number }
  , dir :: { x :: Number, y :: Number }
  | a
  }

type Living a = { life :: Int | a }
type Coli =
  { pos :: { x :: Number, y :: Number }
  , dir :: { x :: Number, y :: Number }
  , life :: Int
  }

type Food = { x :: Number, y :: Number }

world :: { x :: Number, y :: Number }
world = { x: 100.0, y: 100.0 }

unsafeJust :: forall a. Maybe a -> a
unsafeJust a = unsafePartial (fromJust a)

random_dir :: Effect { x :: Number, y :: Number }
random_dir = do
  dir <- randomRange (-pi) pi
  pure { x: cos dir, y: sin dir }

random_coli :: Effect Coli
random_coli = do
  pos <- random_pos
  dir <- random_dir
  life <- randomInt 100 1500
  pure { pos, dir, life }

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
  colis_ref <- new init_coli
  foods_ref <- new init_food
  animate do
    clearRect ctx { x: 0.0, y: 0.0, width: world.x, height: world.y }

    colis <- read colis_ref
    foods <- read foods_ref

    let
      decayed_colis = colis <#> move >>> age >>> coli_to_food
      updated_colis = decayed_colis <#> blush # catMaybes

    colis_ref # write updated_colis
    foods_ref # write (foods <> (decayed_colis <#> hush # catMaybes))
    
    for_ updated_colis \coli -> do
      displayColi ctx coli
    
    read_foods <- read foods_ref
    for_ read_foods \food -> do
      displayFood ctx food

move :: forall a. Movable a -> Movable a
move a = a
  { pos =
      { x: (a.pos.x + a.dir.x + world.x) % world.x
      , y: (a.pos.y + a.dir.y + world.y) % world.y
      }
  }

age :: forall a. Living a -> Living a
age a = a { life = a.life - 1 }

coli_to_food :: Coli -> Either Coli Food
coli_to_food coli =
  if coli.life <= 0 then Right coli.pos
  else Left coli

getCtx :: Effect (Maybe Context2D)
getCtx = do
  canvas <- getCanvasElementById "world"
  canvas
    <#> getContext2D
    # sequence

fillPixel :: Context2D -> { x :: Number, y :: Number } -> Effect Unit
fillPixel ctx { x, y } =
  fillRect ctx { x: floor x, y: floor y, width: 1.0, height: 1.0 }

displayColi :: Context2D -> Coli -> Effect Unit
displayColi ctx coli = do
  setFillStyle ctx "green"
  fillPixel ctx coli.pos

displayFood :: Context2D -> { x :: Number, y :: Number } -> Effect Unit
displayFood ctx food = do
  setFillStyle ctx "red"
  fillPixel ctx food
