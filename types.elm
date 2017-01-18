module ElmFinalGame.Types exposing (..)
import Time exposing (..)

type alias Model = 
  {x : Int, y : Int, key : ButtonState, vel : Int
  , bullets : List(BUpdater), lastTime : Maybe Time}

type Msg
  = Key ButtonState | Tick Time

type ButtonState
  = Left | Right | Up | Down | UpRight | UpLeft | DownRight | DownLeft | Shoot | None

type BUpdater = BUpdater (Float, Float) (Float -> BUpdater)