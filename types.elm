module ElmFinalGame.Types exposing (..)
import Time exposing (..)

type alias Model = 
  {x : Float, y : Float, key : ButtonState, vel : Float
  , bullets : List(BUpdater), lastTime : Maybe Time
  , width : Float, height : Float, enemies : List(EnemyUpdater)}

type Msg
  = Key ButtonState | Tick Time

type ButtonState
  = Left | Right | Up | Down | UpRight | UpLeft | DownRight | DownLeft | Shoot | None

type BUpdater = BUpdater (Float, Float) (Float -> BUpdater)

type EnemyUpdater = EnemyUpdater (Float, Float) (Float -> EnemyUpdater)