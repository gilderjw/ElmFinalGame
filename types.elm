module ElmFinalGame.Types exposing (..)

type alias Model = {x : Int, y : Int, key : ButtonState, vel : Int}

type Msg
  = Key ButtonState

type ButtonState
  = Left | Right | Up | Down | UpRight | UpLeft | DownRight | DownLeft | None

-- 68 is a
-- 65 is d
-- 87 is w
-- 83 is s