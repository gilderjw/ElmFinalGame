module ElmFinalGame.Types exposing (..)

type alias Model = {x : Int, y : Int, key : ButtonState}

type Msg
  = Key ButtonState

type ButtonState
  = Left | Right | None | Both

