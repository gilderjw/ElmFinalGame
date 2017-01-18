module ElmFinalGame.Update exposing (..)

import ElmFinalGame.Types exposing (..)

keyToVel : ButtonState -> Int
keyToVel key =
    case key of
        Left -> -5
        Right -> 5
        _ -> 0

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Key q -> ({model|key=q, x = model.x + keyToVel q}, Cmd.none)
      


