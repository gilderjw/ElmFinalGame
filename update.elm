module ElmFinalGame.Update exposing (..)

import ElmFinalGame.Types exposing (..)

updateControls: Model -> ButtonState -> Model
updateControls model keyCode =
  case keyCode of
    Right -> {model| x = model.x + model.vel, key = keyCode}
    Left -> {model| x = model.x - model.vel, key = keyCode}
    Up -> {model| y = model.y - model.vel, key = keyCode}
    Down -> {model| y = model.y + model.vel, key = keyCode}
    UpLeft -> {model| y = model.y - model.vel, x = model.x - model.vel, key = keyCode}
    UpRight -> {model| y = model.y - model.vel, x = model.x + model.vel, key = keyCode}
    DownLeft -> {model| y = model.y + model.vel, x = model.x - model.vel, key = keyCode}
    DownRight -> {model| y = model.y + model.vel, x = model.x + model.vel, key = keyCode}
    _ -> {model | key = keyCode}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Key q -> (updateControls model q, Cmd.none)
      


