module ElmFinalGame.Update exposing (..)

import ElmFinalGame.Types exposing (..)
--import Time exposing (..)

updateBullet : Float -> BUpdater -> BUpdater
updateBullet delta updater =
  case updater of
    BUpdater _ func -> func delta

updateControls : Model -> ButtonState -> Model
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
    Shoot -> (updateControls model model.key)
    _ -> {model | key = keyCode}

straightBulletUpdate : Float -> Float -> Float -> Float -> BUpdater
straightBulletUpdate x y speed delta =
    let newY = y + ((speed * delta) / 60 )
        newX = x
    in BUpdater (newX, newY) (straightBulletUpdate newX newY speed)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Key q -> (updateControls model q, Cmd.none)
        Tick newTime -> 
          case model.lastTime of
            Just oldTime -> 
              ({model | lastTime=Just newTime
                      , bullets = List.map (updateBullet (newTime - oldTime)) model.bullets}, Cmd.none)
            Nothing -> ({model | lastTime = Just newTime}, Cmd.none)
      


