module ElmFinalGame.Update exposing (..)

import ElmFinalGame.Types exposing (..)
--import Time exposing (..)

spawnStraightBullet : Model -> Float -> Model
spawnStraightBullet model speed =
  {model| bullets = List.append model.bullets [straightBulletUpdate model.x model.y speed 0]}

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
    Shoot -> (spawnStraightBullet (updateControls model model.key) -20)
    _ -> {model | key = keyCode}

straightBulletUpdate : Float -> Float -> Float -> Float -> BUpdater
straightBulletUpdate x y speed delta =
    let newY = y + ((speed * delta) / 60 )
        newX = x
    in BUpdater (newX, newY) (straightBulletUpdate newX newY speed)

isOnScreen : BUpdater -> Bool
isOnScreen updater =
  case updater of 
    BUpdater (x, y) _ ->
      x <= 500 && x >= 0 && y <= 500 && y >= 0

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Key q -> (updateControls model q, Cmd.none)
        Tick newTime -> 
          case model.lastTime of
            Just oldTime -> 
              let newBullets = List.map (updateBullet (newTime - oldTime)) model.bullets in
              ({model | lastTime=Just newTime
                      , bullets = List.filter isOnScreen newBullets }, Cmd.none)
            Nothing -> ({model | lastTime = Just newTime}, Cmd.none)
      


