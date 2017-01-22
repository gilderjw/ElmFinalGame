module Update exposing (..)

import Types exposing (..)

--BULLET SPAWN

spawnStraightBullet : Model -> Float -> Model
spawnStraightBullet model speed =
  {model| bullets = List.append model.bullets [straightBulletUpdate model.x (model.y-(model.height/2)) speed 0]}

spawnSineBullet : Model -> Float -> Model
spawnSineBullet model speed = 
  {model| bullets = List.append model.bullets [sineBulletUpdate model.x 
                                                                (model.y-(model.height/2)) 
                                                                model.x 
                                                                (model.y-(model.height/2)) 
                                                                speed 
                                                                0]}

spawnTripleBullet : Model -> Float -> Float -> Model
spawnTripleBullet model xspeed yspeed =
  {model| bullets = List.append 
                    model.bullets
                    [straightBulletUpdate model.x (model.y-(model.height/2)) yspeed 0
                    ,angleBulletUpdate model.x (model.y-(model.height/2)) xspeed yspeed 0
                    ,angleBulletUpdate model.x (model.y-(model.height/2)) -xspeed yspeed 0]
  }

--BULLET UPDATES

sineBulletUpdate : Float -> Float -> Float -> Float -> Float -> Float -> BUpdater
sineBulletUpdate x y initialX initialY speed delta =
    let newY = y + (delta/60 * speed)
        deltaY = newY - initialY
        newX = initialX + 15*sin (deltaY/20)
    in BUpdater (newX, newY) (sineBulletUpdate newX newY initialX initialY speed)

straightBulletUpdate : Float -> Float -> Float -> Float -> BUpdater
straightBulletUpdate x y speed delta =
    let newY = y + ((speed * delta) / 60 )
        newX = x
    in BUpdater (newX, newY) (straightBulletUpdate newX newY speed)

angleBulletUpdate : Float -> Float -> Float -> Float -> Float -> BUpdater
angleBulletUpdate  x y xspeed yspeed delta =
  let newY = y + ((yspeed * delta) / 60 )
      newX = x + ((xspeed * delta) / 60 )
  in BUpdater (newX, newY) (angleBulletUpdate newX newY xspeed yspeed)

--ENEMY UPDATES
stillEnemyUpdate : Float -> Float -> Float -> Float -> Float -> EnemyUpdater
stillEnemyUpdate x y width height delta =
  EnemyUpdater (x, y, width, height) (stillEnemyUpdate x y width height)

--CLEANUP STUFF
bulletIsOnScreen : BUpdater -> Bool
bulletIsOnScreen updater =
  case updater of 
    BUpdater (x, y) _ ->
      x <= 500 && x >= 0 && y <= 500 && y >= 0


--HIT DETECTION
hit : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Bool
hit x1 y1 width1 height1 x2 y2 width2 height2 =
  (abs (x1 - x2)) < (width1/2 + width2/2) &&
  (abs (y1 - y2)) < (height1/2 + height2/2)

isEnemyHitByBullet : EnemyUpdater -> BUpdater -> Bool
isEnemyHitByBullet enemy bullet =
  case enemy of
    EnemyUpdater (x, y, width, height) _ -> 
      (case bullet of
        BUpdater (bx, by) _ ->
          (hit x y width height bx by 5 5))

isPlayerHitByEnemy : Model -> EnemyUpdater -> Bool
isPlayerHitByEnemy model enemy =
  case enemy of
    EnemyUpdater (x, y, width, height) _ ->
      (hit x y width height model.x model.y model.width model.height)

isPlayerHitByBullet : Model -> BUpdater -> Bool
isPlayerHitByBullet model bullet = 
  case bullet of
    BUpdater (x, y) _ -> 
      (hit x y 5 5 model.x model.y model.width model.height)


dealWithPlayerCollision : Model -> Model
dealWithPlayerCollision model =
  if ((List.foldr --Checking enemy collision
        (\enemy acc -> acc || 
                      (isPlayerHitByEnemy model enemy)) 
        False 
        model.enemies) || 
      --Check for bullet collision
       (List.foldr 
        (\bullet acc -> acc || 
                        (isPlayerHitByBullet model bullet))
        False
        model.bullets)) then 
    {model|x = 9999999} -- nice and elegent solution for killing a player
  else
    model

enemyIsAlive : Model -> EnemyUpdater ->  Bool
enemyIsAlive model updater =
  case updater of 
    EnemyUpdater (x, y, width, height) _ ->
      (x <= 500 && x >= 0 && y <= 500 && y >= 0) --out of bounds
      && (not (List.foldr 
              (\bullet acc -> (acc || (isEnemyHitByBullet updater bullet))) 
              False 
              model.bullets))

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
    Shoot -> (spawnTripleBullet (updateControls model model.key) 5 -20)
    _ -> {model | key = keyCode}

updateBullet : Float -> BUpdater -> BUpdater
updateBullet delta updater =
  case updater of
    BUpdater _ func -> func delta

updateEnemy : Float -> EnemyUpdater -> EnemyUpdater
updateEnemy delta updater =
  case updater of
    EnemyUpdater _ func -> func delta

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Key q -> (updateControls model q, Cmd.none)
        Tick newTime -> 
          case model.lastTime of
            Just oldTime -> 
              let newBullets = List.map (updateBullet (newTime - oldTime)) model.bullets 
                  newEnemies = List.map (updateEnemy (newTime - oldTime)) model.enemies
              in
              (dealWithPlayerCollision {model | lastTime=Just newTime
                      , bullets=List.filter bulletIsOnScreen newBullets
                      , enemies= List.filter (enemyIsAlive model) newEnemies}, Cmd.none)
            Nothing -> ({model | lastTime = Just newTime}, Cmd.none)
      



