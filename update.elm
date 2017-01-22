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

spawnHomingBullet : Model -> Float -> Model
spawnHomingBullet model speed =
  {model| bullets = List.append model.bullets [homingBulletUpdate
                                                                  model.x
                                                                  (model.y-(model.height/2) - 10)
                                                                  speed
                                                                  model
                                                                  0]}

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
angleBulletUpdate x y xspeed yspeed delta =
  let newY = y + ((yspeed * delta) / 60 )
      newX = x + ((xspeed * delta) / 60 )
  in BUpdater (newX, newY) (angleBulletUpdate newX newY xspeed yspeed)

homingBulletUpdate : Float -> Float -> Float -> Model -> Float -> BUpdater
homingBulletUpdate x y speed model delta =
  let (targetX, targetY) = (getClosestEnemy model x y (stillEnemyUpdate x -200 2 2 model 0)) in
  let yDist = (y - targetY)
      xDist = (x - targetX)
      realDist = sqrt(xDist^2 + yDist^2)
      newX = x + xDist / realDist * speed * delta /60
      newY = y + yDist / realDist * speed * delta /60

  in HUpdater (newX, newY) (homingBulletUpdate newX newY speed)


--ENEMY UPDATES
stillEnemyUpdate : Float -> Float -> Float -> Float -> Model -> Float -> EnemyUpdater
stillEnemyUpdate x y width height model delta =
  EnemyUpdater (x, y, width, height) (stillEnemyUpdate x y width height)


fallingEnemyUpdate : Float -> Float -> Float -> Float -> Float -> Model -> Float -> EnemyUpdater
fallingEnemyUpdate x y width height speed model delta =
    let newY = y + ((speed * delta) / 60 )
        newX = x
    in EnemyUpdater (newX, newY, width, height) (fallingEnemyUpdate newX newY width height speed)

sineEnemyUpdate : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Model -> Float -> EnemyUpdater
sineEnemyUpdate x y initialX initialY width height speed model delta =
  let newY = y + (delta/60 * speed)
      deltaY = newY - initialY
      newX = initialX + 80*sin (deltaY/20)
  in EnemyUpdater (newX, newY, width, height) (sineEnemyUpdate newX newY initialX initialY width height speed)

allahuakbarEnemyUpdate : Float -> Float -> Float -> Float -> Float -> Model -> Float -> EnemyUpdater
allahuakbarEnemyUpdate  x y width height speed model delta =
  let (targetX, targetY) = (model.x, model.y) in
    let yDist = (y - targetY)
        xDist = (x - targetX)
        realDist = sqrt(xDist^2 + yDist^2)
        newX = x - xDist / realDist * speed * delta /60
        newY = y - yDist / realDist * speed * delta /60

  in EnemyUpdater (newX, newY, width, height) (allahuakbarEnemyUpdate newX newY width height speed)

--CLEANUP STUFF
bulletIsOnScreen : BUpdater -> Bool
bulletIsOnScreen updater =
  case updater of 
    BUpdater (x, y) _ ->
      x <= 500 && x >= 0 && y <= 500 && y >= 0
    HUpdater (x, y) _ ->
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
          (hit x y width height bx by 5 5)
        HUpdater (bx, by) _ ->
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
    HUpdater (x, y) _ -> 
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

getDistance : Float -> Float -> Float -> Float -> Float
getDistance x1 y1 x2 y2 =
  sqrt((y2-y1)^2 + (x2-x1)^2)

getClosestEnemy : Model -> Float -> Float -> EnemyUpdater ->  (Float, Float)
getClosestEnemy model x y defaultEnemy =
  let enemy = (List.foldr
                (\enemy acc-> 
                  case enemy of 
                    EnemyUpdater (ex, ey, _, _) _ -> 
                      case acc of 
                        EnemyUpdater (accx, accy, _, _) _ -> 
                          if (getDistance ex ey x y)  < (getDistance model.x model.y accx accy) then
                            enemy
                          else
                            acc)
                          defaultEnemy
                          model.enemies) in
  case enemy of 
    EnemyUpdater (x, y, _, _) _ ->
      (x, y)


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
    Shoot -> (spawnHomingBullet (updateControls model model.key) -20)
    _ -> {model | key = keyCode}

updateBullet : Model -> Float -> BUpdater -> BUpdater
updateBullet model delta updater =
  case updater of
    BUpdater _ func -> func delta
    HUpdater _ func -> func model delta

updateEnemy : Model -> Float -> EnemyUpdater -> EnemyUpdater
updateEnemy model delta updater =
  case updater of
    EnemyUpdater _ func -> func model delta

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Key q -> (updateControls model q, Cmd.none)
        Tick newTime -> 
          case model.lastTime of
            Just oldTime -> 
              let newBullets = List.map ((updateBullet model) (newTime - oldTime)) model.bullets 
                  newEnemies = List.map (updateEnemy model (newTime - oldTime)) model.enemies
              in
              (dealWithPlayerCollision {model | lastTime=Just newTime
                      , bullets=List.filter bulletIsOnScreen newBullets
                      , enemies= List.filter (enemyIsAlive model) newEnemies}, Cmd.none)
            Nothing -> ({model | lastTime = Just newTime}, Cmd.none)
      



