module Subscriptions exposing (..)

import Types exposing (..)
import Keyboard
import Time exposing (..)

-- SUBSCRIPTIONS
-- 65 is a
-- 68 is d
-- 87 is w
-- 83 is s
-- 32 is space

handleDown : (Keyboard.KeyCode, ButtonState) -> Msg
handleDown state =
    case state of
        (68, None) -> Key Right
        (65, None) -> Key Left
        (87, None) -> Key Up
        (83, None) -> Key Down

        (87, Left) -> Key UpLeft
        (87, Right) -> Key UpRight
        (83, Left) -> Key DownLeft
        (83, Right) -> Key DownRight

        (65, Up) -> Key UpLeft
        (68, Up) -> Key UpRight
        (65, Down) -> Key DownLeft
        (68, Down) -> Key DownRight

        (32, _) -> Key Shoot

        (_ , current) -> Key current

handleUp : (Keyboard.KeyCode, ButtonState) -> Msg
handleUp state =
    case state of
        (87, Up) -> Key None
        (83, Down) -> Key None
        (68, Right) -> Key None
        (65, Left) -> Key None

        (65, UpLeft) -> Key Up
        (65, DownLeft) -> Key Down
        (68, UpRight) -> Key Up
        (68, DownRight) -> Key Down

        (87, UpLeft) -> Key Left
        (87, UpRight) -> Key Right
        (83, DownLeft) -> Key Left
        (83, DownRight) -> Key Right

        (_ , current) -> Key current


-- on my browser keyboard presses does not work properly
-- so I'm using ups and downs
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
      Keyboard.downs (\k -> handleDown (k, model.key))
      , Keyboard.ups (\k -> handleUp (k, model.key))
      , Time.every (Time.second/60) Tick
     ]