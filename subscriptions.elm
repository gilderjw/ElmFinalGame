module ElmFinalGame.Subscriptions exposing (..)

import ElmFinalGame.Types exposing (..)
import Keyboard

-- SUBSCRIPTIONS
-- 37 is left
-- 39 is right
handleDown : (Keyboard.KeyCode, ButtonState) -> Msg
handleDown state =
    case state of
        (68, Left) -> Key Both
        (65, Right) -> Key Both
        (68, None) -> Key Right
        (65, None) -> Key Left
        (_ , current) -> Key current

handleUp : (Keyboard.KeyCode, ButtonState) -> Msg
handleUp state =
    case state of
        (68, Both) -> Key Left
        (65, Both) -> Key Right
        (68, Right) -> Key None
        (65, Left) -> Key None
        (_ , current) -> Key current


-- on my browser keyboard presses does not work properly
-- so I'm using ups and downs
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
      Keyboard.downs (\k -> handleDown (k, model.key))
      , Keyboard.ups (\k -> handleUp (k, model.key))
     ]