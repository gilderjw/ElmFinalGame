module ElmFinalGame.View exposing (..)

import ElmFinalGame.Types exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)

view : Model -> Html Msg
view model = 
   svg [ viewBox "0 0 500 500", width "500px" ]
       [ rect [ x (toString model.x), y (toString model.y), width "60", height "10", fill "#0B79CE" ] []
       ]