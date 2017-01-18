module ElmFinalGame.View exposing (..)

import ElmFinalGame.Types exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)

bulletToImage: BUpdater -> Svg Msg
bulletToImage updater = 
  case updater of
    BUpdater (x,y) _ -> circle [ cx (toString x), cy (toString y), r "3", fill "#ff0000"][]


view : Model -> Html Msg
view model = 
   svg [ viewBox "0 0 500 500", width "500px" ]
       (List.append
       (List.map bulletToImage model.bullets)
       [ rect [ x (toString model.x), y (toString model.y), width "60", height "10", fill "#0B79CE" ] []
       ])