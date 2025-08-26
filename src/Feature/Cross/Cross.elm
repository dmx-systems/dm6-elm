-- src/Feature/Cross.elm


module Main exposing (viewButton)

import Html.Attributes as Attr


viewButton : Model -> Html Msg
viewButton _ =
    button [ Attr.id "btn-Cross", onClick CrossClick ] [ text "Cross" ]
