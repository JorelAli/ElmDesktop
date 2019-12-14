module Programs exposing (..)

import Html.Styled exposing (Html, div, textarea, text, button, img, br, input)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)

textArea : Html msg
textArea =
  textarea [] [text "hi"]

calculator : Html msg
calculator =
  div [] 
  [ textarea [] [ text "result" ]
  , br [] []
  , button [] [ text "9" ]
  , button [] [ text "8" ]
  , button [] [ text "7" ]
  , br [] []
  , button [] [ text "6" ]
  , button [] [ text "5" ]
  , button [] [ text "4" ]
  , br [] []
  , button [] [ text "3" ]
  , button [] [ text "2" ]
  , button [] [ text "1" ]
  , br [] []
  , button [] [ text "0" ]
  , button [] [ text "+" ]
  , button [] [ text "=" ]
  ]