port module Main4 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html5.DragDrop as DragDrop
import Json.Decode exposing (Value)
import Dict exposing (Dict)
import Programs
import Images

import Arithmetic exposing (isPrime)

port dragstart : Value -> Cmd msg


type alias Position =
  { x : Int
  , y : Int
  }

type alias Window = 
  { pos : Position
  , hovering : Bool
  , dead : Bool
  }

defaultWindow : Window
defaultWindow =
  { pos = 
    { x = 0
    , y = 0
    }
    , hovering = False
    , dead = False
  }

getWindow : Model -> Int -> Window
getWindow model id = 
  case Dict.get id (model.windows) of
    Nothing -> defaultWindow
    Just aWindow -> aWindow


type alias Model =
    { windows : Dict Int Window
    , dragDrop : DragDrop.Model Int Int
    , isPrime : Bool
    }


type Msg
    = DragDropMsg (DragDrop.Msg Int Int)
    | Hovering Int
    | NotHovering Int
    | KillWindow Int
    | IsPrime Int


init : () -> (Model, Cmd msg)
init () =
    ( { windows = Dict.empty
      , dragDrop = DragDrop.init
      , isPrime = False
      }
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
            ( { model
                | dragDrop = model_
                , windows = 
                    case result of
                      Nothing -> model.windows

                      Just (id, _, newPos) -> 
                        Dict.insert id 
                          { pos = 
                            { x = newPos.x
                            , y = newPos.y
                            }
                            , hovering = (getWindow model id).hovering
                            , dead = (getWindow model id).dead
                          } model.windows
              }
            , DragDrop.getDragstartEvent msg_
                |> Maybe.map (.event >> dragstart)
                |> Maybe.withDefault Cmd.none
            )

        Hovering id -> 
          let 
            cWin = getWindow model id
          in
            ({model | windows = Dict.insert id {cWin | hovering = True} model.windows }, Cmd.none)

        NotHovering id -> 
          let 
            cWin = getWindow model id
          in
            ({model | windows = Dict.insert id {cWin | hovering = False} model.windows  }, Cmd.none)

        KillWindow id -> 
          let 
            cWin = getWindow model id
          in
            ({model | windows = Dict.insert id {cWin | dead = True} model.windows  }, Cmd.none)

        IsPrime int ->
          ({model | isPrime = isPrime int}, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

divStyle : List (Attribute Msg)
divStyle =
    [ style "z-index" "1"
    ]

view : Model -> Html Msg
view model =
  div []
      ([ desktop model
       ] 
       ++ window model 1 "ElmIcon" Programs.imageContent
       ++ window model 2 "Notepad" Programs.textArea
       ++ window model 3 "Calculator" Programs.calculator
       ++ window model 4 "PrimeChecker" (primeChecker model)
      )

desktop : Model -> Html Msg
desktop model =
  div (
    [ style "width" "100%"
    , style "height" "100%"
    , style "position" "absolute"
    , style "top" "50"
    , style "left" "0"
    , style "overflow" "hidden"
    , style "background-color" "#360036"
  ] ++ DragDrop.droppable DragDropMsg 1)
  [ toolbar
  ]


window : Model -> Int -> String ->Html Msg -> List (Html Msg)
window model id title content =
  if (getWindow model id).dead then []
  else 
    [ div
      (  style "position" "fixed"
      :: style "left" (String.fromInt (getWindow model id).pos.x ++ "px")
      :: style "top" (String.fromInt (getWindow model id).pos.y ++ "px")
      :: style "resize" "both"
      :: divStyle
      )
      [ div 
        ([ width 100
        , style "background-color" (if (getWindow model id).hovering then "#880088" else "#660066")
        , style "color" "#ffffff"
        , style "font-family" "Arial"
        , style "display" "flex"
        , onMouseOver (Hovering id)
        , onMouseOut (NotHovering id)
        ] ++ DragDrop.draggable DragDropMsg id) 
        [ div 
          [ style "padding" "10px"
          ] 
          [ text title
          ]
        , div 
          [ style "text-align" "right"
          , style "width" "100%"
          ] 
          [ button 
            [ onClick (KillWindow id)
            , style "background-color" "#ff6600"
            , style "border" "none"
            , style "color" "#ffffff"
            , style "height" "100%"
            , style "width" "40px"
            ] [ text "—"] 
          -- , button 
          --   [ onClick (KillWindow id)
          --   , style "background-color" "#ff0000"
          --   , style "border" "none"
          --   , style "color" "#ffffff"
          --   , style "height" "100%"
          --   ] [ text "◻"] 
          , button 
            [ onClick (KillWindow id)
            , style "background-color" "#ff0000"
            , style "border" "none"
            , style "color" "#ffffff"
            , style "height" "100%"
            , style "width" "40px"
            ] [ text "✕"] 
          ] 
        ]
      , content
      ]
    ]

toolbar : Html Msg
toolbar = 
  div 
    [ style "position" "fixed"
    , style "bottom" "0"
    , style "width" "100%"
    , style "color" "#ffffff"
    , style "height" "40px"
    , style "background-color" "#660066"
    ] 
    [ 
      img 
      [ src Images.nix
      , height 40
      ] []
    ]

primeChecker : Model -> Html Msg
primeChecker model = 
  div 
    [ style "color" "#ffffff"
    , style "font-family" "Arial"
    , style "padding" "10px"
    , style "background-color" "#660066"
    ]
    [ input 
      [ onInput (\str -> IsPrime (String.toInt str |> Maybe.withDefault 2)) 
      , placeholder "int"
      , style "width" "100%"
      , style "font-size" "16pt"
      , style "border" "none"
      , style "text-align" "center"
      ] []
    , br [] []
    , div 
      [ style "background-color" (if model.isPrime then pastel.green else pastel.red)
      , style "padding" "5px"
      , style "text-align" "center"
      , style "margin-top" "10px"
      , style "color" "#000000"
      ] 
      [ text (if model.isPrime then "This is prime!" else "This is not prime")
      ]
    ]

pastel = 
  { red = "#ffb3ba"
  , orange = "#ffdfba"
  , yellow = "#ffffba"
  , green = "#baffc9"
  , blue = "#bae1ff"
  }

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }