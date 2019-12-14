port module Main4 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html5.DragDrop as DragDrop
import Json.Decode exposing (Value)
import Dict exposing (Dict)

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
    }


type Msg
    = DragDropMsg (DragDrop.Msg Int Int)
    | Hovering Int
    | NotHovering Int
    | KillWindow Int


init : () -> (Model, Cmd msg)
init () =
    ( { windows = Dict.empty
      , dragDrop = DragDrop.init
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
        ([ desktop 
        ] ++ window model 1 "a" imageContent
          ++ window model 2 "Notepad" textArea)

desktop : Html Msg
desktop =
  div (
    [ style "border" "1px solid black"
    , style "padding" "50px"
    , style "width" "100%"
    , style "height" "100%"
    , style "position" "absolute"
    , style "top" "50"
    , style "left" "0"
    , style "background-color" "#0000ff"
  ] ++ DragDrop.droppable DragDropMsg 1)
  []


window : Model -> Int -> String ->Html Msg -> List (Html Msg)
window model id title content =
  if (getWindow model id).dead then []
  else 
    [ div
      (  style "position" "absolute"
      :: style "left" (String.fromInt (getWindow model id).pos.x ++ "px")
      :: style "top" (String.fromInt (getWindow model id).pos.y ++ "px")
      :: style "resize" "both"
      :: divStyle
      )
      [ div 
        ([ width 100
        , style "background-color" (if (getWindow model id).hovering then "#ff0000" else "#00ff00")
        , style "padding" "10px"
        , style "display" "flex"
        , onMouseOver (Hovering id)
        , onMouseOut (NotHovering id)
        ] ++ DragDrop.draggable DragDropMsg id) 
        [ text title
        , div 
          [ style "text-align" "right"
          , style "width" "100%"
          ] 
          [ button [ onClick (KillWindow id)] [ text "X"] ]
        
          
        ]
      , content
      ]
    ]


textArea : Html Msg
textArea =
  textarea [] [text "hi"]

imageContent : Html Msg
imageContent = 
  img
    [ src "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg" 
    , width 100 
    ] []


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }