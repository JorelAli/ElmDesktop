port module Main3 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import Json.Decode exposing (Value)


port dragstart : Value -> Cmd msg


type alias Position =
  { x : Int
  , y : Int
  }


type alias Model =
    { data : { position : Position }
    , dragDrop : DragDrop.Model () ()
    }


type Msg
    = DragDropMsg (DragDrop.Msg () ())


init () =
    ( { data = { position = { x = 2, y = 30 } }
      , dragDrop = DragDrop.init
      }
    , Cmd.none
    )



update msg model =
    case msg of
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
            ( { model
                | data =
                    case result of
                        Nothing ->
                            model.data

                        Just ( _, _, newPos ) ->
                            { position = {x = newPos.x, y = newPos.y} }
              }
            , DragDrop.getDragstartEvent msg_
                |> Maybe.map (.event >> dragstart)
                |> Maybe.withDefault Cmd.none
            )

subscriptions : Model -> Sub a
subscriptions model =
    Sub.none

divStyle : List (Attribute msg)
divStyle =
    [ style "border" "1px solid black"
    , style "padding" "50px"
    , style "text-align" "center"
    ]


view : Model -> Html Msg
view model =
    let
        dropId =
            DragDrop.getDropId model.dragDrop

        position =
            DragDrop.getDroppablePosition model.dragDrop
    in
    div []
        [ --viewDiv model.data.position model.data dropId position
         desktop model.data.position
         , myImg model.data.position model.data position
        -- , viewDiv Middle model.data dropId position
        -- , viewDiv Down model.data dropId position
        ]

myImg position data droppablePosition =
  let 
    coords = 
          case droppablePosition of
            Nothing ->
              position
                -- {x = 0, y=0}

            Just pos ->
                {x = pos.x, y = pos.y}
  in
  img (
                src "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg" 
                :: style "position" "absolute"
                :: style "left" (String.fromInt coords.x ++ "px")
                :: style "top" (String.fromInt coords.y ++ "px")
                :: width 100 
                :: DragDrop.draggable DragDropMsg ()) []
            -- , text (String.fromInt data.count)
            -- ]

desktop : Position -> Html Msg
desktop pos =
  div (
    [ style "border" "1px solid black"
    , style "padding" "50px"
    , style "background-color" "#ff0000"
  ] ++ DragDrop.droppable DragDropMsg ())
  []

isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Just _ ->
            False

        Nothing ->
            True

viewDiv : Position -> { position : Position } -> Maybe Position -> Maybe DragDrop.Position -> Html Msg
viewDiv position data dropId droppablePosition =
    let
        highlight =
          case droppablePosition of
            Nothing ->
                []

            Just pos ->
                    [ style "background-color" "magenta" ]

        coords = 
          case droppablePosition of
            Nothing ->
              position
                -- {x = 0, y=0}

            Just pos ->
                {x = pos.x, y = pos.y}
    in
    div
        (divStyle
            ++ highlight
            ++ (if data.position /= position then
                    DragDrop.droppable DragDropMsg ()

                else
                    []
               )
        ) 
        (if data.position == position then
            [ img (
                src "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg" 
                :: style "position" "absolute"
                :: style "left" (String.fromInt coords.x ++ "px")
                :: style "top" (String.fromInt coords.y ++ "px")
                :: width 100 
                :: DragDrop.draggable DragDropMsg ()) []
            -- , text (String.fromInt data.count)
            ]

         else
            []
        )

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }