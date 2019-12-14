port module Main2 exposing (..)

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
    { data : { count : Int, position : Position }
    , dragDrop : DragDrop.Model Int Position
    }


type Msg
    = DragDropMsg (DragDrop.Msg Int Position)


init () =
    ( { data = { count = 0, position = { x = 2, y = 30 } }
      , dragDrop = DragDrop.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( 
  { data : 
    { count : Int
    , position : Position 
    }
  , dragDrop : DragDrop.Model Int Position
  }
  , Cmd msg
  )
update msg model =
    case msg of
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
            ( { model
                | dragDrop = model_
                , data =
                    case result of
                        Nothing ->
                            model.data

                        Just ( count, position, newPos ) ->
                            { count = count + 1, position = {x = newPos.x, y = newPos.y} }
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
    , style "z-index" "1"
    ]


view model =
    let
        dropId =
            DragDrop.getDropId model.dragDrop

        position =
            DragDrop.getDroppablePosition model.dragDrop
    in
    div []
        [ viewDiv model.data.position model.data dropId position
        , desktop model.data.position
        -- , window model.data.count (case position of
        --     Nothing ->
        --       {x=0, y=0, width=0, height=0}

        --     Just pos ->
        --         pos)
        --  , myImg model.data.position model.data position
        -- , viewDiv Middle model.data dropId position
        -- , viewDiv Down model.data dropId position
        ]

desktop : Position -> Html Msg
desktop pos =
  div (
    [ style "border" "1px solid black"
    , style "padding" "50px"
    , style "width" "100%"
    , style "height" "100%"
    , style "position" "absolute"
    , style "top" "50"
    , style "left" "0"
    -- , style "z-index" "-1"
    , style "background-color" "#ff0000"
  ] ++ DragDrop.droppable DragDropMsg pos)
  []

isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Just _ ->
            False

        Nothing ->
            True

mkIf : Bool -> List a -> List a
mkIf b l = if b then l else []

viewDiv : Position -> { count : Int, position : Position } -> Maybe Position -> Maybe DragDrop.Position -> Html Msg
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
              {x=0, y=0, width=0, height=0}

            Just pos ->
                pos
    in
    div
        divStyle 
        (mkIf (data.position == position)
            [ img (
                src "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg" 
                :: style "position" "absolute"
                :: style "left" (String.fromInt coords.x ++ "px")
                :: style "top" (String.fromInt coords.y ++ "px")
                :: width 100 
                :: style "z-index" "1"
                :: DragDrop.draggable DragDropMsg data.count) []
            , text (String.fromInt data.count)
            ])

-- window : Int -> DragDrop.Position -> Html Msg
-- window count pos = 
--   div [] 
--     [ div (
--       style "position" "absolute"
--       :: style "left" (String.fromInt pos.x ++ "px")
--       :: style "top" (String.fromInt pos.y ++ "px")
--       -- :: width 100 
--       :: style "padding-top" "20px"
--       :: style "background-color" "#00ff00"
--       :: style "z-index" "2"
--       :: DragDrop.draggable DragDropMsg count
--     ) [] --titlebar
--     -- ,  --content
--     , img (
--                 src "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg" 
--                 -- :: style "position" "absolute"
--                 -- :: style "left" (String.fromInt coords.x ++ "px")
--                 -- :: style "top" (String.fromInt coords.y ++ "px")
--                 :: width 100 
--                 :: style "z-index" "2"
--                 :: DragDrop.draggable DragDropMsg count) []
--     ]

main : Program
  ()
  { data : { count : Int, position : Position }
  , dragDrop : DragDrop.Model Int Position
  }
  Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }