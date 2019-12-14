port module Main4 exposing (..)

import Browser
-- import Html exposing (..)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)

import Css exposing (..)

import Html.Styled exposing (Html, div, textarea, text, button, img, br, input)

import Html.Styled.Attributes exposing (css, src, placeholder)
import Html.Styled.Events exposing (..)
import Html5.DragDrop as DragDrop
import Json.Decode exposing (Value)
import Dict exposing (Dict)
import Programs
import Images
import Arithmetic exposing (isPrime)
import Markdown

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
    , imageUrl : String
    , markdown : String
    }


type Msg
    = DragDropMsg (DragDrop.Msg Int Int)
    | Hovering Int
    | NotHovering Int
    | KillWindow Int
    | IsPrime Int
    | ChangeImageUrl String
    | UpdateMarkdown String


init : () -> (Model, Cmd msg)
init () =
    ( { windows = Dict.empty
      , dragDrop = DragDrop.init
      , isPrime = False
      , imageUrl = "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg"
      , markdown = "# Hello world\nThis is some text"
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

        ChangeImageUrl url ->
          ({model | imageUrl = url}, Cmd.none)

        UpdateMarkdown str ->
          ({model | markdown = str}, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- divStyle : List (Attribute Msg)
divStyle =
  Css.batch
    [ zIndex (int 1)
    ]

view : Model -> Html Msg
view model =
  div []
      ([ desktop model
       ] 
       ++ window model 1 "ImgViewer" (imageContent model.imageUrl) --//Programs.imageContent
       ++ window model 2 "Notepad" Programs.textArea
       ++ window model 3 "Calculator" Programs.calculator
       ++ window model 4 "PrimeChecker" (primeChecker model)
       ++ window model 5 "MarkdownEditor" (markdownEditor model.markdown)
      )

desktop : Model -> Html Msg
desktop model =
  div (
    [ css 
      [ width (pct 100) 
      , height (pct 100)
      , position absolute
      , top (px 50)
      , left (px 0)
      , overflow hidden
      , backgroundColor (hex "360036")
      ] 
  ] ++ List.map Html.Styled.Attributes.fromUnstyled (DragDrop.droppable DragDropMsg 1))
  [ toolbar
  ]


window : Model -> Int -> String -> Html Msg -> List (Html Msg)
window model id title content =
  if (getWindow model id).dead then []
  else 
    [ div
      [ css 
        [ divStyle
        , position fixed
        , left (px <| toFloat (getWindow model id).pos.x)
        , top (px <| toFloat (getWindow model id).pos.y)
        , resize both
        ]
      ]
      -- (  style "position" "fixed"
      -- :: style "left" (String.fromInt (getWindow model id).pos.x ++ "px")
      -- :: style "top" (String.fromInt (getWindow model id).pos.y ++ "px")
      -- :: style "resize" "both"
      -- :: divStyle
      -- )
      [ div 
        ([ Html.Styled.Attributes.width 100
        , css 
          [ backgroundColor (if (getWindow model id).hovering then hex "880088" else hex "660066")
          , color (hex "ffffff")
          , fontFamilies [ "Arial" ] 
          , displayFlex
          ]
        -- , style "background-color" (if (getWindow model id).hovering then "#880088" else "#660066")
        -- , style "color" "#ffffff"
        -- , style "font-family" "Arial"
        -- , style "display" "flex"
        , onMouseOver (Hovering id)
        , onMouseOut (NotHovering id)
        ] ++ List.map Html.Styled.Attributes.fromUnstyled (DragDrop.draggable DragDropMsg id)) 
        [ div 
          [ css 
            [ padding (px 10)
            ] 
          ]
          [ text title
          ]
        , div 
          [ css 
            [ textAlign right
            , width (pct 100)
            ]
          ] 
          [ button 
            [ onClick (KillWindow id)
            , css
              [ backgroundColor (hex "ff6600")
              , border zero
              , color (hex "ffffff")
              , height (pct 100)
              , width (px 40)
              ]
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
            , css 
              [ backgroundColor (hex "ff0000")
              , border zero
              , color (hex "ffffff")
              , height (pct 100)
              , width (px 40)
              ]
            ] [ text "✕"] 
          ] 
        ]
      , content
      ]
    ]
  

toolbar : Html Msg
toolbar = 
  div 
    [ css
      [ position fixed
      , bottom (px 0)
      , width (pct 100)
      , color (hex "ffffff")
      , height (px 40)
      , backgroundColor (hex "660066")
      ] 
    ] 
    [ 
      img 
      [ src Images.nix
      , Html.Styled.Attributes.height 40
      ] []
    ]


-- Programs

primeChecker : Model -> Html Msg
primeChecker model = 
  div 
    [ css 
      [ color (hex "ffffff")
      , fontFamilies [ "Arial" ]
      , padding (px 10)
      , backgroundColor (hex "660066")
      ]
    ]
    [ input 
      [ onInput (\str -> IsPrime (String.toInt str |> Maybe.withDefault 2)) 
      , placeholder "int"
      , css
        [ width (pct 100)
        , fontSize (pt 16)
        , border zero
        , textAlign center
        ]
      ] []
    , br [] []
    , div 
      [ css
        [ backgroundColor (hex (if model.isPrime then pastel.green else pastel.red))
        , padding (px 5)
        , textAlign center
        , marginTop (px 10)
        , color (hex "000000")
        ]
      ] 
      [ text (if model.isPrime then "This is prime!" else "This is not prime")
      ]
    ]

imageContent : String -> Html Msg
imageContent url = 
  div 
  [ css 
    [ resize both
    , overflow auto
    ]
  ]
  [ input 
    [ placeholder "url"
    , onInput ChangeImageUrl
    , css [ width (pct 100) ]
    ] 
    []
  , img
    [ src url
    , css [ width (pct 100) ]
    -- , width 200
    ] []
  ]

markdownEditor : String -> Html Msg
markdownEditor md = 
  div 
  [ css
    [ backgroundColor (hex pastel.blue)
    , displayFlex
    , alignItems stretch
    , fontFamilies [ "Arial" ]
    ]
  ] 
  [ textarea 
    [ placeholder md
    , onInput UpdateMarkdown
    , css
      [ overflow auto
      , height (pct 100)
      ]
    ] []
  , div 
    [ css
      [ marginLeft (px 20)
      , marginRight (px 20)
      ]
    ] <| List.map Html.Styled.fromUnstyled (Markdown.toHtml Nothing md)
  ]

--Main

pastel = 
  { red = "ffb3ba"
  , orange = "ffdfba"
  , yellow = "ffffba"
  , green = "baffc9"
  , blue = "bae1ff"
  }

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> Html.Styled.toUnstyled 
        }