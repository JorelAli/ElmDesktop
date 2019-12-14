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
  , dead : Bool
  }

defaultWindow : Window
defaultWindow =
  { pos = 
    { x = 0
    , y = 0
    }
    , dead = False
  }

getWindow : Model -> Int -> Window
getWindow model id = 
  case Dict.get id model.windows of
    Nothing -> defaultWindow
    Just aWindow -> aWindow


type alias Model =
    { windows : Dict Int Window
    , dragDrop : DragDrop.Model Int Int
    , isPrime : Bool
    , imageUrl : String
    , markdown : String
    , windowHtmls : Windows--List (Model -> Html Msg)
    }

type Windows 
  = Windows (List (Model -> Html Msg))

unwrapWindow : Windows -> List (Model -> Html Msg)
unwrapWindow w =
  case w of
    Windows list -> list

type ProgramType
  = PrimeProgram
  | ImageViewerProgram


type Msg
    = DragDropMsg (DragDrop.Msg Int Int)
    | KillWindow Int
    | IsPrime Int
    | ChangeImageUrl String
    | UpdateMarkdown String
    | Open ProgramType


init : () -> (Model, Cmd msg)
init () =
    ( { windows = Dict.empty
      , dragDrop = DragDrop.init
      , isPrime = False
      , imageUrl = "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg"
      , markdown = "# Hello world\nThis is some text"
      , windowHtmls = Windows
        [ \model -> window model 1 "ImgViewer" (imageContent model.imageUrl)
        , \model -> window model 2 "Notepad" Programs.textArea
        , \model -> window model 3 "Calculator" Programs.calculator
        -- , \model -> window model 4 "PrimeChecker" (primeChecker model)
        , \model -> window model 4 "MarkdownEditor" (markdownEditor model.markdown)
        ]
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
                            , dead = (getWindow model id).dead
                          } model.windows
              }
            , DragDrop.getDragstartEvent msg_
                |> Maybe.map (.event >> dragstart)
                |> Maybe.withDefault Cmd.none
            )

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

        Open programType ->
          case programType of
            PrimeProgram ->
              ({
                model 
                | windowHtmls 
                  = Windows ((unwrapWindow model.windowHtmls) 
                  ++ [\m -> window m 
                ((+) 1 <| List.length <| unwrapWindow model.windowHtmls) "PrimeChecker" (primeChecker m)]
              )}, Cmd.none)
            
            ImageViewerProgram ->
              (model, Cmd.none)



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
       ++ List.map (\w -> w model) (unwrapWindow model.windowHtmls)
      )

desktop : Model -> Html Msg
desktop model =
  div (
    [ css 
      [ width (pct 100) 
      , height (pct 100)
      , position absolute
      , left (px 0)
      , overflow hidden
      , backgroundColor (hex "360036")
      ] 
  ] ++ List.map Html.Styled.Attributes.fromUnstyled (DragDrop.droppable DragDropMsg 1))
  [ toolbar
  , toolbarProgram "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg" PrimeProgram
  ]


window : Model -> Int -> String -> Html Msg -> Html Msg
window model id title content =
  let 
    theWindow = getWindow model id
  in
  if theWindow.dead then div [] []
  else 
    div
      [ css 
        [ divStyle
        , position fixed
        , left (px <| toFloat theWindow.pos.x)
        , top (px <| toFloat theWindow.pos.y)
        , resize both
        ]
      ]
      [ div 
        ([ Html.Styled.Attributes.width 100
        , css 
          [ backgroundColor (hex "660066")
          , color (hex "ffffff")
          , fontFamilies [ "Arial" ] 
          , displayFlex
          , hover [
            backgroundColor (hex "880088")
          ]
          ]
        -- , style "background-color" (if (getWindow model id).hovering then "#880088" else "#660066")
        -- , style "color" "#ffffff"
        -- , style "font-family" "Arial"
        -- , style "display" "flex"
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
    [ img 
      [ src Images.nix
      , Html.Styled.Attributes.height 40
      ] []
    ]

toolbarProgram : String -> ProgramType -> Html Msg
toolbarProgram url program = 
  div 
    [ css
      [ position fixed
      , bottom (px 0)
      , width (pct 100)
      , color (hex "ffffff")
      , height (px 40)
      , backgroundColor (hex "660066")
      , paddingLeft (px 20)
      ] 
    ] 
    [ img 
      [ src url
      , Html.Styled.Attributes.height 40
      , onClick (Open program)
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