module Main exposing (main)


import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as Keyed
import Json.Decode as Decode


type alias Model =
    { current : String
    , entries : List Entry
    }


type Msg
    = ChangeText String
    | KeyDown Int
    | Submit


type alias Entry =
    { key : String
    , text : String
    }

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( { current = "", entries = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeText text ->
            ( { model | current = text }, Cmd.none )


        KeyDown 13 ->
            if canSubmit model then
                ( model |> submitCurrent, Cmd.none )
            else
                ( model, Cmd.none )

        KeyDown _ ->
            ( model, Cmd.none )


        Submit ->
            ( model |> submitCurrent, Cmd.none )


canSubmit : Model -> Bool
canSubmit { current } =
    (String.length current) > 0


submitCurrent : Model -> Model
submitCurrent model =
    let
        entry =
            { key = toString (List.length model.entries)
            , text = model.current
            }
    in
    { model
        | current = ""
        , entries = entry :: model.entries
    }


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Events.on "keydown" (Decode.map tagger Events.keyCode)


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "frame" ]
        [ Html.node "style" []
            [ Html.text
                """

@import url("https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,300,700,600");

html {
    box-sizing: border-box;
}
*, *:before, *:after {
    box-sizing: inherit;
}

body {
    font-family: "Source Sans Pro", sans-serif;
    font-size: 16px;
}

.frame {
    margin: 20px auto;
    max-width: 400px;
    width: 100%;
}

.textbox {
    display: block;
    padding: 4px 8px;
    width: 100%;
}

.btn {
    display: inline-block;
    float: right;
    margin-top: 10px;
}

.entry {
    animation-duration: 1.5s;
    animation-name: glow;
    background: #f5f5f5;
    border: #eee 1px solid;
    border-radius: 5px;
    margin-top: 40px;
    padding: 8px 16px;
}

.entries {
    clear: both;
    list-style: none;
    margin: 20px 0 0 0;
    padding: 0;
}

@keyframes glow {
    0% {
        box-shadow: none;
    }
    10% {
        box-shadow: 0 0 3em #f0641b, 0 0 0.7em #f0641b;
    }
    100% {
        box-shadow: none;
    }
}

                """
            ]
        , Html.input
            [ Attr.class "textbox"
            , Events.onInput ChangeText
            , onKeyDown KeyDown
            , Attr.value model.current
            ]
            []
        , Html.button
            [ Attr.class "btn"
            , Attr.disabled (not (canSubmit model))
            , Events.onClick Submit
            ]
            [ Html.text "Click Me!"
            ]
        , Keyed.ul [ Attr.class "entries" ]
            (List.map viewEntry model.entries)
        ]


viewEntry : Entry -> ( String, Html msg )
viewEntry { key, text }=
    ( key
    , Html.li [ Attr.class "entry" ]
        [ Html.text text ]
    )
