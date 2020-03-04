module Template exposing (main)

import Browser
import Browser.Dom as Dom
import HomeworkHelper as HH
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


main =
    Browser.element
        { init = init
        , update = HH.update continue checkSubmission update
        , subscriptions = subscriptions
        , view = HH.view problemView explanationView
        }



-- Model


type alias Model =
    { x : Int
    , y : String
    }


init : () -> ( HH.Model Model, Cmd (HH.Msg Msg) )
init _ =
    ( HH.newModel "Template"
        { x = 0
        , y = ""
        }
    , Cmd.none
    )



-- Update


type Msg
    = Something
    | NoOp


checkSubmission : Model -> HH.SubmitStatus
checkSubmission model =
    HH.Correct


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Something ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


continue : Model -> ( Model, Cmd Msg )
continue model =
    --- update model when user continues
    ( model, Cmd.none )



-- Subscriptions


subscriptions : HH.Model Model -> Sub (HH.Msg Msg)
subscriptions model =
    Sub.none



-- View


problemView : Bool -> Model -> Html Msg
problemView disable model =
    -- View of problem description
    div []
        [ h4 [ class "subtitle is-4" ] [ text "Description of what to do:" ]
        , h4 [ class "title is-4" ] [ text "This is the problem" ]
        , div [ class "field is-grouped" ]
            [ label [ class "label is-large" ] [ text "Answer:" ]
            , input
                [ class "input"
                , style "max-width" "50px"
                ]
                []
            ]
        ]


explanationView : Model -> Html Msg
explanationView model =
    -- View of explanation when wrong
    div []
        [ label [ class "label is-large" ] [ text "This is what went wrong" ]
        ]
