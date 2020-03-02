module SquareRoots exposing (Msg(..), main, update)

import Browser
import Browser.Dom as Dom
import HomeworkHelper as HH
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random



-- import Task


main =
    Browser.element
        { init = init
        , update = HH.update continue checkSubmission update
        , subscriptions = subscriptions
        , view = HH.view problemView explanationView
        }



-- Model


type alias Model =
    { firstNum : Int
    , secondNum : Int
    , firstInput : String
    , secondInput : String
    }


newNums : Random.Generator ( Int, Int )
newNums =
    Random.map2 (\a b -> ( a, b ))
        (Random.uniform 1 [ 2, 3, 4, 5, 6, 7, 8 ])
        (Random.uniform 1 [ 2, 3, 5, 7 ])


init : () -> ( HH.Model Model, Cmd (HH.Msg Msg) )
init _ =
    ( HH.newModel "Square Roots"
        { firstNum = 0
        , secondNum = 0
        , firstInput = ""
        , secondInput = ""
        }
    , Cmd.map HH.SubMsg (Random.generate RandNums newNums)
    )



-- Update


type Msg
    = RandNums ( Int, Int )
    | ChangeFirst String
    | ChangeSecond String
    | SubmitKeyDown Int
    | NoOp


checkSubmission : Model -> HH.SubmitStatus
checkSubmission model =
    let
        first =
            Maybe.withDefault 1 (String.toInt model.firstInput)

        second =
            Maybe.withDefault 1 (String.toInt model.secondInput)
    in
    if first == model.firstNum && second == model.secondNum then
        HH.Correct

    else if first * first * second == model.firstNum * model.firstNum * model.secondNum then
        HH.Imprecise

    else
        HH.Incorrect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitKeyDown key ->
            if key == 13 then
                ( model, HH.performSubmit NoOp )

            else
                ( model, Cmd.none )

        RandNums ( x, y ) ->
            ( { model | firstNum = x, secondNum = y }, Cmd.none )

        ChangeFirst x ->
            ( { model | firstInput = x }, Cmd.none )

        ChangeSecond x ->
            ( { model | secondInput = x }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


continue : Model -> ( Model, Cmd Msg )
continue model =
    ( { model | firstInput = "", secondInput = "" }, Random.generate RandNums newNums )



-- Subscriptions


subscriptions : HH.Model Model -> Sub (HH.Msg Msg)
subscriptions model =
    Sub.none



-- View


problemView : Bool -> Model -> Html Msg
problemView disable model =
    div []
        [ h4 [ class "subtitle is-4" ] [ text "Simplify the following:" ]

        -- , h4 [ class "subtitle is-4" ] [ text (String.fromInt model.firstNum ++ " / " ++ String.fromInt model.secondNum) ]
        , h4 [ class "title is-4" ] [ text ("√" ++ String.fromInt (model.firstNum * model.firstNum * model.secondNum)) ]
        , div [ class "field is-grouped" ]
            [ label [ class "label is-large" ] [ text "Answer:" ]
            , input
                [ class "input"
                , style "max-width" "50px"
                , value model.firstInput
                , onInput ChangeFirst
                , disabled disable
                , HH.onKeyDown SubmitKeyDown
                ]
                []
            , label [ class "label is-large", style "margin-left" "7px" ] [ text " × √" ]
            , input
                [ class "input"
                , style "max-width" "50px"
                , value model.secondInput
                , onInput ChangeSecond
                , disabled disable
                , HH.onKeyDown SubmitKeyDown
                ]
                []
            ]
        ]


explanationView : Model -> Html Msg
explanationView model =
    div []
        [ h2 [ class "title is-2" ] [ text "Incorrect!" ]
        , label [ class "label is-large" ] [ text ("√" ++ String.fromInt (model.firstNum * model.firstNum * model.secondNum)) ]
        , label [ class "label is-large" ]
            [ text
                ("= √("
                    ++ String.fromInt (model.firstNum * model.firstNum)
                    ++ " × "
                    ++ String.fromInt model.secondNum
                    ++ ")"
                )
            ]
        , label [ class "label is-large" ]
            [ text
                ("= √"
                    ++ String.fromInt (model.firstNum * model.firstNum)
                    ++ " × √"
                    ++ String.fromInt model.secondNum
                )
            ]
        , label [ class "label is-large" ]
            [ text
                ("= "
                    ++ String.fromInt model.firstNum
                    ++ " × √"
                    ++ String.fromInt model.secondNum
                )
            ]
        ]
