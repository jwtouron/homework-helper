module SquareRoots exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Maybe
import Random
import Round
import Task


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type SubmitStatus
    = Correct
    | Incorrect
    | Imprecise


type CompleteStatus
    = Complete
    | Incomplete


type alias Model =
    { firstNum : Int
    , secondNum : Int
    , firstInput : String
    , secondInput : String
    , attempted : Int
    , successful : Int
    , completeStatus : CompleteStatus
    , submitStatus : Maybe SubmitStatus
    }


newNums : Random.Generator ( Int, Int )
newNums =
    Random.map2 (\a b -> ( a, b ))
        (Random.uniform 1 [ 2, 3, 4, 5, 6, 7, 8 ])
        (Random.uniform 1 [ 2, 3, 5, 7 ])


init : () -> ( Model, Cmd Msg )
init _ =
    ( { firstNum = 0
      , secondNum = 0
      , firstInput = ""
      , secondInput = ""
      , attempted = 0
      , successful = 0
      , completeStatus = Incomplete
      , submitStatus = Nothing
      }
    , Random.generate RandNums newNums
    )



-- Update


type Msg
    = RandNums ( Int, Int )
    | SubmitButton
    | ChangeFirst String
    | ChangeSecond String
    | Continue
    | SubmitKeyDown Int
    | NoOp


checkSubmission : Model -> SubmitStatus
checkSubmission model =
    let
        first =
            Maybe.withDefault 1 (String.toInt model.firstInput)

        second =
            Maybe.withDefault 1 (String.toInt model.secondInput)
    in
    if first == model.firstNum && second == model.secondNum then
        Correct

    else if first * first * second == model.firstNum * model.firstNum * model.secondNum then
        Imprecise

    else
        Incorrect


isComplete : Model -> Bool
isComplete model =
    toFloat model.successful
        / toFloat model.attempted
        >= 0.9
        && toFloat model.attempted
        >= 12


submit : Model -> ( Model, Cmd Msg )
submit model =
    let
        submitStatus =
            checkSubmission model

        newModel =
            { model | submitStatus = Just submitStatus, attempted = model.attempted + 1 }
    in
    case submitStatus of
        Correct ->
            let
                newModel2 =
                    { newModel | successful = newModel.successful + 1 }
            in
            if isComplete newModel2 then
                ( { newModel2 | completeStatus = Complete }, Cmd.none )

            else
                ( newModel2, Cmd.none )

        Incorrect ->
            ( newModel, Cmd.none )

        Imprecise ->
            ( { newModel | attempted = newModel.attempted - 1 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SubmitKeyDown key ->
            if key == 13 then
                submit model

            else
                ( model, Cmd.none )

        RandNums ( x, y ) ->
            ( { model | firstNum = x, secondNum = y }, Cmd.none )

        SubmitButton ->
            submit model

        ChangeFirst x ->
            ( { model | firstInput = x }, Cmd.none )

        ChangeSecond x ->
            ( { model | secondInput = x }, Cmd.none )

        Continue ->
            ( { model
                | completeStatus = Incomplete
                , submitStatus = Nothing
                , firstInput = ""
                , secondInput = ""
              }
            , Random.generate RandNums newNums
            )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


shouldDisableInput : Model -> Bool
shouldDisableInput model =
    case model.submitStatus of
        Just Imprecise ->
            False

        Nothing ->
            False

        _ ->
            True


problemView : Model -> Html Msg
problemView model =
    div []
        [ h4 [ class "subtitle is-4" ] [ text "Simplify the following:" ]
        , h4 [ class "title is-4" ] [ text ("√" ++ String.fromInt (model.firstNum * model.firstNum * model.secondNum)) ]
        , div [ class "field is-grouped" ]
            [ label [ class "label is-large" ] [ text "Answer:" ]
            , input
                [ class "input"
                , style "max-width" "50px"
                , value model.firstInput
                , onInput ChangeFirst
                , disabled (shouldDisableInput model)
                , onKeyDown SubmitKeyDown
                ]
                []
            , label [ class "label is-large", style "margin-left" "7px" ] [ text " × √" ]
            , input
                [ class "input"
                , style "max-width" "50px"
                , value model.secondInput
                , onInput ChangeSecond
                , disabled (shouldDisableInput model)
                , onKeyDown SubmitKeyDown
                ]
                []
            , button
                [ class "button is-primary"
                , onClick SubmitButton
                , disabled (shouldDisableInput model)
                ]
                [ text "Submit" ]
            ]
        ]


statsView : Model -> Html Msg
statsView model =
    h3 [ class "subtitle is-3" ]
        [ text
            ("Attempted: "
                ++ String.fromInt model.attempted
                ++ " Successful: "
                ++ String.fromInt model.successful
                ++ " ("
                ++ (if model.attempted == 0 then
                        "0.00%)"

                    else
                        Round.round 2 (toFloat model.successful / toFloat model.attempted * 100)
                            ++ "%)"
                   )
            )
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
        , button [ class "button is-primary", onClick Continue ] [ text "Continue" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ section [ class "section" ]
            [ div [ class "container" ] [ h1 [ class "title is-1 is-spaced" ] [ text "Square Roots" ] ]
            , statsView model
            , case ( model.submitStatus, model.completeStatus ) of
                ( _, Complete ) ->
                    h2 [ class "title is-2" ] [ text "Complete!" ]

                ( Nothing, Incomplete ) ->
                    problemView model

                ( Just Imprecise, _ ) ->
                    div []
                        [ problemView model
                        , h2 [ class "title is-2" ] [ text "Imprecise!" ]
                        , h4 [ class "subtitle is-4" ] [ text "There is a more precise way to simplify this problem, try again." ]
                        ]

                ( Just Correct, _ ) ->
                    div []
                        [ problemView model
                        , div []
                            [ h2 [ class "title is-2" ] [ text "Correct!" ]
                            , button [ class "button is-primary", onClick Continue ] [ text "Continue" ]
                            ]
                        ]

                ( Just Incorrect, _ ) ->
                    div []
                        [ problemView model
                        , explanationView model
                        ]
            ]
        ]
