module SquareRoots exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe
import Random


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type Status
    = Complete
    | Incomplete
    | Correct
    | Incorrect


type alias Model =
    { firstNum : Int
    , secondNum : Int
    , status : Status
    , firstInput : String
    , secondInput : String
    , attempted : Int
    , successful : Int
    }


newNums : Random.Generator ( Int, Int )
newNums =
    Random.map2 (\a b -> ( a, b ))
        (Random.uniform 1 [ 2, 3, 4, 5, 6 ])
        (Random.uniform 1 [ 2, 3, 5, 7 ])


init : () -> ( Model, Cmd Msg )
init _ =
    ( { firstNum = 0
      , secondNum = 0
      , status = Incomplete
      , firstInput = ""
      , secondInput = ""
      , attempted = 0
      , successful = 0
      }
    , Random.generate RandNums newNums
    )



-- Update


type Msg
    = RandNums ( Int, Int )
    | Submit
    | ChangeFirst String
    | ChangeSecond String
    | Continue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandNums ( x, y ) ->
            ( { model | firstNum = x, secondNum = y }, Cmd.none )

        Submit ->
            if
                Maybe.withDefault 1 (String.toInt model.firstInput)
                    == model.firstNum
                    && Maybe.withDefault 1 (String.toInt model.secondInput)
                    == model.secondNum
            then
                let
                    model2 =
                        { model
                            | attempted = model.attempted + 1
                            , successful = model.successful + 1
                            , status = Correct
                        }

                    status2 =
                        if
                            toFloat model2.successful
                                / toFloat model2.attempted
                                >= 0.9
                                && model2.attempted
                                >= 10
                        then
                            Complete

                        else
                            Correct
                in
                ( { model2 | status = status2 }, Cmd.none )

            else
                ( { model | attempted = model.attempted + 1, status = Incorrect }, Cmd.none )

        ChangeFirst x ->
            ( { model | firstInput = x }, Cmd.none )

        ChangeSecond x ->
            ( { model | secondInput = x }, Cmd.none )

        Continue ->
            ( { model | status = Incomplete, firstInput = "", secondInput = "" }
            , Random.generate RandNums newNums
            )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


showProblem : Model -> List (Html Msg)
showProblem model =
    [ h2 [] [ text "Simplify the following:" ]
    , h3 [] [ text ("√" ++ String.fromInt (model.firstNum * model.firstNum * model.secondNum)) ]
    , span []
        [ label [] [ text "Answer:" ]
        , input [ value model.firstInput, onInput ChangeFirst, disabled (model.status /= Incomplete) ] []
        , label [] [ text "√" ]
        , input [ value model.secondInput, onInput ChangeSecond, disabled (model.status /= Incomplete) ] []
        , button [ onClick Submit, disabled (model.status /= Incomplete) ] [ text "Submit" ]
        ]
    ]


view : Model -> Html Msg
view model =
    div []
        ([ h1 [] [ text "Square Roots" ]
         , h2 []
            [ text
                ("Attempted: "
                    ++ String.fromInt model.attempted
                    ++ " Successful: "
                    ++ String.fromInt model.successful
                    ++ " ("
                    ++ (if model.attempted == 0 then
                            "0%)"

                        else
                            String.fromFloat (toFloat model.successful / toFloat model.attempted * 100)
                                ++ "%)"
                       )
                )
            ]
         ]
            ++ (case model.status of
                    Complete ->
                        [ h2 [] [ text "Complete!" ] ]

                    Incomplete ->
                        showProblem model

                    Correct ->
                        showProblem model
                            ++ [ div []
                                    [ h2 [] [ text "Correct!" ]
                                    , button [ onClick Continue ] [ text "Continue" ]
                                    ]
                               ]

                    Incorrect ->
                        showProblem model
                            ++ [ div []
                                    [ h2 [] [ text "Incorrect!" ]
                                    , text ("√" ++ String.fromInt (model.firstNum * model.firstNum * model.secondNum))
                                    , div []
                                        [ text
                                            ("= √("
                                                ++ String.fromInt (model.firstNum * model.firstNum)
                                                ++ " * "
                                                ++ String.fromInt model.secondNum
                                                ++ ")"
                                            )
                                        ]
                                    , div []
                                        [ text
                                            ("= √"
                                                ++ String.fromInt (model.firstNum * model.firstNum)
                                                ++ " * √"
                                                ++ String.fromInt model.secondNum
                                            )
                                        ]
                                    , div []
                                        [ text
                                            ("= "
                                                ++ String.fromInt model.firstNum
                                                ++ " * √"
                                                ++ String.fromInt model.secondNum
                                            )
                                        ]
                                    , h2 [] []
                                    , button [ onClick Continue ] [ text "Continue" ]
                                    ]
                               ]
               )
        )
