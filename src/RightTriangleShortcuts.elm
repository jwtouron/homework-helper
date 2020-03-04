module RightTriangleShortcuts exposing (main)

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


type Angle
    = FourtyFive
    | ThirtySixty


type Leg
    = SL
    | LL
    | H


type alias Problem =
    { angle : Angle
    , length : Float
    , knownLeg : Leg
    , wantedLeg : Leg
    }


new454590 : Random.Generator Problem
new454590 =
    -- H = Leg * sqrt(2)
    Random.map2
        (\leg x ->
            let
                problem =
                    { angle = FourtyFive, length = x, knownLeg = SL, wantedLeg = SL }
            in
            case leg of
                SL ->
                    { problem
                        | knownLeg = SL
                        , wantedLeg = H
                    }

                LL ->
                    { problem
                        | knownLeg = LL
                        , wantedLeg = H
                    }

                H ->
                    Random.andThen
                        (\leg2 ->
                            Random.constant
                                { problem
                                    | knownLeg = H
                                    , wantedLeg = leg2
                                }
                        )
                        (Random.uniform SL [ LL ])
        )
        (Random.uniform SL [ LL, H ])
        (Random.uniform 1 [ 2, 3, 4, 5, 6, 7, 8, 9, 10 ])


new306090 : Random.Generator Problem
new306090 =
    Random.map3
        (\typ want x ->
            let
                problem =
                    { angle = ThirtySixty, length = x, knownLeg = SL, wantedLeg = SL }
            in
            case ( typ, want ) of
                ( 0, 0 ) ->
                    { problem | knownLeg = LL, wantedLeg = SL }

                ( 0, 1 ) ->
                    { problem | knownLeg = SL, wantedLeg = LL }

                ( 1, 0 ) ->
                    { problem | knownLeg = H, wantedLeg = SL }

                ( 1, 1 ) ->
                    { problem | knownLeg = SL, wantedLeg = H }
        )
        (Random.uniform 0 [ 1 ])
        -- 0 => LL = SL * sqrt(3), 1 => H = 2 * SL
        (Random.uniform 0 [ 1 ])
        -- 0 => want SL, 1 => want other
        (Random.uniform 1 [ 2, 3, 4, 5, 6, 7, 8, 9, 10 ])


newProblem : Random.Generator Problem
newProblem =
    Random.andThen
        (\angle ->
            case angle of
                FourtyFive ->
                    new454590

                ThirtySixty ->
                    new306090
        )
        (Random.uniform FourtyFive [ ThirtySixty ])


type alias Model =
    { angle : Angle
    , problem : Problem
    }


init : () -> ( HH.Model Model, Cmd (HH.Msg Msg) )
init _ =
    ( HH.newModel "Right Triangle Shortcuts"
        { angle = ThirtySixty
        , problem = { angle = FourtyFive, length = 0, knownLeg = SL, wantedLeg = SL }
        }
    , Cmd.map HH.SubMsg (Random.generate NewProblem newProblem)
    )



-- Update


type Msg
    = NewProblem Problem
    | NoOp


checkSubmission : Model -> HH.SubmitStatus
checkSubmission model =
    HH.Correct


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewProblem problem ->
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
