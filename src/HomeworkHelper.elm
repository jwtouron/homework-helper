module HomeworkHelper exposing
    ( CompleteStatus(..)
    , Model
    , Msg(..)
    , SubmitStatus(..)
    , maybeSubMsg
    , newModel
    , onKeyDown
    , performSubmit
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Round
import Task


type alias Model a =
    { title : String
    , attempted : Int
    , successful : Int
    , submitStatus : Maybe SubmitStatus
    , completeStatus : CompleteStatus
    , subModel : a
    }


newModel : String -> a -> Model a
newModel title subModel =
    { title = title
    , attempted = 0
    , successful = 0
    , submitStatus = Nothing
    , completeStatus = Incomplete
    , subModel = subModel
    }


type Msg m
    = Continue
    | Submit
    | SubMsg m


type SubmitStatus
    = Correct
    | Incorrect
    | Imprecise


type CompleteStatus
    = Complete
    | Incomplete


maybeSubMsg : m -> Msg m -> m
maybeSubMsg def m =
    case m of
        SubMsg m2 ->
            m2

        _ ->
            def



-- update


submit : (subModel -> SubmitStatus) -> Model subModel -> ( Model subModel, Cmd (Msg msg) )
submit checkSubmission model =
    let
        submitStatus =
            checkSubmission model.subModel

        model2 =
            { model | submitStatus = Just submitStatus, attempted = model.attempted + 1 }
    in
    case submitStatus of
        Correct ->
            let
                model3 =
                    { model2 | successful = model2.successful + 1 }
            in
            if isComplete model3 then
                ( { model3 | completeStatus = Complete }, Cmd.none )

            else
                ( model3, Cmd.none )

        Incorrect ->
            ( model2, Cmd.none )

        Imprecise ->
            ( { model2 | attempted = model2.attempted - 1 }, Cmd.none )


performSubmit : m -> Cmd m
performSubmit def =
    Task.perform (\_ -> Submit) (Task.succeed ()) |> Cmd.map (maybeSubMsg def)


isComplete : Model m -> Bool
isComplete model =
    toFloat model.successful
        / toFloat model.attempted
        >= 0.9
        && toFloat model.attempted
        >= 10


update :
    (subModel -> ( subModel, Cmd subMsg ))
    -> (subModel -> SubmitStatus)
    -> (msg -> subModel -> ( subModel, Cmd subMsg ))
    -> Msg msg
    -> Model subModel
    -> ( Model subModel, Cmd (Msg subMsg) )
update continue checkSubmission subUpdate msg model =
    case msg of
        Continue ->
            let
                ( subModel, cmd ) =
                    continue model.subModel
            in
            ( { model
                | completeStatus = Incomplete
                , submitStatus = Nothing
                , subModel = subModel
              }
            , Cmd.map SubMsg cmd
            )

        Submit ->
            submit checkSubmission model

        SubMsg m ->
            let
                ( subModel, cmd ) =
                    subUpdate m model.subModel
            in
            ( { model | subModel = subModel }, Cmd.map SubMsg cmd )



-- view


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


statsView : Model a -> Html (Msg m)
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


submitButton : Html (Msg msg)
submitButton =
    button
        [ class "button is-primary"
        , onClick Submit

        -- , disabled (shouldDisableInput model)
        ]
        [ text "Submit" ]


view : (Bool -> a -> Html m) -> (a -> Html m) -> Model a -> Html (Msg m)
view problemView explanationView model =
    div []
        [ section [ class "section" ]
            [ h1 [ class "title is-1 is-spaced" ] [ text model.title ]
            , statsView model
            , case ( model.submitStatus, model.completeStatus ) of
                ( _, Complete ) ->
                    h2 [ class "title is-2" ] [ text "Complete!" ]

                ( Nothing, Incomplete ) ->
                    div []
                        [ problemView False model.subModel |> Html.map SubMsg
                        , submitButton
                        ]

                ( Just Imprecise, Incomplete ) ->
                    div []
                        [ problemView False model.subModel |> Html.map SubMsg
                        , h2 [ class "title is-2" ] [ text "Imprecise!" ]
                        , h4 [ class "subtitle is-4" ]
                            [ text "There is a more precise way to simplify this problem, try again." ]
                        , submitButton
                        ]

                ( Just Correct, Incomplete ) ->
                    div []
                        [ problemView True model.subModel |> Html.map SubMsg
                        , div []
                            [ h2 [ class "title is-2" ] [ text "Correct!" ]
                            , button
                                [ class "button is-primary"
                                , onClick Continue
                                ]
                                [ text "Continue" ]
                            ]
                        ]

                ( Just Incorrect, Incomplete ) ->
                    div []
                        [ problemView True model.subModel |> Html.map SubMsg
                        , explanationView model.subModel |> Html.map SubMsg
                        , button [ class "button is-primary", onClick Continue ] [ text "Continue" ]
                        ]
            ]
        ]
