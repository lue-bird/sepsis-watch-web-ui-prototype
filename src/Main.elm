module Main exposing (main)

{-| You do not need to touch this file. Go to `src/App.elm` to paste in examples and fiddle around
-}

import Browser
import Chart
import Chart.Attributes
import Chart.Events
import Chart.Item
import Color
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Random
import Svg
import Svg.Attributes
import Time


type State
    = LoginState { password : String, username : String }
    | PatientsOverviewState
        { patients :
            List
                { id : String
                , name : String
                , risk : Float
                }
        }
    | PatientDetailsState
        { id : String
        , name : String
        , risk : Float
        , heartRateHistory : List HeartRateObservation
        , heartRateHovering :
            List
                (Chart.Item.Item
                    ( Chart.Item.One HeartRateObservation Chart.Item.Any
                    , List (Chart.Item.One HeartRateObservation Chart.Item.Any)
                    )
                )
        , respiratoryRateHistory : List RespiratoryRateObservation
        , respiratoryRateHovering :
            List
                (Chart.Item.Item
                    ( Chart.Item.One RespiratoryRateObservation Chart.Item.Any
                    , List (Chart.Item.One RespiratoryRateObservation Chart.Item.Any)
                    )
                )
        }


type alias HeartRateObservation =
    { timestamp : Time.Posix
    , beatsPerMinute : Float
    }


type alias RespiratoryRateObservation =
    { timestamp : Time.Posix
    , breathCountPerMinute : Float
    }


initialState : State
initialState =
    LoginState { password = "", username = "" }


view : State -> Html State
view state =
    case state of
        LoginState loginState ->
            htmlFrame []
                [ Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.style "align-items" "center"
                    ]
                    [ Html.h1
                        []
                        [ Html.text "Sepsis Watch"
                        ]
                    , Html.div
                        [ Html.Attributes.style "max-width" "500px" ]
                        [ uiInputLabelled
                            { label = "Benutzername"
                            , value = loginState.username
                            }
                            |> Html.map
                                (\newUsernameInput -> LoginState { loginState | username = newUsernameInput })
                        , -- TODO password element
                          uiInputLabelled
                            { label = "Passwort"
                            , value = loginState.password
                            }
                            |> Html.map
                                (\newPasswordInput -> LoginState { loginState | password = newPasswordInput })
                        , -- TODO check (dummy)
                          -- if (loginState.username /= "Max Muster") || (loginState.password /= "1234") then
                          --   Web.domBoolProperty "disabled" True
                          --
                          -- else
                          uiButton "Login"
                            [ Html.Attributes.style "margin" "24px 0px 0px 0px"
                            ]
                            |> Html.map
                                (\() -> PatientsOverviewState { patients = patientOverviewInfoDummies })
                        ]
                    ]
                ]

        PatientsOverviewState patientsOverviewState ->
            htmlFrame []
                [ Html.h1
                    []
                    [ Html.text "Patienten Risiko-Übersicht"
                    ]
                , Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "justify-content" "start"
                    , Html.Attributes.style "align-items" "start"
                    ]
                    (patientsOverviewState.patients
                        |> List.map
                            (\patient ->
                                Html.div
                                    [ Html.Attributes.style "display" "flex"
                                    , Html.Attributes.style "flex-direction" "row"
                                    , Html.Attributes.style "justify-content" "center"
                                    , Html.Attributes.style "align-items" "center"
                                    , Html.Attributes.style "gap" "20px"
                                    ]
                                    [ Html.div
                                        [ Html.Attributes.style "background-color"
                                            (if patient.risk < 0.33 then
                                                "green"

                                             else if patient.risk < 0.66 then
                                                Color.rgb255 200 200 20 |> Color.toCssString

                                             else
                                                "red"
                                            )
                                        , Html.Attributes.style "min-width" "35px"
                                        , Html.Attributes.style "max-width" "35px"
                                        , Html.Attributes.style "border-radius" "35px"
                                        , Html.Attributes.style "min-height" "35px"
                                        , Html.Attributes.style "max-height" "35px"
                                        ]
                                        [ Html.text "" ]
                                    , uiButton "Details anzeigen" []
                                        |> Html.map
                                            (\() ->
                                                case patientDetailDummies |> List.filter (\patientDetail -> patientDetail.id == patient.id) of
                                                    [] ->
                                                        let
                                                            _ =
                                                                Debug.log "patientDetailDummies not matching id"
                                                        in
                                                        PatientsOverviewState patientsOverviewState

                                                    patientDetails :: _ ->
                                                        PatientDetailsState
                                                            { id = patientDetails.id
                                                            , name = patientDetails.name
                                                            , risk = patientDetails.risk
                                                            , heartRateHistory = patientDetails.heartRateHistory
                                                            , heartRateHovering = []
                                                            , respiratoryRateHistory = patientDetails.respiratoryRateHistory
                                                            , respiratoryRateHovering = []
                                                            }
                                            )
                                    , Html.p
                                        []
                                        [ Html.text patient.name
                                        ]
                                    ]
                            )
                    )
                ]

        PatientDetailsState patientDetailsState ->
            htmlFrame []
                [ Html.h1
                    [ Html.Attributes.style "margin-bottom" "0px" ]
                    [ Html.text "Patient Detail-Ansicht"
                    ]
                , uiButton "zur Übersicht zurückkehren"
                    [ Html.Attributes.style "margin" "0px 0px 40px 0px"
                    , Html.Attributes.style "width" "fit-content"
                    ]
                    |> Html.map
                        (\() -> PatientsOverviewState { patients = patientOverviewInfoDummies })
                , Html.p
                    []
                    [ Html.text ("Name: " ++ patientDetailsState.name)
                    ]
                , Html.p
                    []
                    [ Html.text ("Sepsis-Risikoeinschätzung insgesamt: " ++ (patientDetailsState.risk |> String.fromFloat))
                    ]
                , Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "justify-content" "start"
                    , Html.Attributes.style "align-items" "start"
                    , Html.Attributes.style "gap" "40px"
                    ]
                    [ uiChartFrame
                        { label = "Herzschläge/Minute in den letzten 6 Stunden"
                        , chart =
                            Chart.series
                                (\observation -> observation.timestamp |> Time.posixToMillis |> Basics.toFloat)
                                [ Chart.interpolated .beatsPerMinute [] []
                                    |> Chart.named "Herzschläge/Minute"
                                ]
                                (patientDetailsState.heartRateHistory
                                    |> List.sortBy
                                        (\observation -> observation.timestamp |> Time.posixToMillis)
                                )
                        , goodYMin = 65
                        , goodYMax = 100
                        , hovering = patientDetailsState.heartRateHovering
                        }
                        |> Html.map
                            (\newHovering ->
                                PatientDetailsState
                                    { patientDetailsState
                                        | heartRateHovering = newHovering
                                    }
                            )
                    , uiChartFrame
                        { label = "Atemzüge/Minute in den letzten 6 Stunden"
                        , chart =
                            Chart.series
                                (\observation -> observation.timestamp |> Time.posixToMillis |> Basics.toFloat)
                                [ Chart.interpolated .breathCountPerMinute [] []
                                    |> Chart.named "Atemzüge/Minute"
                                ]
                                (patientDetailsState.respiratoryRateHistory
                                    |> List.sortBy
                                        (\observation -> observation.timestamp |> Time.posixToMillis)
                                )
                        , goodYMin = 21
                        , goodYMax = 26
                        , hovering = patientDetailsState.respiratoryRateHovering
                        }
                        |> Html.map
                            (\newHovering ->
                                PatientDetailsState
                                    { patientDetailsState
                                        | respiratoryRateHovering = newHovering
                                    }
                            )
                    ]
                ]


uiChartFrame :
    { label : String
    , hovering : List (Chart.Item.Item b)
    , chart :
        Chart.Element
            data
            (List
                (Chart.Item.Item
                    ( Chart.Item.One data Chart.Item.Any
                    , List (Chart.Item.One data Chart.Item.Any)
                    )
                )
            )
    , goodYMin : Float
    , goodYMax : Float
    }
    ->
        Html
            (List
                (Chart.Item.Item
                    ( Chart.Item.One data Chart.Item.Any
                    , List (Chart.Item.One data Chart.Item.Any)
                    )
                )
            )
uiChartFrame config =
    Html.div
        []
        [ Html.label
            [ Html.Attributes.style "margin-bottom" "17px"
            , Html.Attributes.style "font-size" "1rem"
            ]
            [ Html.text config.label ]
        , Html.div
            [ Html.Attributes.style "max-width" "500px"
            , Html.Attributes.style "min-width" "500px"
            , Html.Attributes.style "padding" "20px 50px 20px 50px"
            ]
            [ Chart.chart
                [ Chart.Attributes.width 500
                , Chart.Attributes.height 400
                , Chart.Events.onMouseMove identity (Chart.Events.getNearest Chart.Item.bins)
                , Chart.Events.onMouseLeave []
                ]
                [ Chart.withPlane <|
                    \p ->
                        [ Chart.rect
                            [ Chart.Attributes.x1 p.x.min
                            , Chart.Attributes.y1 config.goodYMin
                            , Chart.Attributes.x2 p.x.max
                            , Chart.Attributes.y2 config.goodYMax
                            , Chart.Attributes.opacity 0.1
                            , Chart.Attributes.border "none"
                            , Chart.Attributes.color Chart.Attributes.green
                            ]
                        ]
                , Chart.labelAt Chart.Attributes.middle
                    .min
                    [ Chart.Attributes.moveUp 10 ]
                    [ Svg.text "Zeitabstand bis jetzt" ]
                , Chart.grid
                    [ Chart.Attributes.color
                        (Color.black |> Color.toCssString)
                    ]
                , Chart.xTicks
                    [ Chart.Attributes.times Time.utc
                    , Chart.Attributes.noGrid
                    ]
                , Chart.yTicks
                    [ Chart.Attributes.withGrid
                    ]
                , Chart.xLabels
                    [ Chart.Attributes.times Time.utc
                    , Chart.Attributes.color (Color.black |> Color.toCssString)
                    , Chart.Attributes.moveDown 5
                    ]
                , Chart.yLabels [ Chart.Attributes.color (Color.black |> Color.toCssString) ]
                , Chart.xAxis [ Chart.Attributes.color (Color.black |> Color.toCssString) ]
                , Chart.yAxis [ Chart.Attributes.color (Color.black |> Color.toCssString) ]
                , config.chart
                , Chart.each config.hovering
                    (\_ item ->
                        [ Chart.tooltip item [] [] [] ]
                    )
                ]
            ]
        ]


uiButton : String -> List (Html.Attribute ()) -> Html ()
uiButton label additionalModifiers =
    Html.button
        (uiInputStyleBase
            ++ additionalModifiers
            ++ [ Html.Attributes.style "padding" "4px 12px"
            , Html.Attributes.style "border-radius" "100px"
            , Html.Events.on "pointerdown" (Json.Decode.succeed ())
            ]
        )
        [ Html.text label ]


patientOverviewInfoDummies : List { id : String, name : String, risk : Float }
patientOverviewInfoDummies =
    let
        ( dummies, _ ) =
            Random.step (Random.list 6 overviewPatientDummyRandomGenerator) (Random.initialSeed 0)
    in
    dummies


patientDetailDummies :
    List
        { id : String
        , name : String
        , risk : Float
        , heartRateHistory : List HeartRateObservation
        , respiratoryRateHistory : List RespiratoryRateObservation
        }
patientDetailDummies =
    let
        ( dummies, _ ) =
            Random.step
                (Random.map
                    (\healthDataList ->
                        List.map2
                            (\patient healthData ->
                                { id = patient.id
                                , name = patient.name
                                , risk = patient.risk
                                , heartRateHistory = healthData.heartRateHistory
                                , respiratoryRateHistory = healthData.respiratoryRateHistory
                                }
                            )
                            patientOverviewInfoDummies
                            healthDataList
                    )
                    (Random.list 6
                        (Random.map2
                            (\heartRateHistory respiratoryRateHistory ->
                                { heartRateHistory = heartRateHistory
                                , respiratoryRateHistory = respiratoryRateHistory
                                }
                            )
                            (Random.list 200
                                (Random.map2
                                    (\millis beatsPerMinute ->
                                        { timestamp = Time.millisToPosix millis
                                        , beatsPerMinute = beatsPerMinute
                                        }
                                    )
                                    (Random.int 0 (6 * 60 * 60 * 1000))
                                    (Random.float 60 105)
                                )
                            )
                            (Random.list 200
                                (Random.map2
                                    (\millis breathCountPerMinute ->
                                        { timestamp = Time.millisToPosix millis
                                        , breathCountPerMinute = breathCountPerMinute
                                        }
                                    )
                                    (Random.int 0 (6 * 60 * 60 * 1000))
                                    (Random.float 18 28)
                                )
                            )
                        )
                    )
                )
                (Random.initialSeed 0)
    in
    dummies


overviewPatientDummyRandomGenerator : Random.Generator { id : String, name : String, risk : Float }
overviewPatientDummyRandomGenerator =
    Random.map3 (\id name risk -> { id = id, name = name, risk = risk })
        (Random.map String.fromList
            (Random.list 16 (Random.map Char.fromCode (Random.int 0 9)))
        )
        (Random.uniform "Ella" [ "Mina", "Alex", "Fred", "Michael", "Juni", "Sabine", "Erik" ])
        (Random.float 0 1)


htmlFrame : List (Html.Attribute future) -> List (Html future) -> Html future
htmlFrame additionalModifiers subs =
    Html.div
        ([ Html.Attributes.style "padding" "40px 120px 40px 120px"
         ]
            ++ additionalModifiers
        )
        subs


uiInputStyleBase : List (Html.Attribute future_)
uiInputStyleBase =
    [ Html.Attributes.style "box-sizing" "border-box"
    , Html.Attributes.style "width" "100%"
    , Html.Attributes.style "padding" "8px 16px"
    , Html.Attributes.style "font-size" "1rem"
    , Html.Attributes.style "background-color"
        (Color.rgb 1 1 1 |> Color.toCssString)
    ]


uiInputLabelled :
    { label : String
    , value : String
    }
    -> Html String
uiInputLabelled dateInputConfig =
    Html.div
        []
        [ Html.label
            [ Html.Attributes.style "margin-bottom" "7px"
            , Html.Attributes.style "font-size" "1rem"
            ]
            [ Html.text dateInputConfig.label ]
        , Html.input
            (uiInputStyleBase
                ++ [ Html.Attributes.value dateInputConfig.value
                , Html.Attributes.type_ "text"
                , uiInputChangeListen
                ]
            )
            []
        ]


uiInputChangeListen : Html.Attribute String
uiInputChangeListen =
    Html.Events.on "input"
        (Json.Decode.field "target" (Json.Decode.field "value" Json.Decode.string))


main : Platform.Program () State State
main =
    Browser.element
        { init = \() -> ( initialState, Cmd.none )
        , view = view
        , update = \newState _ -> ( newState, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
