module Main exposing (main)

{-| You do not need to touch this file. Go to `src/App.elm` to paste in examples and fiddle around
-}

import Browser
import Chart
import Chart.Attributes
import Chart.Events
import Chart.Item
import Color
import FormatNumber
import FormatNumber.Locales
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
    = LoginState
        { password : String
        , username : String
        , lastCombinationWasInvalid : Bool
        }
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
        , contact : String
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
        , feelingHistory : List FeelingObservation
        , feelingHovering :
            List
                (Chart.Item.Item
                    ( Chart.Item.One FeelingObservation Chart.Item.Any
                    , List (Chart.Item.One FeelingObservation Chart.Item.Any)
                    )
                )
        , temperatureHistory : List TemperatureObservation
        , temperatureHovering :
            List
                (Chart.Item.Item
                    ( Chart.Item.One TemperatureObservation Chart.Item.Any
                    , List (Chart.Item.One TemperatureObservation Chart.Item.Any)
                    )
                )
        , annotationHistory :
            List
                { timestamp : Time.Posix
                , comment : String
                }
        }


type alias HeartRateObservation =
    { timestamp : Time.Posix
    , beatsPerMinute : Float
    }


type alias RespiratoryRateObservation =
    { timestamp : Time.Posix
    , breathCountPerMinute : Float
    }


type alias FeelingObservation =
    { timestamp : Time.Posix
    , value : Feeling
    }


type alias TemperatureObservation =
    { timestamp : Time.Posix
    , degreesCelsius : Float
    }


type Feeling
    = FeelingBad
    | FeelingOkay
    | FeelingGood


initialState : State
initialState =
    LoginState
        { password = ""
        , username = "Mirim Zwischenmuster"
        , lastCombinationWasInvalid = False
        }


view : State -> Html State
view state =
    case state of
        LoginState loginState ->
            uiFrame []
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
                            , type_ = "text"
                            }
                            |> Html.map
                                (\newUsernameInput -> LoginState { loginState | username = newUsernameInput })
                        , -- TODO password element
                          uiInputLabelled
                            { label = "Passwort"
                            , value = loginState.password
                            , type_ = "password"
                            }
                            |> Html.map
                                (\newPasswordInput -> LoginState { loginState | password = newPasswordInput })
                        , uiButton "Login"
                            [ Html.Attributes.style "margin" "24px 0px 0px 0px"
                            ]
                            |> Html.map
                                (\() ->
                                    if loginState.password == "123" then
                                        LoginState { loginState | lastCombinationWasInvalid = True }

                                    else
                                        PatientsOverviewState { patients = patientOverviewInfoDummies }
                                )
                        , if loginState.lastCombinationWasInvalid then
                            Html.p
                                [ Html.Attributes.style "padding" "12px"
                                , Html.Attributes.style "background-color"
                                    (Color.rgb 1 0.7 0.6 |> Color.toCssString)
                                , Html.Attributes.style "border-radius" "10px"
                                ]
                                [ Html.text "Die Kombination aus Benutzername und Passwort ist ungültig"
                                ]

                          else
                            Html.text ""
                        ]
                    ]
                ]

        PatientsOverviewState patientsOverviewState ->
            uiFrame []
                [ Html.h1
                    [ Html.Attributes.style "margin-bottom" "0px" ]
                    [ Html.text "Patienten Risiko-Übersicht"
                    ]
                , Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "align-items" "start"
                    , Html.Attributes.style "gap" "10px"
                    , Html.Attributes.style "padding-bottom" "30px"
                    , Html.Attributes.style "padding-top" "0px"
                    , Html.Attributes.style "margin-top" "0px"
                    ]
                    [ Html.h4
                        [ Html.Attributes.style "margin-bottom" "0px"
                        ]
                        [ Html.text "Risiko-Legende" ]
                    , Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-direction" "row"
                        , Html.Attributes.style "align-items" "start"
                        , Html.Attributes.style "gap" "5px"
                        ]
                        [ uiRiskCircle riskGreen
                        , Html.text "keine Auffälligkeiten"
                        ]
                    , Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-direction" "row"
                        , Html.Attributes.style "align-items" "start"
                        , Html.Attributes.style "gap" "5px"
                        ]
                        [ uiRiskCircle riskYellow
                        , Html.text "wenige aber nicht eindeutige Auffälligkeiten"
                        ]
                    , Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-direction" "row"
                        , Html.Attributes.style "align-items" "start"
                        , Html.Attributes.style "gap" "5px"
                        ]
                        [ uiRiskCircle riskRed
                        , Html.text "Sepsis erkannt"
                        ]
                    ]
                , Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "justify-content" "start"
                    , Html.Attributes.style "align-items" "start"
                    , Html.Attributes.style "gap" "0px"
                    ]
                    (patientsOverviewState.patients
                        |> List.map
                            (\patient ->
                                Html.div
                                    [ Html.Attributes.style "display" "flex"
                                    , Html.Attributes.style "flex-direction" "row"
                                    , Html.Attributes.style "align-items" "center"
                                    , Html.Attributes.style "column-gap" "20px"
                                    ]
                                    [ uiRiskCircle
                                        (if patient.risk < 0.33 then
                                            riskGreen

                                         else if patient.risk < 0.66 then
                                            riskYellow

                                         else
                                            riskRed
                                        )
                                    , uiButton "Details anzeigen"
                                        []
                                        |> Html.map
                                            (\() ->
                                                case patientDetailDummies |> List.filter (\patientDetail -> patientDetail.id == patient.id) of
                                                    [] ->
                                                        PatientsOverviewState patientsOverviewState

                                                    patientDetails :: _ ->
                                                        PatientDetailsState
                                                            { id = patientDetails.id
                                                            , name = patientDetails.name
                                                            , risk = patientDetails.risk
                                                            , contact = patientDetails.contact
                                                            , heartRateHistory = patientDetails.heartRateHistory
                                                            , heartRateHovering = []
                                                            , respiratoryRateHistory = patientDetails.respiratoryRateHistory
                                                            , respiratoryRateHovering = []
                                                            , temperatureHistory = patientDetails.temperatureHistory
                                                            , temperatureHovering = []
                                                            , feelingHistory = patientDetails.feelingHistory
                                                            , feelingHovering = []
                                                            , annotationHistory = patientDetails.annotationHistory
                                                            }
                                            )
                                    , Html.p
                                        [ Html.Attributes.style "white-space" "nowrap" ]
                                        [ Html.text patient.name
                                        ]
                                    ]
                            )
                    )
                ]

        PatientDetailsState patientDetailsState ->
            uiFrame []
                [ Html.h1
                    [ Html.Attributes.style "margin-bottom" "0px" ]
                    [ Html.text "Patient Detail-Ansicht"
                    ]
                , uiButton "zur Übersicht zurückkehren"
                    [ Html.Attributes.style "margin" "0px 0px 10px 0px"
                    , Html.Attributes.style "width" "fit-content"
                    ]
                    |> Html.map
                        (\() -> PatientsOverviewState { patients = patientOverviewInfoDummies })
                , Html.p
                    []
                    [ Html.h4
                        [ Html.Attributes.style "margin-bottom" "0px"
                        ]
                        [ Html.text "Name" ]
                    , Html.text patientDetailsState.name
                    ]
                , Html.p
                    []
                    [ Html.h4
                        [ Html.Attributes.style "margin-bottom" "0px"
                        ]
                        [ Html.text "Sepsis-Risikoeinschätzung insgesamt" ]
                    , Html.text
                        ((patientDetailsState.risk
                            * 100
                            |> FormatNumber.format
                                (FormatNumber.Locales.spanishLocale
                                    |> (\l ->
                                            { l
                                                | decimals =
                                                    FormatNumber.Locales.Max 0
                                            }
                                       )
                                )
                         )
                            ++ "%"
                        )
                    ]
                , Html.p
                    []
                    [ Html.h4
                        [ Html.Attributes.style "margin-bottom" "0px"
                        ]
                        [ Html.text "Kontakt" ]
                    , Html.text
                        patientDetailsState.contact
                    ]
                , Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "justify-content" "start"
                    , Html.Attributes.style "align-items" "start"
                    , Html.Attributes.style "gap" "34px"
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
                    , uiChartFrame
                        { label = "Körpertemperatur in °C (eigene Messung) in den letzten 24 Stunden"
                        , chart =
                            Chart.series
                                (\observation -> observation.timestamp |> Time.posixToMillis |> Basics.toFloat)
                                [ Chart.interpolated .degreesCelsius [] []
                                    |> Chart.named "Körpertemperatur in °C"
                                ]
                                (patientDetailsState.temperatureHistory
                                    |> List.sortBy
                                        (\observation -> observation.timestamp |> Time.posixToMillis)
                                )
                        , goodYMin = 36.5
                        , goodYMax = 38
                        , hovering = patientDetailsState.temperatureHovering
                        }
                        |> Html.map
                            (\newHovering ->
                                PatientDetailsState
                                    { patientDetailsState
                                        | temperatureHovering = newHovering
                                    }
                            )
                    , uiChartFrame
                        { label = "Befinden (Selbsteinschätzung) in den letzten 24 Stunden"
                        , chart =
                            Chart.series
                                (\observation -> observation.timestamp |> Time.posixToMillis |> Basics.toFloat)
                                [ Chart.interpolated (\obs -> obs.value |> feelingToPercentage) [] []
                                    |> Chart.named "Befinden (prozentual)"
                                ]
                                (patientDetailsState.feelingHistory
                                    |> List.sortBy
                                        (\observation -> observation.timestamp |> Time.posixToMillis)
                                )
                        , goodYMin = 0.48
                        , goodYMax = 1
                        , hovering = patientDetailsState.feelingHovering
                        }
                        |> Html.map
                            (\newHovering ->
                                PatientDetailsState
                                    { patientDetailsState
                                        | feelingHovering = newHovering
                                    }
                            )
                    ]
                , Html.div
                    [ Html.Attributes.style "padding-top" "80px" ]
                    [ Html.label
                        [ Html.Attributes.style "font-size" "1rem"
                        ]
                        [ Html.h4 [] [ Html.text "freiform Kommentare des Patienten" ] ]
                    , Html.div
                        ([ Html.Attributes.style "display" "flex"
                         , Html.Attributes.style "flex-direction" "column"
                         , Html.Attributes.style "justify-content" "start"
                         , Html.Attributes.style "align-items" "start"
                         , Html.Attributes.style "gap" "0px"
                         , Html.Attributes.style "max-height" "400px"
                         , Html.Attributes.style "overflow-y" "scroll"
                         , Html.Attributes.style "max-width" "700px"
                         ]
                            ++ uiInputStyleBase
                        )
                        (patientDetailsState.annotationHistory
                            |> List.sortBy
                                (\annotation -> annotation.timestamp |> Time.posixToMillis)
                            |> List.map
                                (\annotation ->
                                    --Html.div [] [
                                    Html.p []
                                        [ Html.em [ Html.Attributes.style "opacity" "0.5" ]
                                            [ Html.text
                                                ("Vor "
                                                    ++ -- TODO relative time
                                                       (annotation.timestamp |> Time.toHour Time.utc |> String.fromInt)
                                                    ++ " Stunden: "
                                                )
                                            ]
                                        , Html.text
                                            annotation.comment
                                        ]
                                )
                        )
                    ]
                ]


riskGreen : Color.Color
riskGreen =
    Color.rgb 0.4 1 0.1


riskYellow : Color.Color
riskYellow =
    Color.rgb 1 0.8 0.3


riskRed : Color.Color
riskRed =
    Color.rgb 1 0.4 0.1


uiRiskCircle : Color.Color -> Html msg
uiRiskCircle color =
    Html.div
        [ Html.Attributes.style "background-color"
            (color |> Color.toCssString)
        , Html.Attributes.style "min-width" "35px"
        , Html.Attributes.style "max-width" "35px"
        , Html.Attributes.style "border-radius" "35px"
        , Html.Attributes.style "min-height" "35px"
        , Html.Attributes.style "max-height" "35px"
        ]
        [ Html.text "" ]


feelingToPercentage : Feeling -> Float
feelingToPercentage feeling =
    case feeling of
        FeelingBad ->
            0

        FeelingOkay ->
            0.5

        FeelingGood ->
            1


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
            [ Html.Attributes.style "font-size" "1rem"
            ]
            [ Html.h4
                [ Html.Attributes.style "margin-bottom" "0px"
                ]
                [ Html.text config.label ]
            ]
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
            Random.step (Random.list 6 overviewPatientDummyRandomGenerator)
                (Random.initialSeed 0)
    in
    dummies


patientDetailDummies :
    List
        { id : String
        , name : String
        , risk : Float
        , contact : String
        , heartRateHistory : List HeartRateObservation
        , respiratoryRateHistory : List RespiratoryRateObservation
        , temperatureHistory : List TemperatureObservation
        , feelingHistory : List FeelingObservation
        , annotationHistory :
            List
                { timestamp : Time.Posix
                , comment : String
                }
        }
patientDetailDummies =
    let
        ( dummies, _ ) =
            Random.step
                (Random.map
                    (\healthDataList ->
                        List.map2
                            (\patient extraPatientData ->
                                { id = patient.id
                                , name = patient.name
                                , risk = patient.risk
                                , contact = extraPatientData.context
                                , heartRateHistory = extraPatientData.heartRateHistory
                                , respiratoryRateHistory = extraPatientData.respiratoryRateHistory
                                , temperatureHistory = extraPatientData.temperatureHistory
                                , feelingHistory = extraPatientData.feelingHistory
                                , annotationHistory = extraPatientData.annotationHistory
                                }
                            )
                            patientOverviewInfoDummies
                            healthDataList
                    )
                    (Random.list 6
                        (Random.constant
                            (\contact heartRateHistory respiratoryRateHistory temperatureHistory feelingHistory annotationHistory ->
                                { context = contact
                                , heartRateHistory = heartRateHistory
                                , respiratoryRateHistory = respiratoryRateHistory
                                , temperatureHistory = temperatureHistory
                                , feelingHistory = feelingHistory
                                , annotationHistory = annotationHistory
                                }
                            )
                            |> randomAndMap
                                (Random.uniform
                                    (Random.list 10 (Random.int 0 9)
                                        |> Random.map
                                            (\digits ->
                                                digits |> List.map String.fromInt |> String.concat
                                            )
                                    )
                                    [ Random.map3
                                        (\name digits domain ->
                                            (name |> String.fromList)
                                                ++ (digits |> List.map String.fromInt |> String.concat)
                                                ++ "@"
                                                ++ domain
                                        )
                                        (Random.list 6
                                            (Random.map Char.fromCode
                                                (Random.int
                                                    ('a' |> Char.toCode)
                                                    ('z' |> Char.toCode)
                                                )
                                            )
                                        )
                                        (Random.list 4 (Random.int 0 9))
                                        (Random.uniform "web.de" [ "gmail.com", "gmx.de", "outlook.com" ])
                                    ]
                                    |> Random.andThen identity
                                )
                            |> randomAndMap
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
                            |> randomAndMap
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
                            |> randomAndMap
                                (Random.list 6
                                    (Random.map2
                                        (\millis degreesCelsius ->
                                            { timestamp = Time.millisToPosix millis
                                            , degreesCelsius = degreesCelsius
                                            }
                                        )
                                        (Random.int 0 (24 * 60 * 60 * 1000))
                                        (Random.float 36.5 38.8)
                                    )
                                )
                            |> randomAndMap
                                (Random.list 6
                                    (Random.map2
                                        (\millis value ->
                                            { timestamp = Time.millisToPosix millis
                                            , value = value
                                            }
                                        )
                                        (Random.int 0 (24 * 60 * 60 * 1000))
                                        (Random.uniform FeelingBad [ FeelingOkay, FeelingGood ])
                                    )
                                )
                            |> randomAndMap
                                (Random.list 20
                                    (Random.map2
                                        (\millis comment ->
                                            { timestamp = Time.millisToPosix millis
                                            , comment = comment
                                            }
                                        )
                                        (Random.int 0 (24 * 60 * 60 * 1000))
                                        (Random.uniform
                                            "Ich habe Beschwerden"
                                            [ "Bitte neuen Termin"
                                            , "Verfärbung im Lungenbereich"
                                            , "Herzprobleme"
                                            , "Schwäche und Müdigkeit"
                                            , "Kopfschmerzen"
                                            , "Kann Pillen nicht finden"
                                            , "Wie sendet man die Daten?"
                                            , "Passwort123456"
                                            , "Mein Name ist Liselotte"
                                            , "Ich habe in ihrer Praxis mein Gebiss vergessen"
                                            , "Mein Herz beruhigt sich nicht. Was soll ich machen?"
                                            , "Die Handystrahlen sind böse sagt feßbock"
                                            ]
                                        )
                                    )
                                )
                        )
                    )
                )
                (Random.initialSeed 0)
    in
    dummies


randomAndMap =
    Random.map2 (|>)


overviewPatientDummyRandomGenerator : Random.Generator { id : String, name : String, risk : Float }
overviewPatientDummyRandomGenerator =
    Random.map3 (\id name risk -> { id = id, name = name, risk = risk })
        (Random.map String.fromList
            (Random.list 16 (Random.map Char.fromCode (Random.int 0 9)))
        )
        (Random.uniform "Ella"
            [ "Mina Berger"
            , "Alex Cena"
            , "Fred Wolf"
            , "Michael Steinmacher"
            , "Juni Gestern"
            , "Sabine Zeitler"
            , "Erik Mätzner"
            , "Angelika Senner"
            , "Riko Gaus"
            , "Nuhr Alavik"
            , "Andreas Kleber"
            ]
        )
        (Random.float 0 1)


uiFrame : List (Html.Attribute future) -> List (Html future) -> Html future
uiFrame additionalModifiers subs =
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
    , Html.Attributes.style "border"
        ("solid " ++ (Color.rgb 0 0 0 |> Color.toCssString))
    , Html.Attributes.style "border-width" "2px 2px 2px 2px"
    , Html.Attributes.style "border-radius" "8px"
    ]


uiInputLabelled :
    { label : String
    , value : String
    , type_ : String
    }
    -> Html String
uiInputLabelled config =
    Html.div
        []
        [ Html.label
            [ Html.Attributes.style "margin-bottom" "7px"
            , Html.Attributes.style "font-size" "1rem"
            ]
            [ Html.text config.label ]
        , Html.input
            (uiInputStyleBase
                ++ [ Html.Attributes.value config.value
                   , Html.Attributes.type_ config.type_
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
