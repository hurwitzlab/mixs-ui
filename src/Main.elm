module Main exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Tab as Tab
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


---- MODEL ----


type alias Model =
    { tabState : Tab.State
    , projectName : String
    , investigationType : String
    , submittedToInsdc : Bool
    , latitude : Maybe Float
    , longitude : Maybe Float
    }


initialModel : Model
initialModel =
    { tabState = Tab.initialState
    , projectName = "Unnamed Project"
    , investigationType = "Unknown"
    , submittedToInsdc = False
    , latitude = Nothing
    , longitude = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateInvestigationType String
    | UpdateProjectName String
    | ToggleSubmittedToInsdc
    | TabMsg Tab.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TabMsg state ->
            ( { model | tabState = state }, Cmd.none )

        UpdateInvestigationType newType ->
            ( { model | investigationType = newType }, Cmd.none )

        UpdateProjectName newName ->
            ( { model | projectName = newName }, Cmd.none )

        ToggleSubmittedToInsdc ->
            ( { model | submittedToInsdc = not model.submittedToInsdc }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    Grid.container []
        [ h1 [] [ text "MIxS UX" ]
        , Tab.config TabMsg
            |> Tab.withAnimation
            |> Tab.right
            |> Tab.items
                [ Tab.item
                    { id = "tabInv"
                    , link = Tab.link [] [ text "Investigation" ]
                    , pane =
                        Tab.pane []
                            [ br [] []
                            , paneInvestigation model
                            ]
                    }
                , Tab.item
                    { id = "tabEnv"
                    , link = Tab.link [] [ text "Environment" ]
                    , pane =
                        Tab.pane []
                            [ br [] []
                            , paneEnv model
                            ]
                    }
                ]
            |> Tab.view model.tabState
        ]


paneInvestigation : Model -> Html Msg
paneInvestigation model =
    table []
        [ mkRowTextEntry "Project Name" model.projectName UpdateProjectName
        , radioInvestigationType model.investigationType
        , mkRowCheckbox
            "Submitted to INSDC"
            model.submittedToInsdc
            ToggleSubmittedToInsdc
        ]


paneEnv : Model -> Html Msg
paneEnv model =
    table []
        [ tr []
            [ th [] [ text "Latitude" ]
            , td [] [ input [ value (toString model.latitude) ] [] ]
            ]
        , tr []
            [ th [] [ text "Longitude" ]
            , td [] [ input [ value (toString model.longitude) ] [] ]
            ]
        ]


radioInvestigationType : String -> Html Msg
radioInvestigationType currentType =
    let
        types =
            [ "Eukaryote"
            , "BacteriaArchaea"
            , "Plasmid"
            , "Virus"
            , "Organelle"
            , "Metagenome"
            , "MimarksSurvey"
            , "MimarksSpecimen"
            ]
    in
    mkRowSelect "Investigation Type" types currentType UpdateInvestigationType


mkTh : String -> Html msg
mkTh label =
    th [ style [ ( "align", "right" ) ] ] [ text label ]


mkRowCheckbox : String -> Bool -> Msg -> Html Msg
mkRowCheckbox label state msg =
    tr []
        [ mkTh label
        , td []
            [ input
                [ type_ "checkbox"
                , onClick msg
                , checked state
                , class "form-control"
                ]
                []
            ]
        ]


mkRowTextEntry : String -> String -> (String -> Msg) -> Html Msg
mkRowTextEntry label defValue msg =
    tr []
        [ mkTh label
        , td []
            [ input
                [ type_ "text"
                , defaultValue defValue
                , class "form-control"
                , onInput msg
                , size 60
                ]
                []
            ]
        ]


mkRowSelect : String -> List String -> String -> (String -> Msg) -> Html Msg
mkRowSelect label optList curOpt msg =
    let
        mkOption val =
            option [ value val, selected (val == curOpt) ] [ text val ]
    in
    tr []
        [ mkTh label
        , td []
            [ select [ onInput msg ] (List.map mkOption optList) ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Tab.subscriptions model.tabState TabMsg
