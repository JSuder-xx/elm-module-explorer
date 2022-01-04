port module Main exposing (main)

import ModuleUML exposing (ModuleUML)
import Platform


port sendModule : ModuleUML -> Cmd msg


port onModuleString : (String -> msg) -> Sub msg


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = SendModule ModuleUML
    | BadModuleParse


main : Platform.Program Flags Model Msg
main =
    Platform.worker
        { init = always <| ( (), Cmd.none )
        , update =
            \msg _ ->
                ( ()
                , case msg of
                    SendModule moduleUML ->
                        sendModule moduleUML

                    BadModuleParse ->
                        Cmd.none
                )
        , subscriptions =
            always <|
                onModuleString
                    (ModuleUML.fromString
                        >> Maybe.map SendModule
                        >> Maybe.withDefault BadModuleParse
                    )
        }
