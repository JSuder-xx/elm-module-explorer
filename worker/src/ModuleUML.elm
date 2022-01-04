module ModuleUML exposing (ModuleUML, fromFile, fromString)

import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing as Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Function
import TypeUML exposing (TypeUML)


type alias ModuleUML =
    { name : String
    , types : List TypeUML
    }


type alias ModuleUMLState =
    { name : String
    , declaredTypes : Dict String TypeUML
    }


addFunctionToTypes : Node Declaration -> ModuleUMLState -> ModuleUMLState
addFunctionToTypes (Node _ declaration) moduleState =
    declaration
        |> Function.fromDeclaration
        |> Maybe.map
            (\function ->
                { moduleState | declaredTypes = (function |> TypeUML.addFunction |> Dict.map) moduleState.declaredTypes }
            )
        |> Maybe.withDefault moduleState


fromFile : File -> ModuleUML
fromFile { moduleDefinition, declarations } =
    let
        typeNameNode : Node Declaration -> Maybe (Node String)
        typeNameNode (Node _ declaration) =
            case declaration of
                AliasDeclaration alias ->
                    Just alias.name

                CustomTypeDeclaration custom ->
                    Just custom.name

                _ ->
                    Nothing

        initialModuleState : ModuleUMLState
        initialModuleState =
            { name = moduleDefinition |> Node.value |> Module.moduleName |> String.join "."
            , declaredTypes =
                declarations
                    |> List.filterMap typeNameNode
                    |> List.foldl
                        (\(Node range typeName) -> Dict.insert typeName <| TypeUML.init typeName range)
                        Dict.empty
            }

        moduleAfterFunctions =
            declarations |> List.foldl addFunctionToTypes initialModuleState
    in
    { name = moduleAfterFunctions.name
    , types = Dict.values moduleAfterFunctions.declaredTypes
    }


fromString : String -> Maybe ModuleUML
fromString =
    Elm.Parser.parse
        >> Result.toMaybe
        >> Maybe.map (Processing.process Processing.init >> fromFile)
