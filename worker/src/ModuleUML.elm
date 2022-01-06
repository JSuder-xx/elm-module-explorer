module ModuleUML exposing (ModuleUML, fromFile, fromString)

import Dict exposing (Dict)
import Elm.Parser
import Elm.Processing as Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Function
import Set
import TypeUML exposing (TypeUML)


type alias ModuleUML =
    { name : String
    , types : List TypeUML
    }


type alias ModuleUMLState =
    { name : String
    , declaredTypes : Dict String TypeUML
    }


addDeclaredFunctionToTypes : (String -> Bool) -> Node Declaration -> ModuleUMLState -> ModuleUMLState
addDeclaredFunctionToTypes isExposed (Node _ declaration) moduleState =
    declaration
        |> Function.fromDeclaration isExposed
        |> Maybe.map
            (\function ->
                { moduleState | declaredTypes = (function |> TypeUML.addFunction |> Dict.map) moduleState.declaredTypes }
            )
        |> Maybe.withDefault moduleState


flip : (a -> b -> c) -> (b -> a -> c)
flip f a b =
    f b a


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

        moduleDef =
            moduleDefinition |> Node.value

        isExposed =
            case Module.exposingList moduleDef of
                All _ ->
                    always True

                Explicit topLevelExposed ->
                    topLevelExposed
                        |> List.filterMap
                            (\(Node _ expose) ->
                                case expose of
                                    FunctionExpose name ->
                                        Just name

                                    _ ->
                                        Nothing
                            )
                        |> List.foldl Set.insert Set.empty
                        |> flip Set.member

        initialModuleState : ModuleUMLState
        initialModuleState =
            { name = moduleDef |> Module.moduleName |> String.join "."
            , declaredTypes =
                declarations
                    |> List.filterMap typeNameNode
                    |> List.foldl
                        (\(Node range typeName) -> Dict.insert typeName <| TypeUML.init typeName range)
                        Dict.empty
            }

        moduleAfterFunctions =
            declarations |> List.foldl (addDeclaredFunctionToTypes isExposed) initialModuleState
    in
    { name = moduleAfterFunctions.name
    , types = Dict.values moduleAfterFunctions.declaredTypes
    }


fromString : String -> Maybe ModuleUML
fromString =
    Elm.Parser.parse
        >> Result.toMaybe
        >> Maybe.map (Processing.process Processing.init >> fromFile)
