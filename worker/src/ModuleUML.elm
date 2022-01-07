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
import TypeUML exposing (FunctionUML, TypeUML)


type alias ModuleUML =
    { name : String
    , types : List TypeUML
    , nomads : List FunctionUML
    , missingSignatures : List FunctionUML
    }


type alias ModuleUMLState =
    { name : String
    , declaredTypes : Dict String TypeUML
    , nomads : List FunctionUML
    }


addDeclaredFunctionToTypes : (String -> Bool) -> Node Declaration -> ModuleUMLState -> ModuleUMLState
addDeclaredFunctionToTypes isExposed (Node _ declaration) moduleState =
    declaration
        |> Function.fromDeclaration isExposed
        |> Maybe.map
            (\function ->
                let
                    interim =
                        moduleState.declaredTypes
                            |> Dict.map (TypeUML.addFunction function)

                    wasAdded =
                        interim
                            |> Dict.values
                            |> List.map Tuple.second
                            |> List.foldl (||) False
                in
                { moduleState
                    | declaredTypes = Dict.map (always Tuple.first) interim
                    , nomads =
                        if not wasAdded then
                            TypeUML.functionUMLFromFunction function :: moduleState.nomads

                        else
                            moduleState.nomads
                }
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

        functionWithoutSignature : Node Declaration -> Maybe FunctionUML
        functionWithoutSignature (Node range functionDeclaration) =
            case functionDeclaration of
                FunctionDeclaration { declaration, signature } ->
                    case signature of
                        Just _ ->
                            Nothing

                        Nothing ->
                            declaration
                                |> Node.value
                                |> .name
                                |> Node.value
                                |> (\name ->
                                        Just
                                            { name = name
                                            , isExposed = isExposed name
                                            , lineNumber = range.start.row
                                            , typeAnnotation = ""
                                            }
                                   )

                _ ->
                    Nothing

        initialModuleState : ModuleUMLState
        initialModuleState =
            { name = moduleDef |> Module.moduleName |> String.join "."
            , nomads = []
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
    , missingSignatures = declarations |> List.filterMap functionWithoutSignature
    , nomads = moduleAfterFunctions.nomads
    }


fromString : String -> Maybe ModuleUML
fromString =
    Elm.Parser.parse
        >> Result.toMaybe
        >> Maybe.map (Processing.process Processing.init >> fromFile)
