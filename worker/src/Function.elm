module Function exposing (Function, fromDeclaration)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Writer as Writer
import TypeRoles exposing (TypeRoles)


type alias Function =
    { name : String
    , isExposed : Bool
    , typeAnnotation : String
    , lineNumber : Int
    , typeRoles : TypeRoles
    }


fromDeclaration : (String -> Bool) -> Declaration -> Maybe Function
fromDeclaration isExposed declaration =
    case declaration of
        FunctionDeclaration { signature } ->
            signature
                |> Maybe.andThen
                    (\(Node range { name, typeAnnotation }) ->
                        let
                            nameString =
                                Node.value name
                        in
                        Just
                            { name = nameString
                            , isExposed = isExposed nameString
                            , lineNumber = range.start.row
                            , typeAnnotation = typeAnnotation |> Writer.writeTypeAnnotation |> Writer.write
                            , typeRoles = typeAnnotation |> Node.value |> TypeRoles.fromAnnotation
                            }
                    )

        _ ->
            Nothing
