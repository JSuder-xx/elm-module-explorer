module Function exposing (Function, fromDeclaration)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Writer as Writer
import TypeRoles exposing (TypeRoles)


type alias Function =
    { name : String
    , typeAnnotation : String
    , lineNumber : Int
    , typeRoles : TypeRoles
    }


fromDeclaration : Declaration -> Maybe Function
fromDeclaration declaration =
    case declaration of
        FunctionDeclaration { signature } ->
            signature
                |> Maybe.andThen
                    (\(Node range { name, typeAnnotation }) ->
                        Just
                            { name = Node.value name
                            , lineNumber = range.start.row
                            , typeAnnotation = typeAnnotation |> Writer.writeTypeAnnotation |> Writer.write
                            , typeRoles = typeAnnotation |> Node.value |> TypeRoles.fromAnnotation
                            }
                    )

        _ ->
            Nothing
