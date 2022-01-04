module TypeRoles exposing (TypeRoles, fromAnnotation)

import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import ModuleName exposing (qualifiedToString)
import Set exposing (Set)


type alias TypeRoles =
    { inTypes : Set String
    , outTypes : Set String
    , invariantTypes : Set String
    , referencedTypes : Set String
    }


empty : TypeRoles
empty =
    { inTypes = Set.empty, outTypes = Set.empty, invariantTypes = Set.empty, referencedTypes = Set.empty }


includeOutType : String -> TypeRoles -> TypeRoles
includeOutType s t =
    { t | outTypes = Set.insert s t.outTypes }


combine : TypeRoles -> TypeRoles -> TypeRoles
combine a b =
    { inTypes = Set.union a.inTypes b.inTypes
    , outTypes = Set.union a.outTypes b.outTypes
    , invariantTypes = Set.union a.invariantTypes b.invariantTypes
    , referencedTypes = Set.union a.referencedTypes b.referencedTypes
    }


convertToInTypes : TypeRoles -> TypeRoles
convertToInTypes { outTypes, inTypes, invariantTypes } =
    { inTypes = outTypes
    , referencedTypes = Set.union inTypes invariantTypes
    , outTypes = Set.empty
    , invariantTypes = Set.empty
    }


promoteToInvariant : TypeRoles -> TypeRoles
promoteToInvariant ({ inTypes, outTypes, invariantTypes } as tr) =
    let
        newInvariant =
            invariantTypes |> Set.union (Set.intersect inTypes outTypes)
    in
    { tr
        | invariantTypes = newInvariant
        , outTypes = Set.diff outTypes newInvariant
        , inTypes = Set.diff inTypes newInvariant
    }


fromTypeAnnotations : List (Node TypeAnnotation) -> TypeRoles
fromTypeAnnotations =
    List.map (Node.value >> fromAnnotation)
        >> List.foldl combine empty


fromAnnotation : TypeAnnotation -> TypeRoles
fromAnnotation typeAnnotation =
    case typeAnnotation of
        GenericType _ ->
            empty

        Typed (Node _ qualifiedTypeName) typeArguments ->
            typeArguments
                |> fromTypeAnnotations
                |> includeOutType (qualifiedToString qualifiedTypeName)

        Unit ->
            empty

        Tupled parts ->
            fromTypeAnnotations parts

        Record record ->
            record
                |> List.map (Node.value >> Tuple.second)
                |> fromTypeAnnotations

        GenericRecord _ (Node _ record) ->
            record
                |> List.map (Node.value >> Tuple.second)
                |> fromTypeAnnotations

        FunctionTypeAnnotation (Node _ aTypeAnnotation) (Node _ bTypeAnnotation) ->
            aTypeAnnotation
                |> fromAnnotation
                |> convertToInTypes
                |> combine (fromAnnotation bTypeAnnotation)
                |> promoteToInvariant
