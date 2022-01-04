module TypeUML exposing (FunctionUML, TypeUML, addFunction, init)

import Elm.Syntax.Range exposing (Range)
import Function exposing (Function)
import Set


type alias FunctionUML =
    { name : String, lineNumber : Int, typeAnnotation : String }


type alias TypeUML =
    { name : String
    , lineNumber : Int
    , returnedBy : List FunctionUML
    , projectionsOf : List FunctionUML
    , updaters : List FunctionUML
    , referencedBy : List FunctionUML
    }


init : String -> Range -> TypeUML
init name range =
    { name = name
    , lineNumber = range.start.row
    , returnedBy = []
    , projectionsOf = []
    , updaters = []
    , referencedBy = []
    }


includeReturnedBy : FunctionUML -> TypeUML -> TypeUML
includeReturnedBy f typeUML =
    { typeUML | returnedBy = f :: typeUML.returnedBy }


includeProjectionsOf : FunctionUML -> TypeUML -> TypeUML
includeProjectionsOf f typeUML =
    { typeUML | projectionsOf = f :: typeUML.projectionsOf }


includeUpdaters : FunctionUML -> TypeUML -> TypeUML
includeUpdaters f typeUML =
    { typeUML | updaters = f :: typeUML.updaters }


includeReferencedBy : FunctionUML -> TypeUML -> TypeUML
includeReferencedBy f typeUML =
    { typeUML | referencedBy = f :: typeUML.referencedBy }


addFunction : Function -> String -> TypeUML -> TypeUML
addFunction { name, lineNumber, typeAnnotation, typeRoles } typeName typeUML =
    let
        functionUML =
            { name = name, lineNumber = lineNumber, typeAnnotation = typeAnnotation }

        isMemberOf =
            Set.member typeName
    in
    (if isMemberOf typeRoles.invariantTypes then
        Just includeUpdaters

     else if isMemberOf typeRoles.outTypes then
        Just includeReturnedBy

     else if isMemberOf typeRoles.inTypes then
        Just includeProjectionsOf

     else if isMemberOf typeRoles.referencedTypes then
        Just includeReferencedBy

     else
        Nothing
    )
        |> Maybe.map (\include -> include functionUML typeUML)
        |> Maybe.withDefault typeUML
