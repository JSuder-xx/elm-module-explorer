module ModuleUMLTest exposing (suite)

import Expect exposing (Expectation)
import ModuleUML exposing (ModuleUML)
import Test exposing (..)
import TypeUML exposing (FunctionUML, TypeUML)


moduleHeader : String
moduleHeader =
    "module MyStuff exposing (..)"


widgetTypeAliasDeclaration : String
widgetTypeAliasDeclaration =
    "type alias Widget = { name : String, isCool : Bool }"


combine : List String -> String
combine =
    String.join "\n"


successfulModuleParse : List String -> (ModuleUML -> Expectation) -> Expectation
successfulModuleParse parts expectation =
    case ModuleUML.fromString <| combine parts of
        Nothing ->
            Expect.fail "Expected success"

        Just moduleUML ->
            expectation moduleUML


type alias Functions =
    { returnedBy : List FunctionUML
    , projectionsOf : List FunctionUML
    , updaters : List FunctionUML
    , referencedBy : List FunctionUML
    }


emptyFunctions : Functions
emptyFunctions =
    { referencedBy = []
    , projectionsOf = []
    , updaters = []
    , returnedBy = []
    }


expectTypeInModule : TypeUML -> ModuleUML -> Expectation
expectTypeInModule expectedType moduleUML =
    case List.head moduleUML.types of
        Nothing ->
            Expect.fail "Expecting a type"

        Just actualType ->
            Expect.equal actualType expectedType


testSingleFunctionModule : String -> Functions -> Test
testSingleFunctionModule fnString expectedType =
    test fnString <|
        \_ ->
            successfulModuleParse
                [ moduleHeader
                , widgetTypeAliasDeclaration
                , fnString
                ]
                (expectTypeInModule
                    { name = "Widget"
                    , lineNumber = 2
                    , inOutput = expectedType.returnedBy
                    , inInput = expectedType.projectionsOf
                    , inInputAndOutput = expectedType.updaters
                    , referencedBy = expectedType.referencedBy
                    }
                )


suite : Test
suite =
    describe "ModuleUML.fromString"
        [ describe "given only a type declaration and no functions" <|
            let
                success =
                    successfulModuleParse [ moduleHeader, widgetTypeAliasDeclaration ]
            in
            [ test "module name is correct" <| always <| success <| \moduleUML -> Expect.equal moduleUML.name "MyStuff"
            , test "there is a single type" <| always <| success <| \moduleUML -> Expect.equal (List.length moduleUML.types) 1
            , test "the single type has no functions" <|
                ({ name = "Widget"
                 , lineNumber = 2
                 , inOutput = []
                 , inInputAndOutput = []
                 , inInput = []
                 , referencedBy = []
                 }
                    |> expectTypeInModule
                    |> success
                    |> always
                )
            ]
        , describe "given a type declaration and three different kinds of functions" <|
            let
                success fn =
                    successfulModuleParse
                        [ moduleHeader
                        , widgetTypeAliasDeclaration
                        , "makesWidget : String -> Widget\nmakesWidget s = { name = s, isCool = True }"
                        , "projectsWidget : Widget -> String\nprojectsWidget { name } = name"
                        , "updatesWidget : String -> Widget -> Widget\nupdatesWidget s w = w"
                        ]
                        fn
            in
            [ test "module name is correct" <| always <| success <| \moduleUML -> Expect.equal moduleUML.name "MyStuff"
            , test "there is a single type" <| always <| success <| \moduleUML -> Expect.equal (List.length moduleUML.types) 1
            , test "the single type has expected functions" <|
                ({ name = "Widget"
                 , lineNumber = 2
                 , inOutput = [ { name = "makesWidget", lineNumber = 3, typeAnnotation = "String -> Widget" } ]
                 , inInput = [ { name = "projectsWidget", lineNumber = 5, typeAnnotation = "Widget -> String" } ]
                 , inInputAndOutput = [ { name = "updatesWidget", lineNumber = 7, typeAnnotation = "String -> (Widget -> Widget)" } ]
                 , referencedBy = []
                 }
                    |> expectTypeInModule
                    |> success
                    |> always
                )
            ]
        , testSingleFunctionModule
            "smartWidgetConstructor : String -> Maybe Widget\nsmartWidgetConstructor s = Just { name = s, isCool = True }"
            { emptyFunctions
                | returnedBy = [ { name = "smartWidgetConstructor", lineNumber = 3, typeAnnotation = "String -> Maybe Widget" } ]
            }
        , testSingleFunctionModule
            "weirdUpdater : { x: String, y: Widget } -> Maybe Widget\nweirdUpdater {y} = Just y"
            { emptyFunctions
                | updaters = [ { name = "weirdUpdater", lineNumber = 3, typeAnnotation = "{x : String, y : Widget} -> Maybe Widget" } ]
            }
        , testSingleFunctionModule
            "weirdThing : { x: String, y: Widget -> Int} -> Maybe Widget\nweirdThing {y} = Just y"
            { emptyFunctions
                | returnedBy = [ { name = "weirdThing", lineNumber = 3, typeAnnotation = "{x : String, y : Widget -> Int} -> Maybe Widget" } ]
            }
        , testSingleFunctionModule
            "weirdThing : { x: String, y: Widget -> Int} -> String\nweirdThing {x} = x"
            { emptyFunctions
                | referencedBy = [ { name = "weirdThing", lineNumber = 3, typeAnnotation = "{x : String, y : Widget -> Int} -> String" } ]
            }
        , testSingleFunctionModule
            "proj : Widget -> Int -> Int\nproj w _ = 10"
            { emptyFunctions
                | projectionsOf = [ { name = "proj", lineNumber = 3, typeAnnotation = "Widget -> (Int -> Int)" } ]
            }
        , testSingleFunctionModule
            "proj : Widget -> Int -> Int -> Int\nproj w _ = 10"
            { emptyFunctions
                | projectionsOf = [ { name = "proj", lineNumber = 3, typeAnnotation = "Widget -> (Int -> (Int -> Int))" } ]
            }
        , testSingleFunctionModule
            "proj : Int -> Widget -> Int\nproj w _ = 10"
            { emptyFunctions
                | projectionsOf = [ { name = "proj", lineNumber = 3, typeAnnotation = "Int -> (Widget -> Int)" } ]
            }
        , testSingleFunctionModule
            "proj : Int -> Widget -> Int -> Int\nproj w _ = 10"
            { emptyFunctions
                | projectionsOf = [ { name = "proj", lineNumber = 3, typeAnnotation = "Int -> (Widget -> (Int -> Int))" } ]
            }
        , testSingleFunctionModule
            "proj : Int -> Int -> Widget -> Int\nproj w _ = 10"
            { emptyFunctions
                | projectionsOf = [ { name = "proj", lineNumber = 3, typeAnnotation = "Int -> (Int -> (Widget -> Int))" } ]
            }
        , testSingleFunctionModule
            "proj : Int -> (Widget -> Int) -> Int\nproj w _ = 10"
            { emptyFunctions
                | referencedBy = [ { name = "proj", lineNumber = 3, typeAnnotation = "Int -> ((Widget -> Int) -> Int)" } ]
            }
        ]
