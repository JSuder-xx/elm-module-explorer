module ModuleUMLTest exposing (suite)

import Elm.Syntax.Module exposing (exposingList)
import Expect exposing (Expectation)
import ModuleUML exposing (ModuleUML)
import Test exposing (..)
import TypeUML exposing (FunctionUML, TypeUML)


moduleHeader : String -> String
moduleHeader expose =
    "module MyStuff exposing (" ++ expose ++ ")"


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


suite : Test
suite =
    describe "ModuleUML.fromString"
        [ describe "given only a type declaration and no functions" <|
            let
                success =
                    successfulModuleParse [ moduleHeader "..", widgetTypeAliasDeclaration ]
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
        , describe "given a module declaring a Widget type" <|
            let
                successfulModuleParseWithFns exposingList functions =
                    successfulModuleParse
                        ([ moduleHeader exposingList
                         , widgetTypeAliasDeclaration
                         ]
                            ++ functions
                        )

                singleFunction : { givenExposingList : String, andFunctionDeclaration : String, thenFunctions : Functions } -> Test
                singleFunction { andFunctionDeclaration, givenExposingList, thenFunctions } =
                    test ("Given exposing " ++ givenExposingList ++ " and function declaration " ++ andFunctionDeclaration) <|
                        \_ ->
                            successfulModuleParseWithFns
                                givenExposingList
                                [ andFunctionDeclaration ]
                                (expectTypeInModule
                                    { name = "Widget"
                                    , lineNumber = 2
                                    , inOutput = thenFunctions.returnedBy
                                    , inInput = thenFunctions.projectionsOf
                                    , inInputAndOutput = thenFunctions.updaters
                                    , referencedBy = thenFunctions.referencedBy
                                    }
                                )
            in
            [ describe "and three different kinds of functions" <|
                let
                    success fn =
                        successfulModuleParse
                            [ moduleHeader ".."
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
                     , inOutput = [ { name = "makesWidget", isExposed = True, lineNumber = 3, typeAnnotation = "String -> Widget" } ]
                     , inInput = [ { name = "projectsWidget", isExposed = True, lineNumber = 5, typeAnnotation = "Widget -> String" } ]
                     , inInputAndOutput = [ { name = "updatesWidget", isExposed = True, lineNumber = 7, typeAnnotation = "String -> (Widget -> Widget)" } ]
                     , referencedBy = []
                     }
                        |> expectTypeInModule
                        |> success
                        |> always
                    )
                ]
            , singleFunction
                { givenExposingList = ".."
                , andFunctionDeclaration = "smartWidgetConstructor : String -> Maybe Widget\nsmartWidgetConstructor s = Just { name = s, isCool = True }"
                , thenFunctions =
                    { emptyFunctions
                        | returnedBy = [ { name = "smartWidgetConstructor", isExposed = True, lineNumber = 3, typeAnnotation = "String -> Maybe Widget" } ]
                    }
                }
            , singleFunction
                { givenExposingList = ".."
                , andFunctionDeclaration = "weirdUpdater : { x: String, y: Widget } -> Maybe Widget\nweirdUpdater {y} = Just y"
                , thenFunctions =
                    { emptyFunctions
                        | updaters = [ { name = "weirdUpdater", isExposed = True, lineNumber = 3, typeAnnotation = "{x : String, y : Widget} -> Maybe Widget" } ]
                    }
                }
            , singleFunction
                { givenExposingList = ".."
                , andFunctionDeclaration = "weirdThing : { x: String, y: Widget -> Int} -> Maybe Widget\nweirdThing {y} = Just y"
                , thenFunctions =
                    { emptyFunctions
                        | returnedBy = [ { name = "weirdThing", isExposed = True, lineNumber = 3, typeAnnotation = "{x : String, y : Widget -> Int} -> Maybe Widget" } ]
                    }
                }
            , singleFunction
                { givenExposingList = ".."
                , andFunctionDeclaration = "weirdThing : { x: String, y: Widget -> Int} -> String\nweirdThing {x} = x"
                , thenFunctions =
                    { emptyFunctions
                        | referencedBy = [ { name = "weirdThing", isExposed = True, lineNumber = 3, typeAnnotation = "{x : String, y : Widget -> Int} -> String" } ]
                    }
                }
            , singleFunction
                { givenExposingList = ".."
                , andFunctionDeclaration = "proj : Widget -> Int -> Int\nproj w _ = 10"
                , thenFunctions =
                    { emptyFunctions
                        | projectionsOf = [ { name = "proj", isExposed = True, lineNumber = 3, typeAnnotation = "Widget -> (Int -> Int)" } ]
                    }
                }
            , singleFunction
                { givenExposingList = ".."
                , andFunctionDeclaration = "proj : Widget -> Int -> Int -> Int\nproj w _ = 10"
                , thenFunctions =
                    { emptyFunctions
                        | projectionsOf = [ { name = "proj", isExposed = True, lineNumber = 3, typeAnnotation = "Widget -> (Int -> (Int -> Int))" } ]
                    }
                }
            , singleFunction
                { givenExposingList = ".."
                , andFunctionDeclaration = "proj : Int -> Widget -> Int\nproj w _ = 10"
                , thenFunctions =
                    { emptyFunctions
                        | projectionsOf = [ { name = "proj", isExposed = True, lineNumber = 3, typeAnnotation = "Int -> (Widget -> Int)" } ]
                    }
                }
            , singleFunction
                { givenExposingList = ".."
                , andFunctionDeclaration = "proj : Int -> Widget -> Int -> Int\nproj w _ = 10"
                , thenFunctions =
                    { emptyFunctions
                        | projectionsOf = [ { name = "proj", isExposed = True, lineNumber = 3, typeAnnotation = "Int -> (Widget -> (Int -> Int))" } ]
                    }
                }
            , singleFunction
                { givenExposingList = ".."
                , andFunctionDeclaration = "proj : Int -> Int -> Widget -> Int\nproj w _ = 10"
                , thenFunctions =
                    { emptyFunctions
                        | projectionsOf = [ { name = "proj", isExposed = True, lineNumber = 3, typeAnnotation = "Int -> (Int -> (Widget -> Int))" } ]
                    }
                }
            , singleFunction
                { givenExposingList = "Widget"
                , andFunctionDeclaration = "proj : Int -> (Widget -> Int) -> Int\nproj w _ = 10"
                , thenFunctions =
                    { emptyFunctions
                        | referencedBy = [ { name = "proj", isExposed = False, lineNumber = 3, typeAnnotation = "Int -> ((Widget -> Int) -> Int)" } ]
                    }
                }
            , singleFunction
                { givenExposingList = "proj"
                , andFunctionDeclaration = "proj : Int -> (Widget -> Int) -> Int\nproj w _ = 10"
                , thenFunctions =
                    { emptyFunctions
                        | referencedBy = [ { name = "proj", isExposed = True, lineNumber = 3, typeAnnotation = "Int -> ((Widget -> Int) -> Int)" } ]
                    }
                }
            ]
        , describe "given a module declaring no types" <|
            [ test "with no functions returns no nomads" <|
                always <|
                    successfulModuleParse
                        [ moduleHeader ".." ]
                        (\{ nomads } ->
                            if List.isEmpty nomads then
                                Expect.pass

                            else
                                Expect.fail "Where did the nomads come from?"
                        )
            , test "with a single function returns that as a nomad" <|
                always <|
                    successfulModuleParse
                        [ moduleHeader "..", "f : Int -> Int\nf x = x + 1" ]
                        (\{ nomads } ->
                            case nomads of
                                [ fn ] ->
                                    Expect.equal fn { name = "f", isExposed = True, lineNumber = 2, typeAnnotation = "Int -> Int" }

                                _ ->
                                    Expect.fail ("Expecting exactly one nomad but got " ++ (nomads |> List.length |> String.fromInt))
                        )
            ]
        ]
