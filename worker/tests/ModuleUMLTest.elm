module ModuleUMLTest exposing (..)

import Expect exposing (Expectation)
import ModuleUML exposing (ModuleUML)
import Test exposing (..)


moduleHeader =
    "module MyStuff exposing (..)"


widgetTypeAliasDeclaration =
    "type alias Widget = { name : String, isCool : Bool }"


combine =
    String.join "\n"


expectSuccess : List String -> (ModuleUML -> Expectation) -> () -> Expectation
expectSuccess parts expectation =
    \_ ->
        case ModuleUML.fromString <| combine parts of
            Nothing ->
                Expect.fail "Expected success"

            Just moduleUML ->
                expectation moduleUML


suite : Test
suite =
    describe "ModuleUML.fromString"
        [ describe "given only a type declaration and no functions" <|
            let
                success =
                    expectSuccess [ moduleHeader, widgetTypeAliasDeclaration ]
            in
            [ test "module name is correct" <| success <| \moduleUML -> Expect.equal moduleUML.name "MyStuff"
            , test "there is a single type" <| success <| \moduleUML -> Expect.equal (List.length moduleUML.types) 1
            , test "the single type has no functions" <|
                success <|
                    \moduleUML ->
                        case List.head moduleUML.types of
                            Nothing ->
                                Expect.fail "Expecting a type"

                            Just theType ->
                                Expect.equal
                                    theType
                                    { name = "Widget"
                                    , lineNumber = 2
                                    , returnedBy = []
                                    , updaters = []
                                    , projectionsOf = []
                                    , referencedBy = []
                                    }
            ]
        , describe "given a type declaration and three different kinds of functions" <|
            let
                success =
                    expectSuccess
                        [ moduleHeader
                        , widgetTypeAliasDeclaration
                        , "makesWidget : String -> Widget\nmakesWidget s = { name = s, isCool = True }"
                        , "projectsWidget : Widget -> String\nprojectsWidget { name } = name"
                        , "updatesWidget : String -> Widget -> Widget\nupdatesWidget s w = w"
                        ]
            in
            [ test "module name is correct" <| success <| \moduleUML -> Expect.equal moduleUML.name "MyStuff"
            , test "there is a single type" <| success <| \moduleUML -> Expect.equal (List.length moduleUML.types) 1
            , test "the single type has expected functions" <|
                success <|
                    \moduleUML ->
                        case List.head moduleUML.types of
                            Nothing ->
                                Expect.fail "Expecting a type"

                            Just theType ->
                                Expect.equal
                                    theType
                                    { name = "Widget"
                                    , lineNumber = 2
                                    , returnedBy = [ { name = "makesWidget", lineNumber = 3, typeAnnotation = "String -> Widget" } ]
                                    , projectionsOf = [ { name = "projectsWidget", lineNumber = 5, typeAnnotation = "Widget -> String" } ]
                                    , updaters = [ { name = "updatesWidget", lineNumber = 7, typeAnnotation = "String -> (Widget -> Widget)" } ]
                                    , referencedBy = []
                                    }
            ]
        , describe "given a type declaration and smart constructor" <|
            let
                success =
                    expectSuccess
                        [ moduleHeader
                        , widgetTypeAliasDeclaration
                        , "smartWidgetConstructor : String -> Maybe Widget\nsmartWidgetConstructor s = Just { name = s, isCool = True }"
                        ]
            in
            [ test "module name is correct" <| success <| \moduleUML -> Expect.equal moduleUML.name "MyStuff"
            , test "there is a single type" <| success <| \moduleUML -> Expect.equal (List.length moduleUML.types) 1
            , test "the single type has the smart constructor in returnedBy" <|
                success <|
                    \moduleUML ->
                        case List.head moduleUML.types of
                            Nothing ->
                                Expect.fail "Expecting a type"

                            Just theType ->
                                Expect.equal
                                    theType
                                    { name = "Widget"
                                    , lineNumber = 2
                                    , returnedBy = [ { name = "smartWidgetConstructor", lineNumber = 3, typeAnnotation = "String -> Maybe Widget" } ]
                                    , projectionsOf = []
                                    , updaters = []
                                    , referencedBy = []
                                    }
            ]
        , describe "given a function with the type nested in a record in an out position" <|
            let
                success =
                    expectSuccess
                        [ moduleHeader
                        , widgetTypeAliasDeclaration
                        , "weirdUpdater : { x: String, y: Widget } -> Maybe Widget\nweirdUpdater {y} = Just y"
                        ]
            in
            [ test "module name is correct" <| success <| \moduleUML -> Expect.equal moduleUML.name "MyStuff"
            , test "there is a single type" <| success <| \moduleUML -> Expect.equal (List.length moduleUML.types) 1
            , test "the single type has the weirdUpdater in updaters" <|
                success <|
                    \moduleUML ->
                        case List.head moduleUML.types of
                            Nothing ->
                                Expect.fail "Expecting a type"

                            Just theType ->
                                Expect.equal
                                    theType
                                    { name = "Widget"
                                    , lineNumber = 2
                                    , returnedBy = []
                                    , projectionsOf = []
                                    , updaters = [ { name = "weirdUpdater", lineNumber = 3, typeAnnotation = "{x : String, y : Widget} -> Maybe Widget" } ]
                                    , referencedBy = []
                                    }
            ]
        , describe "given a function with the type nested in a record in an in position and also returning" <|
            let
                success =
                    expectSuccess
                        [ moduleHeader
                        , widgetTypeAliasDeclaration
                        , "weirdThing : { x: String, y: Widget -> Int} -> Maybe Widget\nweirdThing {y} = Just y"
                        ]
            in
            [ test "module name is correct" <| success <| \moduleUML -> Expect.equal moduleUML.name "MyStuff"
            , test "there is a single type" <| success <| \moduleUML -> Expect.equal (List.length moduleUML.types) 1
            , test "the single type has the weirdThing in returnedBy" <|
                success <|
                    \moduleUML ->
                        case List.head moduleUML.types of
                            Nothing ->
                                Expect.fail "Expecting a type"

                            Just theType ->
                                Expect.equal
                                    theType
                                    { name = "Widget"
                                    , lineNumber = 2
                                    , projectionsOf = []
                                    , returnedBy = [ { name = "weirdThing", lineNumber = 3, typeAnnotation = "{x : String, y : Widget -> Int} -> Maybe Widget" } ]
                                    , updaters = []
                                    , referencedBy = []
                                    }
            ]
        , describe "given a function with the type nested in a record" <|
            let
                success =
                    expectSuccess
                        [ moduleHeader
                        , widgetTypeAliasDeclaration
                        , "weirdThing : { x: String, y: Widget -> Int} -> String\nweirdThing {x} = x"
                        ]
            in
            [ test "module name is correct" <| success <| \moduleUML -> Expect.equal moduleUML.name "MyStuff"
            , test "there is a single type" <| success <| \moduleUML -> Expect.equal (List.length moduleUML.types) 1
            , test "the single type has the weirdThing in returnedBy" <|
                success <|
                    \moduleUML ->
                        case List.head moduleUML.types of
                            Nothing ->
                                Expect.fail "Expecting a type"

                            Just theType ->
                                Expect.equal
                                    theType
                                    { name = "Widget"
                                    , lineNumber = 2
                                    , projectionsOf = []
                                    , referencedBy = [ { name = "weirdThing", lineNumber = 3, typeAnnotation = "{x : String, y : Widget -> Int} -> String" } ]
                                    , updaters = []
                                    , returnedBy = []
                                    }
            ]
        ]
