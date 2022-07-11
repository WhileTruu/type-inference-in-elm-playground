module CompilerTest exposing (..)

import Compiler.AST as AST exposing (Scheme)
import Compiler.Parser as Parser
import Compiler.Typechecker as Typechecker
import Expect exposing (Expectation)
import Test exposing (..)


type Error
    = TypeError Typechecker.Error
    | ParseError String


compileTestSuite : Test
compileTestSuite =
    let
        parseAndTypecheck : String -> Result Error String
        parseAndTypecheck input =
            Parser.run input
                |> Result.mapError ParseError
                |> Result.andThen (Typechecker.run >> Result.mapError TypeError)
                |> Result.map AST.prettyScheme
    in
    Test.describe "compile"
        [ Test.test "deeply nested lambda" <|
            \_ ->
                let
                    input : String
                    input =
                        String.join "\n"
                            [ "\\a1 ->"
                            , "  \\a2 ->"
                            , "    \\a3 ->"
                            , "      \\a3 ->"
                            , "        \\a4 ->"
                            , "          \\a5 ->"
                            , "            \\a6 ->"
                            , "              \\a7 ->"
                            , "                \\a8 ->"
                            , "                  \\a9 ->"
                            , "                    \\a10 ->"
                            , "                      \\a11 ->"
                            , "                        \\a12 ->"
                            , "                          \\a13 ->"
                            , "                            \\a14 ->"
                            , "                              \\a15 ->"
                            , "                                \\a16 ->"
                            , "                                  \\a17 ->"
                            , "                                    \\a18 ->"
                            , "                                      \\a19 ->"
                            , "                                        \\a20 ->"
                            , "                                          \\a21 ->"
                            , "                                            \\a22 ->"
                            , "                                              \\a23 ->"
                            , "                                                \\a24 ->"
                            , "                                                  \\a25 ->"
                            , "                                                    \\a26 ->"
                            , "                                                      1"
                            ]
                in
                Expect.equal (parseAndTypecheck input)
                    (Ok
                        ("∀ a b c d e f g h i j k l m n o p q r s t u v w x y z a1."
                            ++ " a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o"
                            ++ " -> p -> q -> r -> s -> t -> u -> v -> w -> x -> y -> z -> a1 -> Int"
                        )
                    )
        , Test.test "lambda argument is inferred" <|
            \_ ->
                let
                    input : String
                    input =
                        String.join "\n"
                            [ "\\a ->"
                            , "  x = 1"
                            , "  (add a) x"
                            ]
                in
                Expect.equal (parseAndTypecheck input)
                    (Ok "Int -> Int")
        , Test.test "nested lambda arguments are inferred" <|
            \_ ->
                let
                    input : String
                    input =
                        String.join "\n"
                            [ "\\a ->"
                            , "  \\b ->"
                            , "    (add a) b"
                            ]
                in
                Expect.equal (parseAndTypecheck input)
                    (Ok "Int -> Int -> Int")
        , Test.test "variable shadowing" <|
            \_ ->
                let
                    input : String
                    input =
                        String.join "\n"
                            [ "\\a ->"
                            , "  \\b ->"
                            , "    a = 10"
                            , "    (add a) b"
                            ]
                in
                Expect.equal (parseAndTypecheck input)
                    (Ok "∀ a. a -> Int -> Int")
        , Test.test "context" <|
            \_ ->
                let
                    input : String
                    input =
                        String.join "\n"
                            [ "\\a ->"
                            , "  potato = 10"
                            , "  \\b ->"
                            , "    potato"
                            ]
                in
                Expect.equal (parseAndTypecheck input)
                    (Ok "∀ a b. a -> b -> Int")
        , Test.test "ordered defs" <|
            \_ ->
                let
                    input : String
                    input =
                        String.join "\n"
                            [ "\\a ->"
                            , "  mike = 10"
                            , "  mikeAlias = mike"
                            , "  mikeSecondAlias = mikeAlias"
                            , "  (add mikeSecondAlias) a"
                            ]
                in
                Expect.equal (parseAndTypecheck input)
                    (Ok "Int -> Int")

        -- TODO redefinition shouldn't be possible
        -- Better as part of parsing, canonicalization or sth?
        -- , Test.test "redefinition" <|
        --     \_ ->
        --         let
        --             input : String
        --             input =
        --                 String.join "\n"
        --                     [ "\\a ->"
        --                     , "  x = 1"
        --                     , "  x = 2"
        --                     , "  x"
        --                     ]
        --         in
        --         Expect.equal (parseAndTypecheck input)
        --             (Ok "Int -> Int")
        ]
