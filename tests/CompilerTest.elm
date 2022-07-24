module CompilerTest exposing (..)

import Compiler.AST as AST exposing (Annotation)
import Compiler.Parser as Parser
import Compiler.Typechecker.V1 as TypecheckerV1
import Expect exposing (Expectation)
import Test exposing (..)


type Error typeError
    = TypeError typeError
    | ParseError String


compileV1TestSuite : Test
compileV1TestSuite =
    compileTestSuite TypecheckerV1.run


compileTestSuite : (AST.Expr -> Result error Annotation) -> Test
compileTestSuite runTypechecker =
    let
        parseAndTypecheck input =
            Parser.run input
                |> Result.mapError ParseError
                |> Result.andThen (runTypechecker >> Result.mapError TypeError)
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
                            , "  (add a) 1"
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
                            , "    (add ((\\a -> a) 10)) b"
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
                            , "  ((\\potato ->"
                            , "     \\b ->"
                            , "         potato"
                            , "  ) 10)"
                            ]
                in
                Expect.equal (parseAndTypecheck input)
                    (Ok "∀ a b. a -> b -> Int")
        ]
