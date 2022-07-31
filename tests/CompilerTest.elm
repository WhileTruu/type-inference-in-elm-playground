module CompilerTest exposing (..)

import Compiler.AST as AST exposing (Annotation)
import Compiler.Parser as Parser
import Compiler.Typechecker.V1 as TypecheckerV1
import Compiler.Typechecker.V2 as TypecheckerV2
import Compiler.Typechecker.V3 as TypecheckerV3
import Compiler.Typechecker.V4 as TypecheckerV4
import Compiler.Typechecker.V5 as TypecheckerV5
import Examples
import Expect exposing (Expectation)
import Test exposing (..)


type Error typeError
    = TypeError typeError
    | ParseError String


compileV1TestSuite : Test
compileV1TestSuite =
    compileTestSuite "typechecker v1" TypecheckerV1.run


compileV2TestSuite : Test
compileV2TestSuite =
    compileTestSuite "typechecker v2" TypecheckerV2.run


compileV3TestSuite : Test
compileV3TestSuite =
    compileTestSuite "typechecker v3" TypecheckerV3.run


compileV4TestSuite : Test
compileV4TestSuite =
    compileTestSuite "typechecker v4" TypecheckerV4.run


compileV5TestSuite : Test
compileV5TestSuite =
    compileTestSuite "typechecker v5" TypecheckerV5.run


compileTestSuite : String -> (AST.Expr -> Result error Annotation) -> Test
compileTestSuite description runTypechecker =
    let
        parseAndTypecheck : String -> Result (Error error) String
        parseAndTypecheck input =
            Parser.run input
                |> Result.mapError ParseError
                |> Result.andThen (runTypechecker >> Result.mapError TypeError)
                |> Result.map AST.prettyScheme
    in
    Test.describe ("compile - " ++ description)
        (List.map
            (\{ name, code, annotation } ->
                Test.test name <|
                    \_ -> Expect.equal (parseAndTypecheck code) (Ok annotation)
            )
            Examples.examples
            ++ [ Test.test "int with an arg" <|
                    \_ ->
                        Expect.equal
                            (Result.mapError (\_ -> ())
                                (parseAndTypecheck "\\a -> (1 a)")
                            )
                            (Err ())
               , Test.test "if function with int arg" <|
                    \_ ->
                        Expect.equal
                            (Result.mapError (\_ -> ())
                                (parseAndTypecheck "\\a -> \\b -> ((if (1)) a) b")
                            )
                            (Err ())
               ]
        )
