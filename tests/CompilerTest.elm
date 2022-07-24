module CompilerTest exposing (..)

import Compiler.AST as AST exposing (Annotation)
import Compiler.Parser as Parser
import Compiler.Typechecker.V1 as TypecheckerV1
import Compiler.Typechecker.V2 as TypecheckerV2
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


compileTestSuite : String -> (AST.Expr -> Result error Annotation) -> Test
compileTestSuite description runTypechecker =
    let
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
        )
