module Compiler.Parser exposing (..)

import Compiler.AST exposing (Expr(..))
import Data.Name as Name exposing (Name)
import Parser as P exposing ((|.), (|=), Parser)
import Set


run : String -> Result String Expr
run string =
    P.run expression string
        |> Result.mapError P.deadEndsToString


term : Parser Expr
term =
    P.oneOf
        [ variable
            |> P.map (\name -> ExprVar (Name.fromString name))
        , P.number
            { int = Just ExprInt
            , hex = Nothing
            , octal = Nothing
            , binary = Nothing
            , float = Nothing
            }
            -- FIXME remove backtrackable
            |> P.backtrackable
        , P.succeed identity
            |. P.symbol "("
            |= P.lazy (\_ -> expression)
            |. P.symbol ")"
        ]


variable : Parser String
variable =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList []
        }


expression : Parser Expr
expression =
    P.oneOf
        [ lambda
        , term |> P.andThen chompExprEnd
        ]


chompExprEnd : Expr -> Parser Expr
chompExprEnd expr =
    P.succeed identity
        |. ignoreables
        |= P.oneOf
            [ P.succeed (ExprCall expr)
                |. checkIndent (<) "Expecting indentation"
                |= term
            , P.succeed expr
            ]


varAndChompExprEnd : Name -> P.Parser Expr
varAndChompExprEnd name =
    P.succeed (ExprVar name)
        |> P.andThen (\expr -> chompExprEnd expr)


lambda : Parser Expr
lambda =
    P.succeed ExprLambda
        |. backslash
        |= P.map Name.fromString variable
        |. spacesOnly
        |. P.symbol "->"
        |. ignoreablesAndCheckIndent (<) "Invalid func body indent"
        |= P.lazy (\_ -> expression)



-- HELPERS


rememberIndentation : Parser a -> Parser a
rememberIndentation parser =
    P.getCol
        |> P.andThen (\col -> P.withIndent col parser)


ignoreables : Parser ()
ignoreables =
    P.loop 0 <|
        ifProgress <|
            P.oneOf
                [ P.symbol "\t"
                    |> P.andThen (\_ -> P.problem "Invalid tab")
                , P.spaces
                ]


ifProgress : Parser a -> Int -> Parser (P.Step Int ())
ifProgress parser offset =
    P.succeed identity
        |. parser
        |= P.getOffset
        |> P.map
            (\newOffset ->
                if offset == newOffset then
                    P.Done ()

                else
                    P.Loop newOffset
            )


checkIndent : (Int -> Int -> Bool) -> String -> Parser ()
checkIndent check error =
    P.succeed
        (\indent col ->
            if check indent col then
                P.succeed ()

            else
                P.problem error
        )
        |= P.getIndent
        |= P.getCol
        |> P.andThen identity


ignoreablesAndCheckIndent : (Int -> Int -> Bool) -> String -> Parser ()
ignoreablesAndCheckIndent check error =
    P.succeed ()
        |. ignoreables
        |. checkIndent check error


spacesOnly : P.Parser ()
spacesOnly =
    P.chompWhile ((==) ' ')


backslash : P.Parser ()
backslash =
    P.token "\\"
