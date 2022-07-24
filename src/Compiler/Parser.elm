module Compiler.Parser exposing (..)

import Compiler.AST exposing (Exp(..))
import Data.Name as Name exposing (Name)
import Parser as P exposing ((|.), (|=), Parser)
import Set


run : String -> Result String Exp
run string =
    P.run expression string
        |> Result.mapError P.deadEndsToString


term : Parser Exp
term =
    P.oneOf
        [ variable
            |> P.map (\name -> EVar (Name.fromString name))
        , P.number
            { int = Just EInt
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


expression : Parser Exp
expression =
    P.oneOf
        [ lambda
        , rememberIndentation defsOrVarAndChompExprEnd
        , term |> P.andThen chompExprEnd
        ]


chompExprEnd : Exp -> Parser Exp
chompExprEnd expr =
    P.succeed identity
        |. ignoreables
        |= P.oneOf
            [ P.succeed (EApp expr)
                |. checkIndent (<) "Expecting indentation"
                |= term
            , P.succeed expr
            ]


defsOrVarAndChompExprEnd : P.Parser Exp
defsOrVarAndChompExprEnd =
    P.loop [] defsOrVarAndChompExprEndHelp


defsOrVarAndChompExprEndHelp : List ( Name, Exp ) -> Parser (P.Step (List ( Name, Exp )) Exp)
defsOrVarAndChompExprEndHelp args =
    P.oneOf
        [ P.succeed Name.fromString
            |= variable
            |. spacesOnly
            |> P.andThen
                (\name ->
                    P.oneOf
                        [ P.succeed (\body -> ( name, body ))
                            |. P.symbol "="
                            |. ignoreablesAndCheckIndent (<) "ExpectingIndentation"
                            |= P.lazy (\_ -> expression)
                            |> P.map (\def2_ -> P.Loop (def2_ :: args))
                        , if List.isEmpty args then
                            varAndChompExprEnd name
                                |> P.map P.Done

                          else
                            P.succeed (\expr -> ELet (List.reverse args) expr)
                                |= varAndChompExprEnd name
                                |> P.map P.Done
                        ]
                )
        , if List.isEmpty args then
            P.problem "Expecting defs"

          else
            P.succeed (\expr -> ELet (List.reverse args) expr)
                |= P.lazy (\_ -> expression)
                |> P.map P.Done
        ]


varAndChompExprEnd : Name -> P.Parser Exp
varAndChompExprEnd name =
    P.succeed (EVar name)
        |> P.andThen (\expr -> chompExprEnd expr)


lambda : Parser Exp
lambda =
    P.succeed ELam
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
