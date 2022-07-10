module Main exposing (main)

import Browser
import Compiler.AST as AST exposing (Exp)
import Compiler.Parser as Parser
import Compiler.Typechecker as Typechecker
import Html exposing (Html, button, div, input, text, textarea)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { input : String
    , output : Result String Exp
    }


initialModel : Model
initialModel =
    { input = ""
    , output = Err "No expression"
    }


type Msg
    = CompileClicked
    | InputChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        CompileClicked ->
            { model | output = Parser.run model.input }

        InputChanged input ->
            { model | input = input }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick CompileClicked ] [ text "Compile" ]
        , textarea [ onInput InputChanged ] [ text model.input ]
        , Html.div []
            [ case model.output of
                Ok output ->
                    text <| "Success: " ++ AST.expToString output

                Err err ->
                    text <| "Error: " ++ err
            ]
        , Html.div []
            [ model.output
                |> Result.map
                    (\expr ->
                        case Typechecker.run expr of
                            Ok output ->
                                text <| "Success: " ++ AST.prettyScheme output

                            Err err ->
                                text <| "Error: " ++ err
                    )
                |> Result.withDefault (text "")
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
