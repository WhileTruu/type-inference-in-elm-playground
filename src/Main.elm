module Main exposing (main)

import Browser
import Compiler.AST as AST exposing (Expr)
import Compiler.Parser as Parser
import Compiler.Typechecker.V1 as Typechecker
import Data.Name as Name
import Element as El
import Element.Background as ElBackground
import Element.Border as ElBorder
import Element.Input as ElInput
import Examples
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs
import TreeDiagram
import TreeDiagram.Svg



-- MODEL


type alias Model =
    { input : String
    , output : Result String Expr
    }


initialModel : Model
initialModel =
    { input = ""
    , output = Err "No expression"
    }



-- UPDATE


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



-- VIEW


view : Model -> El.Element Msg
view model =
    El.row [ El.width El.fill, El.spacing 30, El.height El.fill ]
        [ El.column
            [ El.alignTop
            , El.spacing 8
            , El.height El.fill
            , ElBackground.color (El.rgb255 230 230 230)
            , El.padding 16
            ]
            (List.map
                (\example ->
                    button
                        { onPress = Just (InputChanged example.code)
                        , label = El.el [ El.centerX ] (El.text example.name)
                        }
                )
                Examples.examples
            )
        , El.column [ El.alignTop, El.spacing 30, El.width (El.px 500) ]
            [ ElInput.multiline []
                { onChange = InputChanged
                , text = model.input
                , placeholder = Nothing
                , label = ElInput.labelHidden "code input"
                , spellcheck = False
                }
            , button
                { onPress = Just CompileClicked
                , label = El.text "Compile"
                }
            , El.el []
                (case model.output of
                    Ok output ->
                        El.text <| "Success: " ++ AST.exprToString output

                    Err err ->
                        El.text <| "Error: " ++ err
                )
            , El.el []
                (model.output
                    |> Result.map
                        (\expr ->
                            case Typechecker.run expr of
                                Ok output ->
                                    El.text <| "Success: " ++ AST.prettyScheme (Debug.log "" output)

                                Err err ->
                                    El.text <| "Error: " ++ Typechecker.errorToString err
                        )
                    |> Result.withDefault (El.text "")
                )
            ]
        , case model.output of
            Ok output ->
                viewExprTree output

            Err err ->
                El.none
        ]


button : { onPress : Maybe msg, label : El.Element msg } -> El.Element msg
button =
    ElInput.button
        [ El.paddingXY 20 0
        , El.height (El.px 57)
        , ElBorder.rounded 14
        , ElBackground.color (El.rgb255 255 255 255)
        , El.width El.fill
        ]


viewExprTree : AST.Expr -> El.Element msg
viewExprTree expr =
    El.el [ El.height El.fill, El.width El.fill ]
        (El.html
            (TreeDiagram.Svg.draw
                ((\a -> { a | siblingDistance = 100 })
                    TreeDiagram.defaultTreeLayout
                )
                svgDrawNode
                svgDrawLine
                (treeFromExpr expr)
            )
        )


svgDrawLine : ( Float, Float ) -> Svg msg
svgDrawLine ( targetX, targetY ) =
    Svg.line
        [ SvgAttrs.x1 "0"
        , SvgAttrs.y1 "0"
        , SvgAttrs.x2 (String.fromFloat targetX)
        , SvgAttrs.y2 (String.fromFloat targetY)
        , SvgAttrs.stroke "black"
        ]
        []


svgDrawNode : String -> Svg msg
svgDrawNode str =
    Svg.g
        []
        [ Svg.circle
            [ SvgAttrs.r "26"
            , SvgAttrs.stroke "black"
            , SvgAttrs.fill "white"
            , SvgAttrs.cx "0"
            , SvgAttrs.cy "0"
            ]
            []
        , Svg.text_
            [ SvgAttrs.textAnchor "middle"
            , SvgAttrs.transform "translate(0,5)"
            ]
            [ Svg.text str ]
        ]


treeFromExpr : AST.Expr -> TreeDiagram.Tree String
treeFromExpr expr =
    case expr of
        AST.ExprVar name ->
            TreeDiagram.node (Name.toString name) []

        AST.ExprInt int ->
            TreeDiagram.node (String.fromInt int) []

        AST.ExprBool bool ->
            TreeDiagram.node
                (if bool then
                    "True"

                 else
                    "False"
                )
                []

        AST.ExprCall expr1 expr2 ->
            TreeDiagram.node "apply"
                [ treeFromExpr expr1
                , treeFromExpr expr2
                ]

        AST.ExprLambda name expr1 ->
            TreeDiagram.node (Name.toString name)
                [ treeFromExpr expr1
                ]



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = El.layout [] << view
        , update = update
        }
