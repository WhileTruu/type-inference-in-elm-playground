module Main exposing (main)

import AssocList as Dict exposing (Dict)
import Browser
import Compiler.AST as AST exposing (Expr)
import Compiler.Parser as Parser
import Compiler.Typechecker.V1 as TypecheckerV1
import Compiler.Typechecker.V2 as TypecheckerV2
import Compiler.Typechecker.V3 as TypecheckerV3
import Compiler.Typechecker.V4 as TypecheckerV4
import Compiler.Typechecker.V5 as TypecheckerV5
import Data.Name as Name exposing (Name)
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
    , typechecker : Typechecker
    }


type Typechecker
    = TypecheckerV1
    | TypecheckerV2
    | TypecheckerV3
    | TypecheckerV4
    | TypecheckerV5


allTypecheckers : List Typechecker
allTypecheckers =
    allTypecheckersStartingWith TypecheckerV1


allTypecheckersStartingWith : Typechecker -> List Typechecker
allTypecheckersStartingWith typechecker =
    case typechecker of
        TypecheckerV1 ->
            TypecheckerV1 :: allTypecheckersStartingWith TypecheckerV2

        TypecheckerV2 ->
            TypecheckerV2 :: allTypecheckersStartingWith TypecheckerV3

        TypecheckerV3 ->
            TypecheckerV3 :: allTypecheckersStartingWith TypecheckerV4

        TypecheckerV4 ->
            TypecheckerV4 :: allTypecheckersStartingWith TypecheckerV5

        TypecheckerV5 ->
            TypecheckerV5 :: []


typecheckerName : Typechecker -> String
typecheckerName typechecker =
    case typechecker of
        TypecheckerV1 ->
            "Typechecker V1"

        TypecheckerV2 ->
            "Typechecker V2"

        TypecheckerV3 ->
            "Typechecker V3"

        TypecheckerV4 ->
            "Typechecker V4"

        TypecheckerV5 ->
            "Typechecker V5"


getSubstitutions : Typechecker -> Expr -> Dict Name AST.Type
getSubstitutions typechecker expr =
    case typechecker of
        TypecheckerV1 ->
            TypecheckerV1.getSubstitutions expr
                |> Result.withDefault Dict.empty

        TypecheckerV2 ->
            TypecheckerV2.getSubstitutions expr
                |> Result.withDefault Dict.empty

        TypecheckerV3 ->
            TypecheckerV3.getSubstitutions expr
                |> Result.withDefault Dict.empty

        TypecheckerV4 ->
            TypecheckerV4.getSubstitutions expr
                |> Result.withDefault Dict.empty

        TypecheckerV5 ->
            TypecheckerV5.getSubstitutions expr
                |> Result.withDefault Dict.empty


runTypechecker : Typechecker -> Expr -> Result String AST.Annotation
runTypechecker typechecker expr =
    case typechecker of
        TypecheckerV1 ->
            TypecheckerV1.run expr
                |> Result.mapError TypecheckerV1.errorToString

        TypecheckerV2 ->
            TypecheckerV2.run expr
                |> Result.mapError TypecheckerV2.errorToString

        TypecheckerV3 ->
            TypecheckerV3.run expr
                |> Result.mapError TypecheckerV3.errorToString

        TypecheckerV4 ->
            TypecheckerV4.run expr
                |> Result.mapError TypecheckerV4.errorToString

        TypecheckerV5 ->
            TypecheckerV5.run expr
                |> Result.mapError TypecheckerV5.errorToString


initialModel : Model
initialModel =
    { input = ""
    , output = Err "No expression"
    , typechecker = TypecheckerV5
    }



-- UPDATE


type Msg
    = ParseClicked
    | InputChanged String
    | ChangeTypecheckerClicked Typechecker


update : Msg -> Model -> Model
update msg model =
    case msg of
        ParseClicked ->
            { model | output = Parser.run model.input }

        InputChanged input ->
            { model | input = input }

        ChangeTypecheckerClicked typechecker ->
            { model | typechecker = typechecker }



-- VIEW


view : Model -> El.Element Msg
view model =
    El.row [ El.width El.fill, El.height El.fill ]
        [ El.column
            [ El.alignTop
            , El.spacing 8
            , El.height El.fill
            , ElBackground.color (El.rgb255 230 230 230)
            , El.padding 16
            ]
            (List.map
                (\example ->
                    button [ ElBackground.color (El.rgb255 255 255 255) ]
                        { onPress = Just (InputChanged example.code)
                        , label = El.el [ El.centerX ] (El.text example.name)
                        }
                )
                Examples.examples
            )
        , El.column [ El.padding 16, El.alignTop, El.spacing 30, El.width El.fill ]
            [ ElInput.multiline []
                { onChange = InputChanged
                , text = model.input
                , placeholder = Nothing
                , label = ElInput.labelHidden "code input"
                , spellcheck = False
                }
            , button [ ElBackground.color (El.rgb255 200 255 200), El.width El.shrink ]
                { onPress = Just ParseClicked
                , label = El.text "Parse"
                }
            , El.column [ El.width El.fill, El.height El.fill, El.spacing 30 ]
                (case model.output of
                    Ok expr ->
                        viewExprStuff model.typechecker expr

                    Err err ->
                        [ El.text <| "Error: " ++ err ]
                )
            ]
        ]


viewExprStuff : Typechecker -> Expr -> List (El.Element Msg)
viewExprStuff typechecker expr =
    [ El.el [] (El.text (AST.exprToString expr))
    , El.row [ El.spacing 8 ]
        (List.map
            (\typechecker1 ->
                button [ ElBackground.color (El.rgb255 2 255 255) ]
                    { onPress = Just (ChangeTypecheckerClicked typechecker1)
                    , label = El.el [ El.centerX ] (El.text (typecheckerName typechecker1))
                    }
            )
            allTypecheckers
        )
    , El.text ("Selected typechecker: " ++ typecheckerName typechecker)
    , El.el []
        (case runTypechecker typechecker expr of
            Ok output ->
                El.text <| "Success: " ++ AST.prettyScheme output

            Err err ->
                El.text <| "Error: " ++ err
        )
    , El.table [ El.width El.shrink ]
        { data = Dict.toList (getSubstitutions typechecker expr)
        , columns =
            [ { header = El.text "Name"
              , width = El.fill
              , view =
                    \( name, _ ) ->
                        El.text (Name.toString name)
              }
            , { header = El.text "Type"
              , width = El.fill
              , view =
                    \( _, type_ ) ->
                        El.text (AST.prettyType type_)
              }
            ]
        }
    , El.el [ El.height El.shrink, El.width El.shrink ] (viewExprTree expr)
    ]


button : List (El.Attribute msg) -> { onPress : Maybe msg, label : El.Element msg } -> El.Element msg
button attrs =
    ElInput.button
        ([ El.paddingXY 20 0
         , El.height (El.px 57)
         , ElBorder.rounded 14
         , El.width El.fill
         ]
            ++ attrs
        )


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
