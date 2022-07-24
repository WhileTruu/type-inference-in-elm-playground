module Compiler.AST exposing (..)

import AssocList as Dict exposing (Dict)
import Data.Name as Name exposing (Name)


type Expr
    = ExprVar Name
    | ExprInt Int
    | ExprBool Bool
    | ExprCall Expr Expr
    | ExprLambda Name Expr


type Type
    = TypeInt
    | TypeBool
    | TypeVar Name
    | TypeLambda Type Type


type Annotation
    = Annotation FreeVars Type


type alias FreeVars =
    Dict Name ()



-- PRETTY PRINTING


exprToString : Expr -> String
exprToString exp =
    case exp of
        ExprVar s ->
            "(Variable " ++ Name.toString s ++ ")"

        ExprInt int ->
            "(Int " ++ String.fromInt int ++ ")"

        ExprBool bool ->
            "(Bool "
                ++ (case bool of
                        True ->
                            "True"

                        False ->
                            "False" ++ ")"
                   )

        ExprCall e1 e2 ->
            "(" ++ exprToString e1 ++ " " ++ exprToString e2 ++ ")"

        ExprLambda s e ->
            "(Lambda " ++ Name.toString s ++ " " ++ exprToString e ++ ")"


isTypeLambda : Type -> Bool
isTypeLambda ty =
    case ty of
        TypeLambda _ _ ->
            True

        _ ->
            False


prettyType : Type -> String
prettyType ty =
    case ty of
        TypeVar var ->
            Name.toString var

        TypeInt ->
            "Int"

        TypeBool ->
            "Bool"

        TypeLambda ty1 ty2 ->
            (if isTypeLambda ty1 then
                "(" ++ prettyType ty1 ++ ")"

             else
                prettyType ty1
            )
                ++ " -> "
                ++ prettyType ty2


prettyScheme : Annotation -> String
prettyScheme (Annotation vars ty) =
    case Dict.keys vars of
        [] ->
            prettyType ty

        _ ->
            let
                vars_ : List ( Name, String )
                vars_ =
                    List.indexedMap
                        (\i var -> ( var, generateVarName i ))
                        (List.sortBy (Name.toString >> String.dropLeft 1 >> String.toInt >> Maybe.withDefault 0)
                            (Dict.keys vars)
                        )

                renamedTy : Type
                renamedTy =
                    List.foldl renameTypeVar ty vars_
            in
            "∀ " ++ String.join " " (List.map Tuple.second vars_) ++ ". " ++ prettyType renamedTy


renameTypeVar : ( Name, String ) -> Type -> Type
renameTypeVar ( old, new ) ty =
    case ty of
        TypeInt ->
            TypeInt

        TypeBool ->
            TypeBool

        TypeVar var ->
            TypeVar
                (if var == old then
                    Name.fromString new

                 else
                    var
                )

        TypeLambda t1 t2 ->
            TypeLambda (renameTypeVar ( old, new ) t1) (renameTypeVar ( old, new ) t2)


generateVarName : Int -> String
generateVarName i =
    let
        char : Char
        char =
            Char.fromCode (97 + modBy 26 i)

        suffix : Int
        suffix =
            i // 26
    in
    String.fromChar char
        ++ (if suffix == 0 then
                ""

            else
                String.fromInt suffix
           )
