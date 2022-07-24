module Compiler.AST exposing (..)

import AssocList as Dict exposing (Dict)
import Data.Name as Name exposing (Name)


type Exp
    = EVar Name
    | EInt Int
    | EBool Bool
    | EApp Exp Exp
    | ELam Name Exp
    | ELet (List ( Name, Exp )) Exp


type Type
    = TInt
    | TBool
    | TVar Name
    | TFun Type Type


type Annotation
    = Annotation FreeVars Type


type alias FreeVars =
    Dict Name ()



-- PRETTY PRINTING


expToString : Exp -> String
expToString exp =
    case exp of
        EVar s ->
            "(Variable " ++ Name.toString s ++ ")"

        EInt int ->
            "(Int " ++ String.fromInt int ++ ")"

        EBool bool ->
            "(Bool "
                ++ (case bool of
                        True ->
                            "True"

                        False ->
                            "False" ++ ")"
                   )

        EApp e1 e2 ->
            "(" ++ expToString e1 ++ " " ++ expToString e2 ++ ")"

        ELam s e ->
            "(Lambda " ++ Name.toString s ++ " " ++ expToString e ++ ")"

        ELet defs e ->
            let
                defsString =
                    "("
                        ++ List.foldl
                            (\( name, body ) acc ->
                                acc ++ ", (Def " ++ Name.toString name ++ " " ++ expToString body ++ ")"
                            )
                            ""
                            defs
                        ++ ")"
            in
            "(Let " ++ defsString ++ " " ++ expToString e ++ ")"


isFun : Type -> Bool
isFun ty =
    case ty of
        TFun _ _ ->
            True

        _ ->
            False


prettyType : Type -> String
prettyType ty =
    case ty of
        TVar var ->
            Name.toString var

        TInt ->
            "Int"

        TBool ->
            "Bool"

        TFun ty1 ty2 ->
            (if isFun ty1 then
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
                    List.foldl renameVar ty vars_
            in
            "∀ " ++ String.join " " (List.map Tuple.second vars_) ++ ". " ++ prettyType renamedTy


renameVar : ( Name, String ) -> Type -> Type
renameVar ( old, new ) ty =
    case ty of
        TInt ->
            TInt

        TBool ->
            TBool

        TVar var ->
            TVar
                (if var == old then
                    Name.fromString new

                 else
                    var
                )

        TFun t1 t2 ->
            TFun (renameVar ( old, new ) t1) (renameVar ( old, new ) t2)


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
