module Compiler.AST exposing (..)


type Exp
    = EVar String
    | EInt Int
    | EBool Bool
    | EApp Exp Exp
    | ELam String Exp
    | ELet (List ( String, Exp )) Exp


type Type
    = TInt
    | TBool
    | TVar String
    | TFun Type Type


type Scheme
    = Scheme (List String) Type



-- PRETTY PRINTING


expToString : Exp -> String
expToString exp =
    case exp of
        EVar s ->
            "(Variable " ++ s ++ ")"

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
            "(Lambda " ++ s ++ " " ++ expToString e ++ ")"

        ELet defs e ->
            let
                defsString =
                    "("
                        ++ List.foldl
                            (\( name, body ) acc ->
                                acc ++ ", (Def " ++ name ++ " " ++ expToString body ++ ")"
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
            var

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


prettyScheme : Scheme -> String
prettyScheme (Scheme vars ty) =
    case vars of
        [] ->
            prettyType ty

        _ ->
            let
                vars_ : List ( String, String )
                vars_ =
                    List.indexedMap
                        (\i var -> ( var, generateVarName i ))
                        (List.sortBy (String.dropLeft 1 >> String.toInt >> Maybe.withDefault 0)
                            vars
                        )

                renamedTy : Type
                renamedTy =
                    List.foldl renameVar ty vars_
            in
            "∀ " ++ String.join " " (List.map Tuple.second vars_) ++ ". " ++ prettyType renamedTy


renameVar : ( String, String ) -> Type -> Type
renameVar ( old, new ) ty =
    case ty of
        TInt ->
            TInt

        TBool ->
            TBool

        TVar var ->
            TVar
                (if var == old then
                    new

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
