module Compiler.Typechecker exposing (..)

import Compiler.AST as AST exposing (Exp(..), Lit(..), Scheme(..), Type(..))
import Dict exposing (Dict)
import Set exposing (Set)


run : Exp -> Result String Scheme
run e =
    typeInference (Id 0) primitives e
        |> Result.map Tuple.first
        |> Result.map (generalize Dict.empty)


type Substitution
    = Substitution (Dict String Type)


substEmpty : Substitution
substEmpty =
    Substitution Dict.empty


substGet : String -> Substitution -> Maybe Type
substGet name (Substitution dict) =
    Dict.get name dict


substRemove : String -> Substitution -> Substitution
substRemove name (Substitution dict) =
    Substitution (Dict.remove name dict)


applySubst : Substitution -> Type -> Type
applySubst subst ty =
    case ty of
        TVar var ->
            Maybe.withDefault (TVar var) (substGet var subst)

        TFun arg res ->
            TFun (applySubst subst arg) (applySubst subst res)

        TInt ->
            TInt

        TBool ->
            TBool


applySubstScheme : Substitution -> Scheme -> Scheme
applySubstScheme subst (Scheme vars t) =
    -- The fold takes care of name shadowing
    Scheme vars (applySubst (List.foldr substRemove subst vars) t)


{-| This is much more subtle than it seems. (union is left biased)
-}
composeSubst : Substitution -> Substitution -> Substitution
composeSubst (Substitution s1) (Substitution s2) =
    Substitution (Dict.union (Dict.map (\_ -> applySubst (Substitution s1)) s2) s1)


type Id
    = Id Int


incrementId : Id -> Id
incrementId (Id id) =
    Id (id + 1)


idToString : Id -> String
idToString (Id id) =
    String.fromInt id


newTyVar : Id -> ( Type, Id )
newTyVar id =
    ( TVar ("u" ++ idToString id), incrementId id )


freeTypeVars : Type -> Set String
freeTypeVars ty =
    case ty of
        TVar var ->
            Set.singleton var

        TFun t1 t2 ->
            Set.union (freeTypeVars t1) (freeTypeVars t2)

        _ ->
            Set.empty


freeTypeVarsScheme : Scheme -> Set String
freeTypeVarsScheme (Scheme vars t) =
    Set.diff (freeTypeVars t) (Set.fromList vars)


{-| Creates a fresh unification variable and binds it to the given type
-}
varBind : String -> Type -> Result String Substitution
varBind var ty =
    if ty == TVar var then
        Ok substEmpty

    else if Set.member var (freeTypeVars ty) then
        Result.Err "occurs check failed"

    else
        Ok (Substitution (Dict.singleton var ty))


unify : Type -> Type -> Result String Substitution
unify ty1 ty2 =
    case ( ty1, ty2 ) of
        ( TInt, TInt ) ->
            Ok substEmpty

        ( TBool, TBool ) ->
            Ok substEmpty

        ( TFun l r, TFun l_ r_ ) ->
            unify l l_
                |> Result.andThen
                    (\s1 ->
                        unify (applySubst s1 r) (applySubst s1 r_)
                            |> Result.map (\s2 -> composeSubst s1 s2)
                    )

        ( TVar u, t ) ->
            varBind u t

        ( t, TVar u ) ->
            varBind u t

        ( t1, t2 ) ->
            Err ("types do not unify: " ++ AST.prettyType t1 ++ " vs. " ++ AST.prettyType t2)


type alias Context =
    Dict String Scheme


applySubstCtx : Substitution -> Context -> Context
applySubstCtx subst ctx =
    Dict.map (\_ -> applySubstScheme subst) ctx


freeTypeVarsCtx : Context -> Set String
freeTypeVarsCtx ctx =
    List.foldl (\a acc -> Set.union (freeTypeVarsScheme a) acc) Set.empty (Dict.values ctx)


generalize : Context -> Type -> Scheme
generalize ctx t =
    let
        vars : List String
        vars =
            Set.toList (Set.diff (freeTypeVars t) (freeTypeVarsCtx ctx))
    in
    Scheme vars t


inferLiteral : Lit -> ( Substitution, Type )
inferLiteral lit =
    ( substEmpty
    , case lit of
        LInt _ ->
            TInt

        LBool _ ->
            TBool
    )


instantiate : Id -> Scheme -> ( Type, Id )
instantiate id (Scheme vars ty) =
    List.foldl
        (\_ ( acc, id_ ) ->
            let
                ( tyVar, id__ ) =
                    newTyVar id_
            in
            ( tyVar :: acc, id__ )
        )
        ( [], id )
        vars
        |> (\( newVars, id_ ) ->
                let
                    subst : Substitution
                    subst =
                        Substitution (Dict.fromList (List.map2 Tuple.pair vars newVars))
                in
                ( applySubst subst ty, id_ )
           )


infer : Id -> Context -> Exp -> Result String ( Substitution, Type, Id )
infer id ctx exp =
    case exp of
        EVar var ->
            case Dict.get var ctx of
                Nothing ->
                    Err ("unbound variable: " ++ var)

                Just scheme ->
                    instantiate id scheme
                        |> (\( ty, id_ ) ->
                                Ok ( substEmpty, ty, id_ )
                           )

        ELit lit ->
            let
                ( subst, ty ) =
                    inferLiteral lit
            in
            Ok ( subst, ty, id )

        EApp fun arg ->
            newTyVar id
                |> (\( tyRes, id1 ) ->
                        infer id1 ctx fun
                            |> Result.andThen
                                (\( s1, tyFun, id2 ) ->
                                    infer id2 (applySubstCtx s1 ctx) arg
                                        |> Result.andThen
                                            (\( s2, tyArg, id3 ) ->
                                                unify (applySubst s2 tyFun) (TFun tyArg tyRes)
                                                    |> Result.map
                                                        (\s3 ->
                                                            ( composeSubst s1 (composeSubst s2 s3), applySubst s3 tyRes, id3 )
                                                        )
                                            )
                                )
                   )

        ELam binder body ->
            newTyVar id
                |> (\( tyBinder, id1 ) ->
                        let
                            tmpCtx : Context
                            tmpCtx =
                                Dict.insert binder (Scheme [] tyBinder) ctx
                        in
                        infer id1 tmpCtx body
                            |> Result.map
                                (\( s1, tyBody, id2 ) ->
                                    ( s1, TFun (applySubst s1 tyBinder) tyBody, id2 )
                                )
                   )

        ELet binder binding body ->
            infer id ctx binding
                |> Result.andThen
                    (\( s1, tyBinder, id1 ) ->
                        let
                            -- let scheme = generalize ctx (applySubst s1 t1)
                            scheme : Scheme
                            scheme =
                                Scheme [] (applySubst s1 tyBinder)

                            tmpCtx : Context
                            tmpCtx =
                                Dict.insert binder scheme ctx
                        in
                        infer id1 (applySubstCtx s1 tmpCtx) body
                            |> Result.map
                                (\( s2, tyBody, id2 ) ->
                                    ( composeSubst s1 s2, tyBody, id2 )
                                )
                    )


typeInference : Id -> Context -> Exp -> Result String ( Type, Id )
typeInference id ctx exp =
    infer id ctx exp
        |> Result.map (\( s, t, id1 ) -> ( applySubst s t, id1 ))


primitives : Context
primitives =
    Dict.fromList
        [ ( "identity", Scheme [ "a" ] (TFun (TVar "a") (TVar "a")) )
        , ( "const", Scheme [ "a", "b" ] (TFun (TVar "a") (TFun (TVar "b") (TVar "a"))) )
        , ( "add", Scheme [] (TFun TInt (TFun TInt TInt)) )
        , ( "gte", Scheme [] (TFun TInt (TFun TInt TBool)) )
        , ( "if", Scheme [ "a" ] (TFun TBool (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))) )
        ]
