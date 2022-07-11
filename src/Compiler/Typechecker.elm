module Compiler.Typechecker exposing (..)

import Compiler.AST as AST exposing (Exp(..), Lit(..), Scheme(..), Type(..))
import Dict exposing (Dict)
import Set exposing (Set)


run : Exp -> Result Error Scheme
run e =
    typeInference (Id 0) primitives e
        |> Result.map Tuple.first
        |> Result.map (generalize Dict.empty)


applySubst : Dict String Type -> Type -> Type
applySubst subst ty =
    case ty of
        TVar var ->
            Maybe.withDefault (TVar var) (Dict.get var subst)

        TFun arg res ->
            TFun (applySubst subst arg) (applySubst subst res)

        TInt ->
            TInt

        TBool ->
            TBool


applySubstScheme : Dict String Type -> Scheme -> Scheme
applySubstScheme subst (Scheme vars t) =
    -- The fold takes care of name shadowing
    -- TODO just get rid of shadowing?
    Scheme vars (applySubst (List.foldr Dict.remove subst vars) t)


{-| TODO: I'm not sure what the relevance of that is, but
<https://github.com/kritzcreek/fby19/blob/master/src/Typechecker.hs> has the
following warning: "This is much more subtle than it seems. (union is left biased)"

Also, should simply a `Dict String Type` be used instead of substitution?

-}
composeSubst : Dict String Type -> Dict String Type -> Dict String Type
composeSubst s1 s2 =
    Dict.union (Dict.map (\_ -> applySubst s1) s2) s1


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
varBind : String -> Type -> Result Error (Dict String Type)
varBind var ty =
    if ty == TVar var then
        Ok Dict.empty

    else if Set.member var (freeTypeVars ty) then
        Result.Err OccursCheckFailed

    else
        Ok (Dict.singleton var ty)


unify : Type -> Type -> Result Error (Dict String Type)
unify ty1 ty2 =
    case ( ty1, ty2 ) of
        ( TInt, TInt ) ->
            Ok Dict.empty

        ( TBool, TBool ) ->
            Ok Dict.empty

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
            Err (TypesDoNotUnify t1 t2)


type alias Context =
    Dict String Scheme


applySubstCtx : Dict String Type -> Context -> Context
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


inferLiteral : Lit -> Type
inferLiteral lit =
    case lit of
        LInt _ ->
            TInt

        LBool _ ->
            TBool


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
                    subst : Dict String Type
                    subst =
                        Dict.fromList (List.map2 Tuple.pair vars newVars)
                in
                ( applySubst subst ty, id_ )
           )


infer : Id -> Context -> Exp -> Result Error ( Dict String Type, Type, Id )
infer id ctx exp =
    case exp of
        EVar var ->
            case Dict.get var ctx of
                Nothing ->
                    Err (UnboundVariable var)

                Just scheme ->
                    instantiate id scheme
                        |> (\( ty, id_ ) ->
                                Ok ( Dict.empty, ty, id_ )
                           )

        ELit lit ->
            Ok ( Dict.empty, inferLiteral lit, id )

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

        ELet defs body ->
            inferRecursiveDefs id ctx defs
                |> Result.andThen
                    (\( s1, tmpCtx, id1 ) ->
                        infer id1 (applySubstCtx s1 tmpCtx) body
                            |> Result.map
                                (\( s2, tyBody, id2 ) ->
                                    ( composeSubst s1 s2, tyBody, id2 )
                                )
                    )


inferRecursiveDefs : Id -> Context -> List ( String, Exp ) -> Result Error ( Dict String Type, Context, Id )
inferRecursiveDefs id ctx defs =
    -- FIXME will create an infinite loop if there is a cycle in the definitions
    -- Elm compiler has cycle detection in `canonicalizeLet`
    -- (https://github.com/elm/compiler/blob/master/compiler/src/Canonicalize/Expression.hs#L297)
    -- do sth similar somewhere?
    inferRecursiveDefsHelp id ctx Dict.empty defs


inferRecursiveDefsHelp : Id -> Context -> Dict String Type -> List ( String, Exp ) -> Result Error ( Dict String Type, Context, Id )
inferRecursiveDefsHelp id ctx s defs =
    case defs of
        [] ->
            Ok ( s, ctx, id )

        ( binder, binding ) :: otherDefs ->
            case infer id ctx binding of
                Ok ( s1, tyBinder, id_ ) ->
                    let
                        scheme : Scheme
                        scheme =
                            -- TODO figure out what the comment below is about :D
                            -- https://github.com/kritzcreek/fby19/blob/master/src/Typechecker.hs#L134
                            -- let scheme = generalize ctx (applySubst s1 t1)
                            Scheme [] (applySubst s1 tyBinder)

                        tmpCtx : Context
                        tmpCtx =
                            Dict.insert binder scheme ctx
                    in
                    inferRecursiveDefsHelp id_ tmpCtx (composeSubst s s1) otherDefs

                Err _ ->
                    inferRecursiveDefsHelp id ctx s (otherDefs ++ [ ( binder, binding ) ])


typeInference : Id -> Context -> Exp -> Result Error ( Type, Id )
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



-- ERROR


type Error
    = OccursCheckFailed
    | TypesDoNotUnify Type Type
    | UnboundVariable String


errorToString : Error -> String
errorToString error =
    case error of
        OccursCheckFailed ->
            "Occurs check failed"

        TypesDoNotUnify ty1 ty2 ->
            "Types do not unify: " ++ AST.prettyType ty1 ++ " and " ++ AST.prettyType ty2

        UnboundVariable var ->
            "Unbound variable: " ++ var
