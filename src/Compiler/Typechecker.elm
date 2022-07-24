module Compiler.Typechecker exposing (..)

import AssocList as Dict exposing (Dict)
import Compiler.AST as AST exposing (Annotation(..), Exp(..), FreeVars, Type(..))
import Data.Name as Name exposing (Name)


run : Exp -> Result Error Annotation
run e =
    typeInference (Id 0) primitives e
        |> Result.map Tuple.first
        |> Result.map (generalize Dict.empty)


applySubst : Dict Name Type -> Type -> Type
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


applySubstScheme : Dict Name Type -> Annotation -> Annotation
applySubstScheme subst (Annotation vars t) =
    -- The fold takes care of name shadowing
    -- TODO just get rid of shadowing?
    Annotation vars (applySubst (Dict.diff subst vars) t)


{-| TODO: I'm not sure what the relevance of that is, but
<https://github.com/kritzcreek/fby19/blob/master/src/Typechecker.hs> has the
following warning: "This is much more subtle than it seems. (union is left biased)"
-}
composeSubst : Dict Name Type -> Dict Name Type -> Dict Name Type
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
    ( TVar (Name.fromString ("u" ++ idToString id)), incrementId id )


freeTypeVars : Type -> FreeVars
freeTypeVars ty =
    case ty of
        TVar var ->
            Dict.singleton var ()

        TFun t1 t2 ->
            Dict.union (freeTypeVars t1) (freeTypeVars t2)

        _ ->
            Dict.empty


freeTypeVarsScheme : Annotation -> FreeVars
freeTypeVarsScheme (Annotation vars t) =
    Dict.diff (freeTypeVars t) vars


{-| Creates a fresh unification variable and binds it to the given type
-}
varBind : Name -> Type -> Result Error (Dict Name Type)
varBind var ty =
    if ty == TVar var then
        Ok Dict.empty

    else if Dict.member var (freeTypeVars ty) then
        Result.Err OccursCheckFailed

    else
        Ok (Dict.singleton var ty)


unify : Type -> Type -> Result Error (Dict Name Type)
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
    Dict Name Annotation


applySubstCtx : Dict Name Type -> Context -> Context
applySubstCtx subst ctx =
    Dict.map (\_ -> applySubstScheme subst) ctx


freeTypeVarsCtx : Context -> FreeVars
freeTypeVarsCtx ctx =
    List.foldl (\a acc -> Dict.union (freeTypeVarsScheme a) acc) Dict.empty (Dict.values ctx)


generalize : Context -> Type -> Annotation
generalize ctx t =
    let
        vars : FreeVars
        vars =
            Dict.diff (freeTypeVars t) (freeTypeVarsCtx ctx)
    in
    Annotation vars t


instantiate : Id -> Annotation -> ( Type, Id )
instantiate id (Annotation vars ty) =
    List.foldl
        (\_ ( acc, id1 ) ->
            let
                ( tyVar, id2 ) =
                    newTyVar id1
            in
            ( tyVar :: acc, id2 )
        )
        ( [], id )
        (Dict.keys vars)
        |> (\( newVars, id1 ) ->
                let
                    subst : Dict Name Type
                    subst =
                        Dict.fromList (List.map2 Tuple.pair (Dict.keys vars) newVars)
                in
                ( applySubst subst ty, id1 )
           )


infer : Id -> Context -> Exp -> Result Error ( Dict Name Type, Type, Id )
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

        EInt _ ->
            Ok ( Dict.empty, TInt, id )

        EBool _ ->
            Ok ( Dict.empty, TBool, id )

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
                                Dict.insert binder (Annotation Dict.empty tyBinder) ctx
                        in
                        infer id1 tmpCtx body
                            |> Result.map
                                (\( s1, tyBody, id2 ) ->
                                    ( s1, TFun (applySubst s1 tyBinder) tyBody, id2 )
                                )
                   )


typeInference : Id -> Context -> Exp -> Result Error ( Type, Id )
typeInference id ctx exp =
    infer id ctx exp
        |> Result.map (\( s, t, id1 ) -> ( applySubst s t, id1 ))


primitives : Context
primitives =
    Dict.fromList
        [ ( Name.fromString "identity"
          , Annotation (Dict.singleton (Name.fromString "a") ())
                (TFun (TVar (Name.fromString "a")) (TVar (Name.fromString "a")))
          )
        , ( Name.fromString "const"
          , Annotation (Dict.fromList [ ( Name.fromString "a", () ), ( Name.fromString "b", () ) ])
                (TFun
                    (TVar (Name.fromString "a"))
                    (TFun
                        (TVar (Name.fromString "b"))
                        (TVar (Name.fromString "a"))
                    )
                )
          )
        , ( Name.fromString "add", Annotation Dict.empty (TFun TInt (TFun TInt TInt)) )
        , ( Name.fromString "gte", Annotation Dict.empty (TFun TInt (TFun TInt TBool)) )
        , ( Name.fromString "if"
          , Annotation (Dict.singleton (Name.fromString "a") ())
                (TFun TBool
                    (TFun
                        (TVar (Name.fromString "a"))
                        (TFun
                            (TVar (Name.fromString "a"))
                            (TVar (Name.fromString "a"))
                        )
                    )
                )
          )
        ]



-- ERROR


type Error
    = OccursCheckFailed
    | TypesDoNotUnify Type Type
    | UnboundVariable Name


errorToString : Error -> String
errorToString error =
    case error of
        OccursCheckFailed ->
            "Occurs check failed"

        TypesDoNotUnify ty1 ty2 ->
            "Types do not unify: " ++ AST.prettyType ty1 ++ " and " ++ AST.prettyType ty2

        UnboundVariable name ->
            "Unbound variable: " ++ Name.toString name
