module Compiler.Typechecker.V1 exposing (Error, errorToString, run)

import AssocList as Dict exposing (Dict)
import Compiler.AST as AST
import Data.Name as Name exposing (Name)


run : AST.Expr -> Result Error AST.Annotation
run e =
    typeInference (Id 0) primitives e
        |> Result.map Tuple.first
        |> Result.map (generalize Dict.empty)


applySubstitution : Dict Name AST.Type -> AST.Type -> AST.Type
applySubstitution subst ty =
    case ty of
        AST.TypeVar var ->
            Maybe.withDefault (AST.TypeVar var) (Dict.get var subst)

        AST.TypeLambda arg res ->
            AST.TypeLambda (applySubstitution subst arg) (applySubstitution subst res)

        AST.TypeInt ->
            AST.TypeInt

        AST.TypeBool ->
            AST.TypeBool


applySubstitutionAnnotation : Dict Name AST.Type -> AST.Annotation -> AST.Annotation
applySubstitutionAnnotation subst (AST.Forall vars t) =
    -- The fold takes care of name shadowing
    -- TODO just get rid of shadowing?
    AST.Forall vars (applySubstitution (Dict.diff subst vars) t)


{-| TODO: I'm not sure what the relevance of that is, but
<https://github.com/kritzcreek/fby19/blob/master/src/Typechecker.hs> has the
following warning: "This is much more subtle than it seems. (union is left biased)"
-}
composeSubstitution : Dict Name AST.Type -> Dict Name AST.Type -> Dict Name AST.Type
composeSubstitution s1 s2 =
    Dict.union (Dict.map (\_ -> applySubstitution s1) s2) s1


type Id
    = Id Int


incrementId : Id -> Id
incrementId (Id id) =
    Id (id + 1)


idToString : Id -> String
idToString (Id id) =
    String.fromInt id


newTypeVar : Id -> ( AST.Type, Id )
newTypeVar id =
    ( AST.TypeVar (Name.fromString ("u" ++ idToString id)), incrementId id )


freeTypeVars : AST.Type -> AST.FreeVars
freeTypeVars ty =
    case ty of
        AST.TypeVar var ->
            Dict.singleton var ()

        AST.TypeLambda t1 t2 ->
            Dict.union (freeTypeVars t1) (freeTypeVars t2)

        _ ->
            Dict.empty


freeTypeVarsScheme : AST.Annotation -> AST.FreeVars
freeTypeVarsScheme (AST.Forall vars t) =
    Dict.diff (freeTypeVars t) vars


{-| Creates a fresh unification variable and binds it to the given type
-}
varBind : Name -> AST.Type -> Result Error (Dict Name AST.Type)
varBind var ty =
    if ty == AST.TypeVar var then
        Ok Dict.empty

    else if Dict.member var (freeTypeVars ty) then
        Result.Err OccursCheckFailed

    else
        Ok (Dict.singleton var ty)


unify : AST.Type -> AST.Type -> Result Error (Dict Name AST.Type)
unify ty1 ty2 =
    case ( ty1, ty2 ) of
        ( AST.TypeInt, AST.TypeInt ) ->
            Ok Dict.empty

        ( AST.TypeBool, AST.TypeBool ) ->
            Ok Dict.empty

        ( AST.TypeLambda l r, AST.TypeLambda l_ r_ ) ->
            unify l l_
                |> Result.andThen
                    (\s1 ->
                        unify (applySubstitution s1 r) (applySubstitution s1 r_)
                            |> Result.map (\s2 -> composeSubstitution s1 s2)
                    )

        ( AST.TypeVar u, t ) ->
            varBind u t

        ( t, AST.TypeVar u ) ->
            varBind u t

        ( t1, t2 ) ->
            Err (TypesDoNotUnify t1 t2)


type alias Context =
    Dict Name AST.Annotation


applySubstitutionContext : Dict Name AST.Type -> Context -> Context
applySubstitutionContext subst ctx =
    Dict.map (\_ -> applySubstitutionAnnotation subst) ctx


freeTypeVarsContext : Context -> AST.FreeVars
freeTypeVarsContext ctx =
    List.foldl (\a acc -> Dict.union (freeTypeVarsScheme a) acc) Dict.empty (Dict.values ctx)


generalize : Context -> AST.Type -> AST.Annotation
generalize ctx t =
    let
        vars : AST.FreeVars
        vars =
            Dict.diff (freeTypeVars t) (freeTypeVarsContext ctx)
    in
    AST.Forall vars t


instantiate : Id -> AST.Annotation -> ( AST.Type, Id )
instantiate id (AST.Forall vars ty) =
    List.foldl
        (\_ ( acc, id1 ) ->
            let
                ( tyVar, id2 ) =
                    newTypeVar id1
            in
            ( tyVar :: acc, id2 )
        )
        ( [], id )
        (Dict.keys vars)
        |> (\( newVars, id1 ) ->
                let
                    subst : Dict Name AST.Type
                    subst =
                        Dict.fromList (List.map2 Tuple.pair (Dict.keys vars) newVars)
                in
                ( applySubstitution subst ty, id1 )
           )


infer : Id -> Context -> AST.Expr -> Result Error ( Dict Name AST.Type, AST.Type, Id )
infer id context expr =
    case expr of
        AST.ExprVar var ->
            case Dict.get var context of
                Nothing ->
                    Err (UnboundVariable var)

                Just scheme ->
                    instantiate id scheme
                        |> (\( ty, id_ ) ->
                                Ok ( Dict.empty, ty, id_ )
                           )

        AST.ExprInt _ ->
            Ok ( Dict.empty, AST.TypeInt, id )

        AST.ExprBool _ ->
            Ok ( Dict.empty, AST.TypeBool, id )

        AST.ExprCall fun arg ->
            newTypeVar id
                |> (\( tyRes, id1 ) ->
                        infer id1 context fun
                            |> Result.andThen
                                (\( s1, tyFun, id2 ) ->
                                    infer id2 (applySubstitutionContext s1 context) arg
                                        |> Result.andThen
                                            (\( s2, tyArg, id3 ) ->
                                                unify (applySubstitution s2 tyFun) (AST.TypeLambda tyArg tyRes)
                                                    |> Result.map
                                                        (\s3 ->
                                                            ( composeSubstitution s1 (composeSubstitution s2 s3), applySubstitution s3 tyRes, id3 )
                                                        )
                                            )
                                )
                   )

        AST.ExprLambda binder body ->
            newTypeVar id
                |> (\( tyBinder, id1 ) ->
                        let
                            tmpCtx : Context
                            tmpCtx =
                                Dict.insert binder (AST.Forall Dict.empty tyBinder) context
                        in
                        infer id1 tmpCtx body
                            |> Result.map
                                (\( s1, tyBody, id2 ) ->
                                    ( s1, AST.TypeLambda (applySubstitution s1 tyBinder) tyBody, id2 )
                                )
                   )


typeInference : Id -> Context -> AST.Expr -> Result Error ( AST.Type, Id )
typeInference id ctx exp =
    infer id ctx exp
        |> Result.map (\( s, t, id1 ) -> ( applySubstitution s t, id1 ))


primitives : Context
primitives =
    Dict.fromList
        [ ( Name.fromString "identity"
          , AST.Forall (Dict.singleton (Name.fromString "a") ())
                (AST.TypeLambda (AST.TypeVar (Name.fromString "a")) (AST.TypeVar (Name.fromString "a")))
          )
        , ( Name.fromString "const"
          , AST.Forall (Dict.fromList [ ( Name.fromString "a", () ), ( Name.fromString "b", () ) ])
                (AST.TypeLambda
                    (AST.TypeVar (Name.fromString "a"))
                    (AST.TypeLambda
                        (AST.TypeVar (Name.fromString "b"))
                        (AST.TypeVar (Name.fromString "a"))
                    )
                )
          )
        , ( Name.fromString "add"
          , AST.Forall Dict.empty
                (AST.TypeLambda
                    AST.TypeInt
                    (AST.TypeLambda AST.TypeInt AST.TypeInt)
                )
          )
        , ( Name.fromString "gte"
          , AST.Forall Dict.empty
                (AST.TypeLambda AST.TypeInt (AST.TypeLambda AST.TypeInt AST.TypeBool))
          )
        , ( Name.fromString "if"
          , AST.Forall (Dict.singleton (Name.fromString "a") ())
                (AST.TypeLambda AST.TypeBool
                    (AST.TypeLambda
                        (AST.TypeVar (Name.fromString "a"))
                        (AST.TypeLambda
                            (AST.TypeVar (Name.fromString "a"))
                            (AST.TypeVar (Name.fromString "a"))
                        )
                    )
                )
          )
        ]



-- ERROR


type Error
    = OccursCheckFailed
    | TypesDoNotUnify AST.Type AST.Type
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
