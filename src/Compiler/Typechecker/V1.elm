module Compiler.Typechecker.V1 exposing (..)

import AssocList as Dict exposing (Dict)
import Compiler.AST as AST exposing (Annotation(..), Expr(..), FreeVars, Type(..))
import Data.Name as Name exposing (Name)


run : Expr -> Result Error Annotation
run e =
    typeInference (Id 0) primitives e
        |> Result.map Tuple.first
        |> Result.map (generalize Dict.empty)


applySubstitution : Dict Name Type -> Type -> Type
applySubstitution subst ty =
    case ty of
        TypeVar var ->
            Maybe.withDefault (TypeVar var) (Dict.get var subst)

        TypeLambda arg res ->
            TypeLambda (applySubstitution subst arg) (applySubstitution subst res)

        TypeInt ->
            TypeInt

        TypeBool ->
            TypeBool


applySubstitutionAnnotation : Dict Name Type -> Annotation -> Annotation
applySubstitutionAnnotation subst (Annotation vars t) =
    -- The fold takes care of name shadowing
    -- TODO just get rid of shadowing?
    Annotation vars (applySubstitution (Dict.diff subst vars) t)


{-| TODO: I'm not sure what the relevance of that is, but
<https://github.com/kritzcreek/fby19/blob/master/src/Typechecker.hs> has the
following warning: "This is much more subtle than it seems. (union is left biased)"
-}
composeSubstitution : Dict Name Type -> Dict Name Type -> Dict Name Type
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


newTypeVar : Id -> ( Type, Id )
newTypeVar id =
    ( TypeVar (Name.fromString ("u" ++ idToString id)), incrementId id )


freeTypeVars : Type -> FreeVars
freeTypeVars ty =
    case ty of
        TypeVar var ->
            Dict.singleton var ()

        TypeLambda t1 t2 ->
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
    if ty == TypeVar var then
        Ok Dict.empty

    else if Dict.member var (freeTypeVars ty) then
        Result.Err OccursCheckFailed

    else
        Ok (Dict.singleton var ty)


unify : Type -> Type -> Result Error (Dict Name Type)
unify ty1 ty2 =
    case ( ty1, ty2 ) of
        ( TypeInt, TypeInt ) ->
            Ok Dict.empty

        ( TypeBool, TypeBool ) ->
            Ok Dict.empty

        ( TypeLambda l r, TypeLambda l_ r_ ) ->
            unify l l_
                |> Result.andThen
                    (\s1 ->
                        unify (applySubstitution s1 r) (applySubstitution s1 r_)
                            |> Result.map (\s2 -> composeSubstitution s1 s2)
                    )

        ( TypeVar u, t ) ->
            varBind u t

        ( t, TypeVar u ) ->
            varBind u t

        ( t1, t2 ) ->
            Err (TypesDoNotUnify t1 t2)


type alias Context =
    Dict Name Annotation


applySubstitutionContext : Dict Name Type -> Context -> Context
applySubstitutionContext subst ctx =
    Dict.map (\_ -> applySubstitutionAnnotation subst) ctx


freeTypeVarsContext : Context -> FreeVars
freeTypeVarsContext ctx =
    List.foldl (\a acc -> Dict.union (freeTypeVarsScheme a) acc) Dict.empty (Dict.values ctx)


generalize : Context -> Type -> Annotation
generalize ctx t =
    let
        vars : FreeVars
        vars =
            Dict.diff (freeTypeVars t) (freeTypeVarsContext ctx)
    in
    Annotation vars t


instantiate : Id -> Annotation -> ( Type, Id )
instantiate id (Annotation vars ty) =
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
                    subst : Dict Name Type
                    subst =
                        Dict.fromList (List.map2 Tuple.pair (Dict.keys vars) newVars)
                in
                ( applySubstitution subst ty, id1 )
           )


infer : Id -> Context -> Expr -> Result Error ( Dict Name Type, Type, Id )
infer id context expr =
    case expr of
        ExprVar var ->
            case Dict.get var context of
                Nothing ->
                    Err (UnboundVariable var)

                Just scheme ->
                    instantiate id scheme
                        |> (\( ty, id_ ) ->
                                Ok ( Dict.empty, ty, id_ )
                           )

        ExprInt _ ->
            Ok ( Dict.empty, TypeInt, id )

        ExprBool _ ->
            Ok ( Dict.empty, TypeBool, id )

        ExprCall fun arg ->
            newTypeVar id
                |> (\( tyRes, id1 ) ->
                        infer id1 context fun
                            |> Result.andThen
                                (\( s1, tyFun, id2 ) ->
                                    infer id2 (applySubstitutionContext s1 context) arg
                                        |> Result.andThen
                                            (\( s2, tyArg, id3 ) ->
                                                unify (applySubstitution s2 tyFun) (TypeLambda tyArg tyRes)
                                                    |> Result.map
                                                        (\s3 ->
                                                            ( composeSubstitution s1 (composeSubstitution s2 s3), applySubstitution s3 tyRes, id3 )
                                                        )
                                            )
                                )
                   )

        ExprLambda binder body ->
            newTypeVar id
                |> (\( tyBinder, id1 ) ->
                        let
                            tmpCtx : Context
                            tmpCtx =
                                Dict.insert binder (Annotation Dict.empty tyBinder) context
                        in
                        infer id1 tmpCtx body
                            |> Result.map
                                (\( s1, tyBody, id2 ) ->
                                    ( s1, TypeLambda (applySubstitution s1 tyBinder) tyBody, id2 )
                                )
                   )


typeInference : Id -> Context -> Expr -> Result Error ( Type, Id )
typeInference id ctx exp =
    infer id ctx exp
        |> Result.map (\( s, t, id1 ) -> ( applySubstitution s t, id1 ))


primitives : Context
primitives =
    Dict.fromList
        [ ( Name.fromString "identity"
          , Annotation (Dict.singleton (Name.fromString "a") ())
                (TypeLambda (TypeVar (Name.fromString "a")) (TypeVar (Name.fromString "a")))
          )
        , ( Name.fromString "const"
          , Annotation (Dict.fromList [ ( Name.fromString "a", () ), ( Name.fromString "b", () ) ])
                (TypeLambda
                    (TypeVar (Name.fromString "a"))
                    (TypeLambda
                        (TypeVar (Name.fromString "b"))
                        (TypeVar (Name.fromString "a"))
                    )
                )
          )
        , ( Name.fromString "add", Annotation Dict.empty (TypeLambda TypeInt (TypeLambda TypeInt TypeInt)) )
        , ( Name.fromString "gte", Annotation Dict.empty (TypeLambda TypeInt (TypeLambda TypeInt TypeBool)) )
        , ( Name.fromString "if"
          , Annotation (Dict.singleton (Name.fromString "a") ())
                (TypeLambda TypeBool
                    (TypeLambda
                        (TypeVar (Name.fromString "a"))
                        (TypeLambda
                            (TypeVar (Name.fromString "a"))
                            (TypeVar (Name.fromString "a"))
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
