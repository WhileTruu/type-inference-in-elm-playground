module Compiler.Typechecker.V3 exposing (..)

{-| From WYAH
-}

import AssocList as Dict exposing (Dict)
import Compiler.AST exposing (Annotation(..), Expr(..), FreeVars, Type(..))
import Data.Name as Name exposing (Name)



-- RUN


run : Expr -> Result TypeError Annotation
run e =
    infer e
        { id = Id 0
        , env = primitives
        , constraints = []
        }
        |> Result.andThen
            (\( t, stuff ) ->
                solve stuff.constraints
                    |> Result.map (\s -> applySubst s t)
            )
        |> Result.map (generalize (TypeEnv Dict.empty))


primitives : TypeEnv
primitives =
    TypeEnv <|
        Dict.fromList
            [ ( Name.fromString "identity"
              , Forall (Dict.singleton (Name.fromString "a") ())
                    (TypeLambda (TypeVar (Name.fromString "a")) (TypeVar (Name.fromString "a")))
              )
            , ( Name.fromString "const"
              , Forall (Dict.fromList [ ( Name.fromString "a", () ), ( Name.fromString "b", () ) ])
                    (TypeLambda
                        (TypeVar (Name.fromString "a"))
                        (TypeLambda
                            (TypeVar (Name.fromString "b"))
                            (TypeVar (Name.fromString "a"))
                        )
                    )
              )
            , ( Name.fromString "add", Forall Dict.empty (TypeLambda TypeInt (TypeLambda TypeInt TypeInt)) )
            , ( Name.fromString "gte", Forall Dict.empty (TypeLambda TypeInt (TypeLambda TypeInt TypeBool)) )
            , ( Name.fromString "if"
              , Forall (Dict.singleton (Name.fromString "a") ())
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



-- CONTEXT


type TypeEnv
    = TypeEnv (Dict Name Annotation)


extend : TypeEnv -> ( Name, Annotation ) -> TypeEnv
extend (TypeEnv env) ( name, scheme ) =
    TypeEnv (Dict.insert name scheme env)



-- NEW TYPE, ID


type Id
    = Id Int


fresh : Id -> ( Type, Id )
fresh (Id id) =
    ( TypeVar (Name.fromString ("u" ++ String.fromInt id)), Id (id + 1) )



-- SUBSTITUTION


type alias Subst =
    Dict Name Type


nullSubst : Subst
nullSubst =
    Dict.empty


composeSubst : Subst -> Subst -> Subst
composeSubst s1 s2 =
    Dict.union (Dict.map (\_ -> applySubst s1) s2) s1


applySubst : Subst -> Type -> Type
applySubst subst type_ =
    case type_ of
        TypeVar var ->
            Maybe.withDefault (TypeVar var) (Dict.get var subst)

        TypeLambda arg res ->
            TypeLambda (applySubst subst arg) (applySubst subst res)

        TypeInt ->
            TypeInt

        TypeBool ->
            TypeBool



-- UNIFICATION


unify : Type -> Type -> Result TypeError Subst
unify ty1 ty2 =
    case ( ty1, ty2 ) of
        ( TypeLambda l r, TypeLambda l_ r_ ) ->
            unify l l_
                |> Result.andThen
                    (\s1 ->
                        unify (applySubst s1 r) (applySubst s1 r_)
                            |> Result.map (\s2 -> composeSubst s1 s2)
                    )

        ( TypeVar a, t ) ->
            bind a t

        ( t, TypeVar a ) ->
            bind a t

        ( TypeInt, TypeInt ) ->
            Ok nullSubst

        ( TypeBool, TypeBool ) ->
            Ok nullSubst

        ( t1, t2 ) ->
            Err (UnificationFail t1 t2)


{-| Creates a fresh unification variable and binds it to the given type
-}
bind : Name -> Type -> Result TypeError Subst
bind a t =
    if t == TypeVar a then
        Ok Dict.empty

    else if occursCheck a t then
        Result.Err (InfiniteType (TypeVar a) t)

    else
        Ok (Dict.singleton a t)


occursCheck : Name -> Type -> Bool
occursCheck a t =
    Dict.member a (ftv t)


ftv : Type -> FreeVars
ftv ty =
    case ty of
        TypeVar var ->
            Dict.singleton var ()

        TypeLambda t1 t2 ->
            Dict.union (ftv t1) (ftv t2)

        TypeInt ->
            Dict.empty

        TypeBool ->
            Dict.empty



-- GENERALIZATION


instantiate : Id -> Annotation -> ( Type, Id )
instantiate id (Forall vars ty) =
    List.foldl
        (\_ ( acc, id_ ) ->
            let
                ( tyVar, id__ ) =
                    fresh id_
            in
            ( tyVar :: acc, id__ )
        )
        ( [], id )
        (Dict.keys vars)
        |> (\( newVars, id_ ) ->
                let
                    subst : Dict Name Type
                    subst =
                        Dict.fromList
                            (List.map2 Tuple.pair
                                (Dict.keys vars)
                                newVars
                            )
                in
                ( applySubst subst ty, id_ )
           )


generalize : TypeEnv -> Type -> Annotation
generalize env t =
    let
        vars : FreeVars
        vars =
            Dict.diff (ftv t) (ftvEnv env)
    in
    Forall vars t


ftvEnv : TypeEnv -> FreeVars
ftvEnv (TypeEnv ctx) =
    List.foldl (\a acc -> Dict.union (ftvAnnotation a) acc) Dict.empty (Dict.values ctx)


ftvAnnotation : Annotation -> FreeVars
ftvAnnotation (Forall vars t) =
    Dict.diff (ftv t) vars



-- TYPING RULES


type alias Stuff =
    { id : Id
    , env : TypeEnv
    , constraints : List ( Type, Type )
    }


uni : Type -> Type -> Stuff -> Stuff
uni t1 t2 a =
    { a | constraints = ( t1, t2 ) :: a.constraints }


infer : Expr -> Stuff -> Result TypeError ( Type, Stuff )
infer exp ({ id, env, constraints } as stuff) =
    case exp of
        ExprVar var ->
            lookupEnv var stuff

        ExprLambda x e ->
            let
                ( tv, id1 ) =
                    fresh id

                tmpEnv : TypeEnv
                tmpEnv =
                    extend env ( x, Forall Dict.empty tv )
            in
            infer e { stuff | id = id1, env = tmpEnv }
                |> Result.map
                    (\( t, stuff1 ) ->
                        ( TypeLambda tv t, { stuff1 | env = stuff.env } )
                    )

        ExprCall e1 e2 ->
            infer e1 stuff
                |> Result.andThen
                    (\( t1, stuff1 ) ->
                        infer e2 stuff1
                            |> Result.map
                                (\( t2, stuff2 ) ->
                                    let
                                        ( tv, id1 ) =
                                            fresh stuff2.id
                                    in
                                    ( tv, uni t1 (TypeLambda t2 tv) { stuff2 | id = id1 } )
                                )
                    )

        ExprInt _ ->
            Ok ( TypeInt, stuff )

        ExprBool _ ->
            Ok ( TypeBool, stuff )


lookupEnv : Name -> Stuff -> Result TypeError ( Type, Stuff )
lookupEnv x ({ env, id } as stuff) =
    case Dict.get x ((\(TypeEnv a) -> a) env) of
        Nothing ->
            Err (UnboundVariable x)

        Just scheme ->
            instantiate id scheme
                |> (\( t, id1 ) ->
                        Ok ( t, { stuff | id = id1 } )
                   )



-- CONSTRAINT SOLVER


unifies : Type -> Type -> Result TypeError Subst
unifies t1 t2 =
    if t1 == t2 then
        Ok nullSubst

    else
        case ( t1, t2 ) of
            ( TypeVar a, t ) ->
                bind a t

            ( t, TypeVar a ) ->
                bind a t

            ( TypeLambda l r, TypeLambda l_ r_ ) ->
                unifyMany [ l, r ] [ l_, r_ ]

            ( _, _ ) ->
                Err (UnificationFail t1 t2)


unifyMany : List Type -> List Type -> Result TypeError Subst
unifyMany xs1 xs2 =
    case ( xs1, xs2 ) of
        ( [], [] ) ->
            Ok nullSubst

        ( t1 :: ts1, t2 :: ts2 ) ->
            unifies t1 t2
                |> Result.andThen
                    (\su1 ->
                        unifyMany (List.map (applySubst su1) ts1) (List.map (applySubst su1) ts2)
                            |> Result.map (Dict.union su1)
                    )

        ( t1, t2 ) ->
            Err (UnificationMismatch t1 t2)


solve : List ( Type, Type ) -> Result TypeError Subst
solve constraints =
    case constraints of
        [] ->
            Ok nullSubst

        ( t1, t2 ) :: cs0 ->
            unifies t1 t2
                |> Result.andThen
                    (\su1 ->
                        solve
                            (List.map (Tuple.mapBoth (applySubst su1) (applySubst su1))
                                cs0
                            )
                            |> Result.map
                                (\su2 ->
                                    composeSubst su2 su1
                                )
                    )



-- ERROR


type TypeError
    = UnificationFail Type Type
    | InfiniteType Type Type
    | UnboundVariable Name
    | UnificationMismatch (List Type) (List Type)
