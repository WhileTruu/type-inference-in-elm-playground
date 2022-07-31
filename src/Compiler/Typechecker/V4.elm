module Compiler.Typechecker.V4 exposing (errorToString, getSubstitutions, run)

{-| A step towards Elm-compiler-like constraint based type checking.
-}

import AssocList as Dict exposing (Dict)
import Compiler.AST exposing (Annotation(..), Expr(..), FreeVars, Type(..))
import Data.Name as Name exposing (Name)



-- RUN


run : Expr -> Result TypeError Annotation
run expr =
    infer expr
        |> Result.map (\( s, t ) -> applySubst s t)
        |> Result.map (generalize (TypeEnv Dict.empty))


infer : Expr -> Result TypeError ( Dict Name Type, Type )
infer expr =
    let
        ( expectedType, id ) =
            fresh (Id 0)
    in
    constrain id primitives expr expectedType
        |> Result.andThen
            (\( constraint, _ ) ->
                Result.map (\a -> ( a, expectedType )) (solve constraint nullSubst)
            )


getSubstitutions : Expr -> Result TypeError (Dict Name Type)
getSubstitutions expr =
    infer expr
        |> Result.map (\( s, t ) -> Dict.insert (Name.fromString "u0") t s)


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



-- GENERALIZATION


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



-- TYPING RULES


type Constraint
    = CEqual Type Type
    | CAnd (List Constraint)


constrain : Id -> TypeEnv -> Expr -> Type -> Result TypeError ( Constraint, Id )
constrain id env exp expected =
    case exp of
        ExprVar var ->
            lookupEnv id env var
                |> Result.map (\( varType, id1 ) -> ( CEqual varType expected, id1 ))

        ExprLambda arg body ->
            constrainLambda id env arg body expected

        ExprCall func arg ->
            constrainCall id env func arg expected

        ExprInt _ ->
            Ok ( CEqual TypeInt expected, id )

        ExprBool _ ->
            Ok ( CEqual TypeBool expected, id )


constrainLambda : Id -> TypeEnv -> Name -> Expr -> Type -> Result TypeError ( Constraint, Id )
constrainLambda id env arg body expected =
    let
        ( argType, id1 ) =
            fresh id

        ( resultType, id2 ) =
            fresh id1

        tmpEnv : TypeEnv
        tmpEnv =
            extend env ( arg, Forall Dict.empty argType )
    in
    constrain id2 tmpEnv body resultType
        |> Result.map
            (\( bodyCon, id3 ) ->
                ( CAnd
                    [ bodyCon
                    , CEqual (TypeLambda argType resultType) expected
                    ]
                , id3
                )
            )


constrainCall : Id -> TypeEnv -> Expr -> Expr -> Type -> Result TypeError ( Constraint, Id )
constrainCall id env func arg expected =
    let
        ( funcType, id1 ) =
            fresh id

        ( argType, id2 ) =
            fresh id1

        ( resultType, id3 ) =
            fresh id2
    in
    constrain id3 env func funcType
        |> Result.andThen
            (\( funcCon, id4 ) ->
                constrain id4 env arg argType
                    |> Result.map
                        (\( argCon, id5 ) ->
                            ( CAnd
                                [ funcCon
                                , CEqual funcType (TypeLambda argType resultType)
                                , argCon
                                , CEqual resultType expected
                                ]
                            , id5
                            )
                        )
            )


lookupEnv : Id -> TypeEnv -> Name -> Result TypeError ( Type, Id )
lookupEnv id env x =
    case Dict.get x ((\(TypeEnv a) -> a) env) of
        Nothing ->
            Err (UnboundVariable x)

        Just scheme ->
            Ok (instantiate id scheme)


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


solve : Constraint -> Subst -> Result TypeError Subst
solve constraint subst =
    case constraint of
        CEqual t1 t2 ->
            unifies (applySubst subst t1) (applySubst subst t2)

        CAnd constraints ->
            List.foldl
                (\constraint1 ->
                    Result.andThen
                        (\subst1 ->
                            Result.map (\a -> composeSubst a subst1) (solve constraint1 subst1)
                        )
                )
                (Ok subst)
                constraints



-- ERROR


type TypeError
    = UnificationFail Type Type
    | InfiniteType Type Type
    | UnboundVariable Name
    | UnificationMismatch (List Type) (List Type)


errorToString : TypeError -> String
errorToString error =
    Debug.toString error
