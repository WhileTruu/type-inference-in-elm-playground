module Compiler.Typechecker.V5 exposing (errorToString, getSubstitutions, run)

{-| A step towards Elm-compiler-like constraint based type checking.
-}

import AssocList as Dict exposing (Dict)
import Compiler.AST as AST exposing (Annotation(..), Expr(..), FreeVars, Type(..))
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
        |> (\( constraint, _ ) ->
                Result.map (\a -> ( a, expectedType )) (solve primitives constraint nullSubst)
           )


getSubstitutions : Expr -> Result TypeError (Dict Name Type)
getSubstitutions expr =
    infer expr
        |> Result.map (\( s, t ) -> Dict.insert (Name.fromString "u0") t s)


primitives : RTV
primitives =
    Dict.fromList
        [ ( Name.fromString "identity"
          , TypeLambda (TypeVar (Name.fromString "a")) (TypeVar (Name.fromString "a"))
          )
        , ( Name.fromString "const"
          , TypeLambda
                (TypeVar (Name.fromString "a"))
                (TypeLambda
                    (TypeVar (Name.fromString "b"))
                    (TypeVar (Name.fromString "a"))
                )
          )
        , ( Name.fromString "add", TypeLambda TypeInt (TypeLambda TypeInt TypeInt) )
        , ( Name.fromString "gte", TypeLambda TypeInt (TypeLambda TypeInt TypeBool) )
        , ( Name.fromString "if"
          , TypeLambda TypeBool
                (TypeLambda
                    (TypeVar (Name.fromString "a"))
                    (TypeLambda
                        (TypeVar (Name.fromString "a"))
                        (TypeVar (Name.fromString "a"))
                    )
                )
          )
        ]



-- CONTEXT


type TypeEnv
    = TypeEnv (Dict Name Annotation)



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
    | CLocal Name Type
    | CLet
        { header : ( Name, Type )
        , bodyCon : Constraint
        }


type alias RTV =
    Dict Name Type


constrain : Id -> RTV -> Expr -> Type -> ( Constraint, Id )
constrain id rtv exp expected =
    case exp of
        ExprVar var ->
            ( CLocal var expected, id )

        ExprLambda arg body ->
            constrainLambda id rtv arg body expected

        ExprCall func arg ->
            constrainCall id rtv func arg expected

        ExprInt _ ->
            ( CEqual TypeInt expected, id )

        ExprBool _ ->
            ( CEqual TypeBool expected, id )


constrainLambda : Id -> RTV -> Name -> Expr -> Type -> ( Constraint, Id )
constrainLambda id rtv arg body expected =
    let
        ( argType, id1 ) =
            fresh id

        ( resultType, id2 ) =
            fresh id1

        ( bodyCon, id3 ) =
            constrain id2 rtv body resultType
    in
    ( CAnd
        [ CLet
            { header = ( arg, argType )
            , bodyCon = bodyCon
            }
        , CEqual (TypeLambda argType resultType) expected
        ]
    , id3
    )


constrainCall : Id -> RTV -> Expr -> Expr -> Type -> ( Constraint, Id )
constrainCall id rtv func arg expected =
    let
        ( funcType, id1 ) =
            fresh id

        ( argType, id2 ) =
            fresh id1

        ( resultType, id3 ) =
            fresh id2

        ( funcCon, id4 ) =
            constrain id3 rtv func funcType

        ( argCon, id5 ) =
            constrain id4 rtv arg argType
    in
    ( CAnd
        [ funcCon
        , CEqual funcType (TypeLambda argType resultType)
        , argCon
        , CEqual resultType expected
        ]
    , id5
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


type alias State =
    { subst : Subst
    , errors : List TypeError
    }


solve : RTV -> Constraint -> Subst -> Result TypeError Subst
solve rtv constraint subst =
    case constraint of
        CEqual t1 t2 ->
            unifies (applySubst subst t1) (applySubst subst t2)

        CAnd constraints ->
            List.foldl
                (\constraint1 ->
                    Result.andThen
                        (\subst1 ->
                            Result.map (\a -> composeSubst a subst1) (solve rtv constraint1 subst1)
                        )
                )
                (Ok subst)
                constraints

        CLocal name t ->
            lookupRTV rtv name
                |> Result.andThen
                    (\actual ->
                        unifies (applySubst subst actual) (applySubst subst t)
                    )

        CLet { header, bodyCon } ->
            solve
                (Dict.insert (Tuple.first header)
                    (Tuple.second header)
                    rtv
                )
                bodyCon
                subst


lookupRTV : RTV -> Name -> Result TypeError Type
lookupRTV rtv x =
    case Dict.get x rtv of
        Nothing ->
            Err (UnboundVariable x)

        Just type_ ->
            Ok type_



-- ERROR


type TypeError
    = UnificationFail Type Type
    | InfiniteType Type Type
    | UnboundVariable Name
    | UnificationMismatch (List Type) (List Type)


errorToString : TypeError -> String
errorToString error =
    case error of
        UnificationFail ty1 ty2 ->
            "Types do not unify: " ++ AST.prettyType ty1 ++ " and " ++ AST.prettyType ty2

        InfiniteType ty1 ty2 ->
            "Infinite type: " ++ AST.prettyType ty1 ++ " and " ++ AST.prettyType ty2

        UnboundVariable name ->
            "Unbound variable: " ++ Name.toString name

        UnificationMismatch ts1 ts2 ->
            "Unification mismatch: "
                ++ "("
                ++ String.join ", "
                    (List.map AST.prettyType ts1)
                ++ ")"
                ++ "("
                ++ String.join ", "
                    (List.map AST.prettyType ts2)
                ++ ")"
