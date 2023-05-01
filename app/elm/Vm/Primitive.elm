module Vm.Primitive exposing
    ( Primitive1
    , Primitive2
    , Primitive3
    , PrimitiveN
    , array
    , ashift
    , bitand
    , bitnot
    , boolp
    , butfirst
    , char
    , count
    , difference
    , emptyp
    , equalp
    , first
    , floatp
    , form
    , fput
    , greaterp
    , integerp
    , lessp
    , listp
    , lput
    , lshift
    , minus
    , notequalp
    , product
    , quotient
    , remainder
    , round_
    , sentence
    , sum
    , sum2
    , word
    , wordp
    )

{-| This module contains types and functions related to Logo’s builtin
functions.

Citations in this file’s comments are taken from [UCB Logo’s
manual][ucb-manual].

[ucb-manual]: https://people.eecs.berkeley.edu/~bh/usermanual

-}

import Array exposing (Array)
import Bitwise
import Round
import Vm.Error exposing (Error(..), Internal(..))
import Vm.Type as Type


{-| Represent a builtin function that takes one argument.
-}
type alias Primitive1 =
    { name : String
    , f :
        Type.Value -> Result Error Type.Value
    }


{-| Represent a builtin function that takes two arguments.
-}
type alias Primitive2 =
    { name : String
    , f :
        Type.Value -> Type.Value -> Result Error Type.Value
    }


{-| Represent a builtin function that takes three arguments.
-}
type alias Primitive3 =
    { name : String
    , f :
        Type.Value -> Type.Value -> Type.Value -> Result Error Type.Value
    }


{-| Represent a builtin function that takes a variable number of arguments.
-}
type alias PrimitiveN =
    { name : String
    , f :
        List Type.Value -> Result Error Type.Value
    , numberOfDefaultArguments : Int
    }


{-| Get the first element of a `Value`.

    first (Word "word") == Ok (Word "w")

    first (List [ Word "word" ]) == Ok (Word "word")

Return `Err ...` for empty inputs.

-}
first : Type.Value -> Result Error Type.Value
first value =
    case value of
        Type.Word "" ->
            Err <| WrongInput "first" (Type.toDebugString value)

        Type.Word str ->
            Ok <| Type.Word <| String.left 1 str

        Type.Int int ->
            Ok <| Type.Word <| String.left 1 <| String.fromInt int

        Type.Float float ->
            Ok <| Type.Word <| String.left 1 <| String.fromFloat float

        Type.List (first_ :: _) ->
            Ok first_

        Type.List [] ->
            Err <| WrongInput "first" (Type.toDebugString value)

        Type.Array { origin } ->
            Ok <| Type.Word <| String.fromInt origin


{-| Get all but the first element of a `Value`.

    butfirst (Word "word") == Ok (Word "ord")

    butfirst (List [ Word "word" ]) == Ok (List [])

Return `Err ...` for empty inputs.

-}
butfirst : Type.Value -> Result Error Type.Value
butfirst value =
    case value of
        Type.Word "" ->
            Err <| WrongInput "butfirst" (Type.toDebugString value)

        Type.Word str ->
            Ok <| Type.Word <| String.dropLeft 1 str

        Type.Int int ->
            Ok <| Type.Word <| String.dropLeft 1 <| String.fromInt int

        Type.Float float ->
            Ok <| Type.Word <| String.dropLeft 1 <| String.fromFloat float

        Type.List (_ :: rest) ->
            Ok <| Type.List rest

        Type.List [] ->
            Err <| WrongInput "butfirst" (Type.toDebugString value)

        Type.Array _ ->
            Err <| WrongInput "butfirst" (Type.toDebugString value)


{-| Count the number of characters in a `Word` or the number of elements in a
`List`.

    count (Word "word") == Ok (Word "4")

    count (List [ Word "word" ]) == Ok (Word "1")

-}
count : Type.Value -> Result Error Type.Value
count value =
    let
        length =
            case value of
                Type.Word word_ ->
                    String.length word_

                Type.Int int ->
                    int |> String.fromInt |> String.length

                Type.Float float ->
                    float |> String.fromFloat |> String.length

                Type.List list ->
                    List.length list

                Type.Array { items } ->
                    Array.length items
    in
    length |> String.fromInt |> Type.Word |> Ok


{-| Convert two values to numbers and compare whether the first one is less
than the second one.

    lessp (Word "0") (Word "1") == Ok (Word "true")
    lessp (Word "1") (Word "1") == Ok (Word "false")
    lessp (Word "a") (Word "1") == Err _

-}
lessp : Type.Value -> Type.Value -> Result Error Type.Value
lessp value1 value2 =
    case Type.toInt value1 of
        Ok int1 ->
            case Type.toInt value2 of
                Ok int2 ->
                    if int1 < int2 then
                        Ok (Type.Word "true")

                    else
                        Ok (Type.Word "false")

                Err _ ->
                    Err <| WrongInput "lessp" (Type.toDebugString value2)

        Err _ ->
            Err <| WrongInput "lessp" (Type.toDebugString value1)


{-| Convert two values to numbers and compare whether the first one is greater
than the second one.

    greaterp (Word "1") (Word "0") == Ok (Word "true")
    greaterp (Word "0") (Word "1") == Ok (Word "false")
    greaterp (Word "1") (Word "1") == Ok (Word "false")
    greaterp (Word "a") (Word "1") == Err _

-}
greaterp : Type.Value -> Type.Value -> Result Error Type.Value
greaterp value1 value2 =
    case ( Type.toFloat value1, Type.toFloat value2 ) of
        ( Ok float1, Ok float2 ) ->
            if float1 > float2 then
                Ok (Type.Word "true")

            else
                Ok (Type.Word "false")

        ( Err _, _ ) ->
            Err <| WrongInput "greaterp" (Type.toDebugString value1)

        ( _, Err _ ) ->
            Err <| WrongInput "greaterp" (Type.toDebugString value2)


{-| Check whether a given `Value` is empty. Only the empty `Word` and the empty
`List` are considered empty.

    emptyp (Word "") == Ok (Word "true")

    emptyp (Word "word") == Ok (Word "false")

-}
emptyp : Type.Value -> Result Error Type.Value
emptyp value =
    case value of
        Type.Word "" ->
            Ok (Type.Word "true")

        Type.List [] ->
            Ok (Type.Word "true")

        _ ->
            Ok (Type.Word "false")


{-| Helper function to check whether two `Value`s are equal.
-}
equalp_ : Type.Value -> Type.Value -> Bool
equalp_ value1 value2 =
    let
        compareLists list1 list2 =
            if List.length list1 == List.length list2 then
                List.map2 (\a b -> ( a, b )) list1 list2
                    |> List.all (\( a, b ) -> equalp_ a b)

            else
                False
    in
    case ( value1, value2 ) of
        ( Type.Int int1, Type.Int int2 ) ->
            int1 == int2

        ( Type.Int int, Type.Float float ) ->
            toFloat int == float

        ( Type.Float float, Type.Int int ) ->
            toFloat int == float

        ( Type.Float float1, Type.Float float2 ) ->
            float1 == float2

        ( Type.List list1, Type.List list2 ) ->
            compareLists list1 list2

        ( Type.List _, _ ) ->
            False

        ( _, Type.List _ ) ->
            False

        ( Type.Array array1, Type.Array array2 ) ->
            array1.id == array2.id

        ( Type.Array _, _ ) ->
            False

        ( _, Type.Array _ ) ->
            False

        ( Type.Word word1, Type.Word word2 ) ->
            word1 == word2

        ( Type.Word word_, Type.Int int ) ->
            word_ == String.fromInt int

        ( Type.Int int, Type.Word word_ ) ->
            word_ == String.fromInt int

        ( Type.Float float, Type.Word word_ ) ->
            word_ == String.fromFloat float

        ( Type.Word word_, Type.Float float ) ->
            word_ == String.fromFloat float


{-| Check whether two `Value`s are equal.

    equalp (Int 10) (Int 10) == Ok (Word "true")

    equalp (Float 10.0) (Int 10) == Ok (Word "true")

    equalp (Word "10") (Int 10) == Ok (Word "true")

    equalp (List []) (List []) == Ok (Word "true")

-}
equalp : Type.Value -> Type.Value -> Result Error Type.Value
equalp value1 value2 =
    equalp_ value1 value2 |> Type.fromBool |> Ok


{-| Check whether two `Value`s are not equal.

    notequalp (Int 10) (Int 11) == Ok (Word "true")

    notequalp (Word "10") (Int 10) == Ok (Word "false")

-}
notequalp : Type.Value -> Type.Value -> Result Error Type.Value
notequalp value1 value2 =
    equalp_ value1 value2 |> not |> Type.fromBool |> Ok


{-| Calculate the remainder when dividing `value1` by `value2`.

    remainder (Int 20) (Int 3) == Ok (Int 2)

    remainder (Int 20) (Int 4) == Ok (Int 0)

-}
remainder : Type.Value -> Type.Value -> Result Error Type.Value
remainder value1 value2 =
    let
        result1 =
            Type.toInt value1
                |> Result.mapError (\_ -> WrongInput "remainder" (Type.toDebugString value1))

        result2 =
            Type.toInt value2
                |> Result.mapError (\_ -> WrongInput "remainder" (Type.toDebugString value2))
    in
    case ( result1, result2 ) of
        ( Ok _, Ok 0 ) ->
            Err <| WrongInput "remainder" (Type.toDebugString value2)

        _ ->
            Result.map2 (\int1 int2 -> Type.Int <| remainderBy int2 int1) result1 result2


{-| Calculate the sum of `values`.

    sum [ Int 20, Int 3 ] == Ok (Float 23)

    sum [ Word "20", Int 4 ] == Ok (Float 24)

-}
sum : List Type.Value -> Result Error Type.Value
sum values =
    values
        |> List.map
            (\value ->
                value
                    |> Type.toFloat
                    |> Result.mapError (always (Type.toDebugString value) >> WrongInput "sum")
            )
        |> List.foldl
            (\value acc ->
                Result.map2 (\value1 value2 -> value1 + value2) value acc
            )
            (Ok 0)
        |> Result.map Type.Float


{-| Calculate the sum of `value1` and `value2`.

    sum (Int 20) (Int 3) == Ok (Int 23)

    sum (Word "20") (Int 4) == Ok (Float 24)

-}
sum2 : Type.Value -> Type.Value -> Result Error Type.Value
sum2 value1 value2 =
    case ( value1, value2 ) of
        ( Type.Int int1, Type.Int int2 ) ->
            Ok <| Type.Int (int1 + int2)

        _ ->
            case ( Type.toFloat value1, Type.toFloat value2 ) of
                ( Ok float1, Ok float2 ) ->
                    Ok <| Type.Float (float1 + float2)

                ( Err _, _ ) ->
                    Err <| WrongInput "sum" (Type.toDebugString value1)

                ( _, Err _ ) ->
                    Err <| WrongInput "sum" (Type.toDebugString value2)


{-| Calculate the difference of `value1` and `value2`.

    difference (Int 20) (Int 3) == Ok (Int 17)

    difference (Word "20") (Int 4) == Ok (Float 16)

-}
difference : Type.Value -> Type.Value -> Result Error Type.Value
difference value1 value2 =
    case ( value1, value2 ) of
        ( Type.Int int1, Type.Int int2 ) ->
            Ok <| Type.Int (int1 - int2)

        _ ->
            case ( Type.toFloat value1, Type.toFloat value2 ) of
                ( Ok float1, Ok float2 ) ->
                    Ok <| Type.Float (float1 - float2)

                ( Err _, _ ) ->
                    Err <| WrongInput "difference" (Type.toDebugString value1)

                ( _, Err _ ) ->
                    Err <| WrongInput "difference" (Type.toDebugString value2)


{-| Calculate the product of `value1` and `value2`.

    product (Int 20) (Int 3) == Ok (Int 60)

    product (Word "20") (Int 4) == Ok (Float 80)

-}
product : Type.Value -> Type.Value -> Result Error Type.Value
product value1 value2 =
    case ( value1, value2 ) of
        ( Type.Int int1, Type.Int int2 ) ->
            Ok <| Type.Int (int1 * int2)

        _ ->
            case ( Type.toFloat value1, Type.toFloat value2 ) of
                ( Ok float1, Ok float2 ) ->
                    Ok <| Type.Float (float1 * float2)

                ( Err _, _ ) ->
                    Err <| WrongInput "product" (Type.toDebugString value1)

                ( _, Err _ ) ->
                    Err <| WrongInput "product" (Type.toDebugString value2)


{-| Calculate the quotient of `value1` and `value2`.

    quotient (Int 20) (Int 4) == Ok (Int 5)
    quotient (Word "20") (Int 4) == Ok (Float 5)
    quotient (Int 20) (Int 0) == Err _
    quotient (Int 20) (Word "0") == Err _

-}
quotient : Type.Value -> Type.Value -> Result Error Type.Value
quotient value1 value2 =
    case ( value1, value2 ) of
        ( _, Type.Int 0 ) ->
            Err <| WrongInput "quotient" "0"

        ( Type.Int int1, Type.Int int2 ) ->
            if remainderBy int2 int1 == 0 then
                Ok <| Type.Int (int1 // int2)

            else
                Ok <| Type.Float <| toFloat int1 / toFloat int2

        _ ->
            case ( Type.toFloat value1, Type.toFloat value2 ) of
                ( Ok float1, Ok float2 ) ->
                    if float2 == 0.0 then
                        Err <| WrongInput "quotient" "0"

                    else
                        Ok <| Type.Float <| float1 / float2

                ( Err _, _ ) ->
                    Err <| WrongInput "quotient" (Type.toDebugString value1)

                ( _, Err _ ) ->
                    Err <| WrongInput "quotient" (Type.toDebugString value2)


{-| Round `value` to the nearest integer.

      round_ (Int -1.5) == Ok (Int -2)
      round_ (Int -1.2) == Ok (Int -1)

-}
round_ : Type.Value -> Result Error Type.Value
round_ value =
    let
        roundAwayFromZero : Float -> Int
        roundAwayFromZero float =
            if float < 0 then
                float |> negate |> round |> negate

            else
                round float
    in
    Type.toFloat value
        |> Result.map (roundAwayFromZero >> Type.fromInt)
        |> Result.mapError (always <| WrongInput "round" (Type.toDebugString value))


{-| Negate `value`.

    negate (Int 10) == Ok (Float -10)

    negate (Word "10") == Ok (Float -10)

-}
minus : Type.Value -> Result Error Type.Value
minus value =
    Type.toFloat value
        |> Result.map (negate >> Type.fromFloat)
        |> Result.mapError (always <| WrongInput "minus" (Type.toDebugString value))


{-| Join two words into one.

    word (Word "a") (Word "b) == Ok (Word "ab")

-}
word : Type.Value -> Type.Value -> Result Error Type.Value
word value1 value2 =
    case ( value1, value2 ) of
        ( Type.List _, _ ) ->
            Err <| WrongInput "word" (Type.toDebugString value1)

        ( _, Type.List _ ) ->
            Err <| WrongInput "word" (Type.toDebugString value2)

        _ ->
            Ok <| Type.Word <| (Type.toString value1 ++ Type.toString value2)


{-| Join a list of values into a list. One level of nesting will be flattened.

    sentence [ Word "a", Word "b ] == Ok (List [ Word "a", Word "b" ])
    sentence [ Word "a", List [ Word "b" ] ] == Ok (List [ Word "a", Word "b" ])

-}
sentence : List Type.Value -> Result Error Type.Value
sentence =
    let
        toList : Type.Value -> List Type.Value
        toList value =
            case value of
                Type.List list ->
                    list

                _ ->
                    [ value ]
    in
    Ok << Type.List << List.concatMap toList


{-|

> outputs a list equal to its second input with one extra member, the first
> input, at the beginning. If the second input is a word, then the first input
> must be a one-letter word, and FPUT is equivalent to WORD.

-}
fput : Type.Value -> Type.Value -> Result Error Type.Value
fput value1 value2 =
    case ( value1, value2 ) of
        ( _, Type.List list ) ->
            Ok <| Type.List <| value1 :: list

        _ ->
            let
                word_ =
                    Type.toString value1
            in
            if String.length word_ == 1 then
                Ok <| Type.Word <| word_ ++ Type.toString value2

            else
                Err <| WrongInput "fput" word_


{-|

> outputs a list equal to its second input with one extra member, the first
> input, at the end. If the second input is a word, then the first input must
> be a one-letter word, and LPUT is equivalent to WORD with its inputs in the
> other order.

-}
lput : Type.Value -> Type.Value -> Result Error Type.Value
lput value1 value2 =
    case ( value1, value2 ) of
        ( _, Type.List list ) ->
            Ok <| Type.List <| list ++ [ value1 ]

        _ ->
            let
                word_ =
                    Type.toString value1
            in
            if String.length word_ == 1 then
                Ok <| Type.Word <| Type.toString value2 ++ word_

            else
                Err <| WrongInput "lput" word_


{-|

> outputs an array of "size" members (must be a positive integer), each of which
> initially is an empty list. Array members can be selected with ITEM and
> changed with SETITEM. The first member of the array is member number 1 unless
> an "origin" input (must be an integer) is given, in which case the first
> member of the array has that number as its index. (Typically 0 is used as the
> origin if anything.)

-}
array : List Type.Value -> Result Error Type.Value
array values =
    let
        initializeItems : Type.Value -> Result Error (Array Type.Value)
        initializeItems value =
            Type.toInt value
                |> Result.map (\len -> Array.initialize len (always (Type.List [])))
                |> Result.mapError (always <| WrongInput "array" (Type.toDebugString value))

        initializeWithItemsAndOrigin : Array Type.Value -> Int -> Type.Value
        initializeWithItemsAndOrigin items origin =
            Type.Array { items = items, origin = origin, id = Nothing }
    in
    case values of
        [ first_, second ] ->
            let
                items =
                    initializeItems first_

                origin =
                    Type.toInt second
                        |> Result.mapError (always <| WrongInput "array" (Type.toDebugString second))
            in
            Result.map2 initializeWithItemsAndOrigin items origin

        [ first_ ] ->
            initializeItems first_
                |> Result.map (\items -> Type.Array { items = items, origin = 1, id = Nothing })

        [] ->
            Err <| NotEnoughInputs "array"

        _ ->
            Err <| TooManyInputs "array"


{-| Convert an integer to a character. The actual work is delegated to Elm’s
`Char.fromCode`.

    char (Word "65") == Ok (Word "A")

    char (Word "10") == Ok (Word "\n")

<https://package.elm-lang.org/packages/elm/core/latest/Char#fromCode>

-}
char : Type.Value -> Result Error Type.Value
char value =
    Type.toInt value
        |> Result.map (Char.fromCode >> String.fromChar >> Type.Word)
        |> Result.mapError (always <| WrongInput "char" (Type.toDebugString value))


{-| Check whether a given `Value` is a word.

    wordp (Word "a") == Ok (Word "true")

    wordp (Int 1) == Ok (Word "true")

    wordp (Float 1.0) == Ok (Word "true")

    wordp (List []) == Ok (Word "false")

-}
wordp : Type.Value -> Result Error Type.Value
wordp value =
    case value of
        Type.Int _ ->
            Ok Type.true

        Type.Float _ ->
            Ok Type.true

        Type.Word _ ->
            Ok Type.true

        Type.List _ ->
            Ok Type.false

        Type.Array _ ->
            Ok Type.false


{-| Check whether a given `Value` is a list.

    listp (Word "a") == Ok (Word "false")

    listp (Int 1) == Ok (Word "false")

    listp (Float 1.0) == Ok (Word "false")

    listp (List []) == Ok (Word "true")

-}
listp : Type.Value -> Result Error Type.Value
listp value =
    case value of
        Type.List _ ->
            Ok Type.true

        _ ->
            Ok Type.false


{-| Check whether a given `Value` is an integer.

    integerp (Word "a") == Ok (Word "false")

    integerp (Int 10) == Ok (Word "true")

    integerp (Word "10") == Ok (Word "true")

-}
integerp : Type.Value -> Result Error Type.Value
integerp value =
    case value of
        Type.Int _ ->
            Ok Type.true

        Type.Word word_ ->
            case String.toInt word_ of
                Just _ ->
                    Ok Type.true

                Nothing ->
                    Ok Type.false

        Type.Float float ->
            if (round float |> toFloat) == float then
                Ok Type.true

            else
                Ok Type.false

        _ ->
            Ok Type.false


{-| Check whether a given `Value` is a float.

    floatp (Word "a") == Ok (Word "false")

    floatp (Int 10) == Ok (Word "true")

    floatp (Float 10.1) == Ok (Word "true")

    floatp (Word "10") == Ok (Word "true")

-}
floatp : Type.Value -> Result Error Type.Value
floatp value =
    case value of
        Type.Int _ ->
            Ok Type.true

        Type.Float _ ->
            Ok Type.true

        Type.Word word_ ->
            case String.toFloat word_ of
                Just _ ->
                    Ok Type.true

                _ ->
                    Ok Type.false

        _ ->
            Ok Type.false


{-| Check whether a given `Value` is a boolean.

    boolp (Word "false") == Ok (Word "true")

    boolp (Word "TRUE") == Ok (Word "true")

    boolp (Word "a") == Ok (Word "false")

    boolp (Int 10) == Ok (Word "false")

-}
boolp : Type.Value -> Result Error Type.Value
boolp value =
    case value of
        Type.Word word_ ->
            if String.toLower word_ == "true" || String.toLower word_ == "false" then
                Ok Type.true

            else
                Ok Type.false

        _ ->
            Ok Type.false


{-|

> outputs a word containing a printable representation of "num", possibly
> preceded by spaces (and therefore not a number for purposes of performing
> arithmetic operations), with at least "width" characters, including exactly
> "precision" digits after the decimal point. (If "precision" is 0 then there
> will be no decimal point in the output.)

-}
form : Type.Value -> Type.Value -> Type.Value -> Result Error Type.Value
form value1 value2 value3 =
    case ( Type.toFloat value1, Type.toInt value2, Type.toInt value3 ) of
        ( Ok number, Ok width, Ok precision ) ->
            let
                paddedNumber =
                    number
                        |> Round.round precision
                        |> String.padLeft width ' '
            in
            Ok <| Type.Word paddedNumber

        ( Err _, _, _ ) ->
            Err <| WrongInput "form" (Type.toDebugString value1)

        ( _, Err _, _ ) ->
            Err <| WrongInput "form" (Type.toDebugString value2)

        ( _, _, Err _ ) ->
            Err <| WrongInput "form" (Type.toDebugString value3)


{-| Calculate the bitwise and of `values` which must be integers.

    bitand [ Int 1, Int 3 ] == Ok (Int 1)

-}
bitand : List Type.Value -> Result Error Type.Value
bitand values =
    values
        |> List.map
            (\value ->
                value
                    |> Type.toInt
                    |> Result.mapError (always (Type.toDebugString value) >> WrongInput "bitand")
            )
        |> List.foldl
            (\value acc ->
                Result.map2 (\value1 value2 -> Bitwise.and value1 value2) value acc
            )
            (Ok <| Bitwise.complement 0)
        |> Result.map Type.Int


{-| Calculate the bitwise not of `value` which must be an integer.

    bitnot (Int 6) == Ok (Int -7)

-}
bitnot : Type.Value -> Result Error Type.Value
bitnot value =
    value
        |> Type.toInt
        |> Result.map (Bitwise.complement >> Type.Int)
        |> Result.mapError (always (Type.toDebugString value) >> WrongInput "bitnot")


{-| Shift bits of `value1` to the left by `value2`. Both values must be
integers. This is called an [arithmetic shift].

    ashift (Int 2) (Int 3) == Ok (Int 16)

[bitwise operations]: https://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift
[arithmetic shift]: https://en.wikipedia.org/wiki/Arithmetic_shift

-}
ashift : Type.Value -> Type.Value -> Result Error Type.Value
ashift value1 value2 =
    case ( Type.toInt value1, Type.toInt value2 ) of
        ( Ok int1, Ok int2 ) ->
            if int2 > 0 then
                Ok <| Type.Int <| Bitwise.shiftLeftBy int2 int1

            else
                Ok <| Type.Int <| Bitwise.shiftRightBy (negate int2) int1

        ( Err _, _ ) ->
            Err <| WrongInput "ashift" (Type.toDebugString value1)

        ( _, Err _ ) ->
            Err <| WrongInput "ashift" (Type.toDebugString value2)


{-| Shift bits of `value1` to the left by `value2`. Both values must be
integers. This is called a [locical shift].

    lshift (Int 2) (Int 3) == Ok (Int 16)

[bitwise operations]: https://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift
[logical shift]: https://en.wikipedia.org/wiki/Logical_shift

-}
lshift : Type.Value -> Type.Value -> Result Error Type.Value
lshift value1 value2 =
    case ( Type.toInt value1, Type.toInt value2 ) of
        ( Ok int1, Ok int2 ) ->
            if int2 > 0 then
                Ok <| Type.Int <| Bitwise.shiftLeftBy int2 int1

            else
                Ok <| Type.Int <| Bitwise.shiftRightZfBy (negate int2) int1

        ( Err _, _ ) ->
            Err <| WrongInput "lshift" (Type.toDebugString value1)

        ( _, Err _ ) ->
            Err <| WrongInput "lshift" (Type.toDebugString value2)
