module Ui.Examples exposing (view)

import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E


type alias Config msg =
    { onClick : String -> msg
    }


all : Dict String String
all =
    [ ( "ice cream", """to choices :menu [:sofar []]
if emptyp :menu [print :sofar stop]
foreach first :menu [(choices butfirst :menu sentence :sofar ?)]
end
choices [[small medium large] [vanilla [ultra chocolate] lychee [rum raisin] ginger] [cone cup]]
""" )
    , ( "square"
      , """to square :length
repeat 4 [ forward :length right 90 ]
end
pendown square 100
"""
      )
    , ( "snowflake", """to snowflake :length :depth
if :depth = 0 [ forward :length ]
if :depth <> 0 [ snowflake :length / 3 :depth - 1 right 60 snowflake :length / 3 :depth - 1 left 120 snowflake :length / 3 :depth - 1 right 60 snowflake :length / 3 :depth - 1 ]
end
back 200 right 30 pendown snowflake 350 3 left 120 snowflake 350 3 left 120 snowflake 350 3
""" )
    , ( "fizzbuzz", """to fizzbuzz :times
repeat :times [ ifelse 0 = remainder repcount 15 [ print "fizzbuzz ] [ ifelse 0 = remainder repcount 5 [ print "buzz ] [ ifelse 0 = remainder repcount 3 [ print "fizz ] [ print repcount ] ] ] ]
end
fizzbuzz 100
""" )
    , ( "mandelbrot"
      , """to count.color :count
  if :count > 256 [output 0]
  if :count > 128 [output 7]
  if :count >  64 [output 5]
  if :count >  32 [output 6]
  if :count >  16 [output 4]
  if :count >   8 [output 2]
  if :count >   4 [output 1]
  output 3
end
to calc :zr :zi [:count 0] [:az 0] [:bz 0]
  if :az*:az + :bz*:bz>4 [output :count]
  if :count>256 [output :count]
  output (calc :zr :zi (:count + 1) (:zr + :az*:az - :bz*:bz) (:zi + 2*:az*:bz))
end
to mandelbrot :left :bottom :side :size
  clearscreen
  localmake "inc :side/:size
  localmake "zr :left
  repeat :size [ make "zr :zr + :inc make "zi :bottom penup setxy repcount - :size/2 minus :size/2 pendown repeat :size [ make "zi :zi + :inc setpencolor count.color calc :zr :zi forward 1 ] ]
end
mandelbrot minus 2 minus 1.25 2.5 30
"""
      )
    , ( "fibonacci i"
      , """to fibonacci :i
  if :i = 1 [ output 1 ]
  if :i = 2 [ output 1 ]
  output (fibonacci :i - 1) + (fibonacci :i - 2)
end
foreach [ 1 2 3 4 5 6 7 8 9 10 ] [ print fibonacci ?1 ]
"""
      )
    , ( "fibonacci ii"
      , """to fibonacci :i
  if :i = 1 [ output 1 ]
  if :i = 2 [ output 1 ]
  localmake "a 1
  localmake "b 1
  localmake "c 1
  repeat (:i - 2) [ make "c :a make "a :a + :b make "b :c ]
  output :a
end
foreach [ 1 2 3 4 5 6 7 8 9 10 ] [ print fibonacci ?1 ]"""
      )
    , ( "99 bottles of beer"
      , """to bottles :n
  if :n = 0 [output [No more bottles]]
  if :n = 1 [output [1 bottle]]
  output sentence :n "bottles
end
to verse :n
  print sentence bottles :n [of beer on the wall]
  print sentence bottles :n [of beer]
  print [Take one down, pass it around]
  print sentence bottles :n-1 [of beer on the wall]
end
to range :from :to [:acc []]
  if :from > :to - 1 [make "acc (sentence :acc :from) output (range :from - 1 :to :acc)]
  output :acc
end
foreach range 99 1 [verse ?1 (print ")]"""
      )
    ]
        |> Dict.fromList


example : Config msg -> String -> String -> Html msg
example { onClick } title code =
    H.li []
        [ H.button [ A.class "load-example", E.onClick (onClick code) ]
            [ H.text title ]
        ]


view : Config msg -> Html msg
view config =
    let
        h1 =
            H.h1 [] [ H.text "Examples" ]

        children =
            Dict.map (example config) all
                |> Dict.values
    in
    H.div [ A.id "examples" ] [ h1, H.ul [] children ]
