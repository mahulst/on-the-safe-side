module Tests exposing (..)

import Test exposing (..)
import Expect
import Main exposing (updateCompleted)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "updateCompleted"
        [ test "complete first code" <|
            \_ ->
                Expect.equal (updateCompleted [ 38, 12, 58, 45 ] 38 37 0) 1
        , test "do not reset after counter clockwise turn" <|
            \_ ->
                Expect.equal (updateCompleted [ 38, 12, 58, 45 ] 37 38 1) 1
        , test "reset after clockwise turn" <|
            \_ ->
                Expect.equal (updateCompleted [ 38, 12, 58, 45 ] 39 38 1) 0
        , test "do not reset after  clockwise turn" <|
            \_ ->
                Expect.equal (updateCompleted [ 38, 12, 58, 45 ] 13 12 2) 2
        , test "reset after counter clockwise turn" <|
            \_ ->
                Expect.equal (updateCompleted [ 38, 12, 58, 45 ] 11 12 2) 0
        ]
