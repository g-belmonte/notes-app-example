module Example exposing (..)

import Expect exposing (Expectation)
-- import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Hello world"
        [ test "hello world" <|
              \_ ->
                  "hello world"
                      |> Expect.equal "hello world"
        ]
