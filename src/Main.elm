module Main exposing (main)

import Benchmark
import Benchmark.Runner exposing (BenchmarkProgram, program)
import List
import List.Extra
import Maybe


main : BenchmarkProgram
main =
    program <|
        Benchmark.describe "init tests" <|
            List.map
                (\i ->
                    let
                        length =
                            100 * i

                        list =
                            List.range 0 length
                    in
                    Benchmark.compare ("init list of length " ++ String.fromInt length)
                        "old"
                        (\_ ->
                            Maybe.map List.reverse <| List.tail <| List.reverse list
                        )
                        "new"
                        (\_ ->
                            List.Extra.init list
                        )
                )
                (List.range 1 5)
