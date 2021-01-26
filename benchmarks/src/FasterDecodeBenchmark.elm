module FasterDecodeBenchmark exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Length
import Obj.Decode
import Decode

main : BenchmarkProgram
main =
    program <|
        Benchmark.compare "decode"
            "old triangles"
            (\_ -> Decode.decodeString Length.meters Decode.triangles obj |> Result.map (always ()))
            "triangles"
            (\_ -> Obj.Decode.decodeString Length.meters Obj.Decode.triangles obj |> Result.map (always ()))


obj : String
obj = """# Blender v2.83.3 OBJ File: 'cube'
# www.blender.org
v 1.000000 1.000000 -1.000000
v 1.000000 -1.000000 -1.000000
v 1.000000 1.000000 1.000000
v 1.000000 -1.000000 1.000000
v -1.000000 1.000000 -1.000000
v -1.000000 -1.000000 -1.000000
v -1.000000 1.000000 1.000000
v -1.000000 -1.000000 1.000000
f 1 5 7 3
f 4 3 7 8
f 8 7 5 6
f 6 2 4 8
f 2 1 3 4
f 6 5 1 2
"""