open Expecto
open AudioLib.Generator

let sine1sec frequency= sine 44100 frequency |> Seq.take 44100

let countZeroCrossings stream =
    stream 
    |> Seq.pairwise 
    |> Seq.choose(fun a -> 
            match a with
            | (x, y) when x <= 0.0 && y > 0.0 -> Some a
            | (x, y) when x >= 0.0 && y < 0.0 -> Some a
            | _ -> None)
    |> Seq.length

let tests = 
    testList "Generator Tests" [
        test "Generator output is always below 1" {
            Expect.isLessThan (sine1sec 440.0 |> Seq.max) 1.0 "Generator output is always below 1"
        }
        test "Generator output is always above -1" {
            Expect.isGreaterThan (sine1sec 440.0 |> Seq.min) -1.0 "Generator output is always above -1"
        }
        test "1 second of 440Hz crosses zero 880 times" {
            Expect.equal ((sine1sec 440.0) |> countZeroCrossings) 880 "1 second of 440Hz crosses zero 880 times"
        }
        test "1 second of 0Hz crosses zero 0 times" {
            Expect.equal ((sine1sec 0.0) |> countZeroCrossings) 0 "1 second of 0Hz crosses zero 0 times"
        }
]

[<EntryPoint>] 
let main args = 
    runTestsWithArgs defaultConfig args tests
