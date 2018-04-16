open Expecto
open AudioLib.Generator
open FsCheck

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

type AudioFrequencies =
    static member PositiveFloat () =
        Arb.Default.Float() |> Arb.mapFilter abs (fun t -> t >= 0.0 && t <= 25000.0)

let config = { FsCheckConfig.defaultConfig with arbitrary = [typeof<AudioFrequencies>] }

let tests = 
    testList "Generator Tests" [
        testProperty "Generator output is always below 1" (fun frequency ->
            sine1sec frequency |> Seq.max <= 1.0
        )
        testProperty "Generator output is always above -1" (fun frequency ->
            sine1sec frequency |> Seq.min >= -1.0
        )
        testPropertyWithConfig config "1 second of f Hz contains approximately 2 * f zero crossings" (fun frequency ->
            let crossings = sine1sec frequency |> countZeroCrossings 
            let expected = int (frequency * 2.0)
            Expect.isGreaterThanOrEqual crossings (expected - 1) "Not enough zero crossings"
            Expect.isLessThanOrEqual crossings (expected + 1) "Too many zero crossings"
        )
        test "1 second of 0 Hz is always 0.0" {
            Expect.equal (sine1sec 0.0 |> countZeroCrossings) 0 "0 Hz signal oscillates"
        }
        test "1 second of 0 Hz contains all values at 0.0" {
            Expect.equal (sine1sec 0.0 |> Seq.max) 0.0 "Max is not 0.0"
            Expect.equal (sine1sec 0.0 |> Seq.min) 0.0 "Min is not 0.0"
        }
    ]

[<EntryPoint>] 
let main args = 
    runTestsWithArgs defaultConfig args tests
