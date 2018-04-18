open Expecto
open AudioLib.Generator
open FsCheck

let gen1sec fn frequency =
    fn 44100.0 frequency |> Seq.take 44100

let sine1sec = gen1sec sine

let square1sec = gen1sec square

let saw1sec = gen1sec sawtooth

let tri1sec = gen1sec triangle

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
        Arb.Default.Float() |> Arb.mapFilter abs (fun t -> t >= 1.0 && t <= 25000.0)

let config = { FsCheckConfig.defaultConfig with arbitrary = [typeof<AudioFrequencies>] }

let tests = 
    testList "Generator Tests" [
        testPropertyWithConfig config "Sine output is always below 1" (fun frequency ->
            sine1sec frequency |> Seq.max <= 1.0
        )
        testPropertyWithConfig config "Sine output is always above -1" (fun frequency ->
            sine1sec frequency |> Seq.min >= -1.0
        )
        testPropertyWithConfig config "Square +ve output is always 1" (fun frequency ->
            square1sec frequency |> Seq.max = 1.0
        )
        testPropertyWithConfig config "Square -ve is always -1" (fun frequency ->
            square1sec frequency |> Seq.min = -1.0
        )
        testPropertyWithConfig config "Sawtooth output is always below 1" (fun frequency ->
            saw1sec frequency |> Seq.max <= 1.0
        )
        testPropertyWithConfig config "Sawtooth output is always above -1" (fun frequency ->
            saw1sec frequency |> Seq.min >= -1.0
        )
        testPropertyWithConfig config "Sawtooth output is always increasing" (fun frequency ->
            let saw = saw1sec frequency
            saw |> Seq.pairwise |> Seq.forall (fun (x,y) -> y > x || x - y > 1.99)
        )
        testPropertyWithConfig config "Triangle output is always below 1" (fun frequency ->
            tri1sec frequency |> Seq.max <= 1.0
        )
        testPropertyWithConfig config "Triangle output is always above -1" (fun frequency ->
            tri1sec frequency |> Seq.min >= -1.0
        )
        test "Noise is always below 1" {
            Expect.isTrue (noise |> Seq.take 10000 |> Seq.max <= 1.0) "Noise exceeds 1.0"
        }
        test "Noise is always above -1" {
            Expect.isTrue (noise |> Seq.take 10000 |> Seq.min >= -1.0) "Noise is less than -1.0"
        }
        testPropertyWithConfig config "1 second of f Hz contains approximately 2 * f zero crossings" (fun frequency ->
            let crossings = sine1sec frequency |> countZeroCrossings 
            let expected = int (frequency * 2.0)
            Expect.isGreaterThanOrEqual crossings (expected) "Not enough zero crossings"
            Expect.isLessThanOrEqual crossings (expected + 1) "Too many zero crossings"
        )
        test "1 second of 0 Hz does not cross zero" {
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
