module Tests

open FsCheck.Xunit
open AudioLib

let countZeroCrossings stream =
    stream 
    |> Seq.pairwise 
    |> Seq.choose(fun a -> 
            match a with
            | (x, y) when x <= 0.0 && y >= 0.0 -> Some a
            | (x, y) when x >= 0.0 && y <= 0.0 -> Some a
            | _ -> None)
    |> Seq.length

[<Property>]
let ``Generator output is always below 1`` () =
     Generator.sine 44100 440.0 |> Seq.take 10000 |> Seq.max < 1.0

[<Property>]
let ``Generator output is always above -1`` () =
     Generator.sine 44100 440.0 |> Seq.take 10000 |> Seq.max > -1.0

[<Property>]
let ``1 second of 440Hz crosses zero 880 times`` () =
    Generator.sine 44100 440.0 |> Seq.take 44100
    |> countZeroCrossings = 880

[<Property>]
let ``1 second of 0Hz crosses zero 0 times`` () =
    Generator.sine 44100 0.0 |> Seq.take 44100
    |> countZeroCrossings = 44100
