module Tests

open FsCheck.Xunit
open AudioLib

[<Property>]
let ``Generator output is always below 1`` () =
     Generator.sine 44100 440.0 |> Seq.take 10000 |> Seq.max < 1.0

[<Property>]
let ``Generator output is always above -1`` () =
     Generator.sine 44100 440.0 |> Seq.take 10000 |> Seq.max > -1.0
