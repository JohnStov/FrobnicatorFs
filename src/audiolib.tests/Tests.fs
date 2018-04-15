module Tests

open FsCheck.Xunit

let sin = Seq.initInfinite (fun _ -> 0.0) |> Seq.take 10000

[<Property>]
let ``Generator output is always below 1`` () =
     sin |> Seq.max < 1.0
