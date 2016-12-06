#load "AdventUtils.fsx"

let getInput() = AdventUtils.inputFromFile "day_2.txt"

/// get the dimensions from a LxWxH string
let parseDimensions (dims:string) =
    dims.Split('x')
    |> Array.take 3
    |> Array.map int
    |> fun arr -> arr.[0], arr.[1], arr.[2]


/// the area required to wrap a present
let requiredWrapping dims =
    let (l, w, h) = parseDimensions dims
    let sides = [| l*w; w*h; h*l |]
    // we need the area of each side plus the smallest side
    Array.sum sides |> (*) 2 |> (+) <| Array.min sides


//
// Main functions
//

let part1() =
    getInput()
    |> Seq.map requiredWrapping
    |> Seq.sum
    |> printfn "Part 1 - total area: %d"


//
// Tests
//
let test1() =
    let testData = [| "2x3x4", 58; "1x1x10", 43 |]
    let runTest (data, expected) =
        let shouldBe = AdventUtils.printTestResult (sprintf "1 - %s" data) (=)
        data |> requiredWrapping |> shouldBe expected
    testData |> Array.iter runTest


part1()
test1()