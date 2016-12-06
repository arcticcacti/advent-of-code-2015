#load "AdventUtils.fsx"

let getInput() = AdventUtils.inputFromFile "day_2.txt"

/// get the dimensions from a LxWxH string
let parseDimensions (dims:string) =
    dims.Split('x')
    |> Array.take 3
    |> Array.map int
    |> fun arr -> arr.[0], arr.[1], arr.[2]


/// the area required to wrap a present
let requiredWrapping (l, w, h) =
    let sides = [| l*w; w*h; h*l |]
    // we need the area of each side plus the smallest side
    Array.sum sides |> (*) 2 |> (+) <| Array.min sides


let requiredRibbon (l, w, h) =
    let smallestPerimeter = [| l; w; h |] |> Array.sort |> Array.take 2 |> Array.sum |> (*) 2
    let volume = l * w * h
    smallestPerimeter + volume


//
// Main functions
//

let part1() =
    getInput()
    |> Seq.map (parseDimensions >> requiredWrapping)
    |> Seq.sum
    |> printfn "Part 1 - total area: %d"


let part2() =
    getInput()
    |> Seq.map (parseDimensions >> requiredRibbon)
    |> Seq.sum
    |> printfn "Part 2 - total ribbon length: %d"


//
// Tests
//
let test1() =
    let testData = [| "2x3x4", 58; "1x1x10", 43 |]
    let test = AdventUtils.testResultIsExpected "1" (parseDimensions >> requiredWrapping)
    testData |> Array.iter test


let test2() =
    let testData = [| "2x3x4", 34; "1x1x10", 14 |]
    let test = AdventUtils.testResultIsExpected "2" (parseDimensions >> requiredRibbon)
    testData |> Array.iter test


part1()
part2()
test1()
test2()