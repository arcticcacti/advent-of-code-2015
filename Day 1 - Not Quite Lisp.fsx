#load "AdventUtils.fsx"

let getInput() = AdventUtils.inputFromFile "day_1.txt" |> Array.head

/// move from a position by parsing a movement char
let move current dir =
    match dir with
    | '(' -> current + 1
    | ')' -> current - 1
    | _ -> failwithf "Unrecognised direction char %c" dir

/// get the final floor number after following directions from floor 0
let followDirections = Seq.fold move 0

/// get the step in a set of directions where the basement is entered 
let basementEntered = Seq.scan move 0 >> Seq.findIndex ((=) -1)


//
// main functions
//

let part1() =
    getInput()
    |> followDirections
    |> printfn "Part 1 - final floor: %d"


let part2() =
    getInput()
    |> basementEntered
    |> printfn "Part 2 - basement entered on step %d"


//
// tests
//

let test1() =
    let testData =
        [|  "(())", 0;
            "()()", 0;
            "(((", 3;
            "(()(()(", 3;
            "))(((((", 3;
            "())", -1;
            "))(", -1;
            ")))", -3;
            ")())())", -3; |]

    let runTest (data, expected) =
        let shouldBe = AdventUtils.printTestResult (sprintf "1 - %s" data) (=)
        let result = followDirections data
        result |> shouldBe expected
    testData |> Seq.iter runTest

let test2() =
    let testData = [| ")", 1; "()())", 5 |]
    let runTest (data, expected) =
        let shouldBe = AdventUtils.printTestResult (sprintf "2 - %s" data) (=)
        let result = basementEntered data
        result |> shouldBe expected
    testData |> Seq.iter runTest




part1()
part2()
test1()
test2()