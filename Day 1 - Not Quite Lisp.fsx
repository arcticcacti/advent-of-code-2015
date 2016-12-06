#load "AdventUtils.fsx"

let getInput() = AdventUtils.inputFromFile "day_1.txt" |> Array.head

let move current dir =
    match dir with
    | '(' -> current + 1
    | ')' -> current - 1
    | _ -> failwithf "Unrecognised direction char %c" dir

let followDirections = Seq.fold move 0


let part1() =
    getInput()
    |> followDirections
    |> printfn "Part 1 - final floor: %d" 

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



part1()
test1()