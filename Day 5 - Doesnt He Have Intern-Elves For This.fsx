#load "AdventUtils.fsx"

let getInput() = AdventUtils.inputFromFile "day_5.txt"

type Judgement = Naughty | Nice

/// count the number of appearances of any of the listed elements
let countAnyOf elems =
    Seq.filter (fun x -> elems |> Seq.contains x) >> Seq.length


/// whether a double letter appears in a string
let hasADoubleLetter = Seq.pairwise >> Seq.exists (fun (x,y) -> x=y)


/// check that a string doesn't contain any of the listed letter pairs
let doesNotContainAnyOf pairs str =
    let allPairs = str |> Seq.pairwise |> Seq.map (fun (x,y) -> string x + string y)
    countAnyOf allPairs pairs = 0


/// Whether a string is Naughty or Nice according to Santa's rules
let naughtyOrNice (str:string) =
    let isNice =
        str |> countAnyOf ['a'; 'e'; 'i'; 'o'; 'u'] >= 3
        && str |> hasADoubleLetter
        && str |> doesNotContainAnyOf ["ab"; "cd"; "pq"; "xy"]
    if isNice then Nice else Naughty

//
// Main
//

let part1() =
    getInput()
    |> Seq.map naughtyOrNice
    |> Seq.filter ((=) Nice)
    |> Seq.length
    |> printfn "Part 1 - %d strings are nice"

// Tests

let test1() =
    let testData = [|
        "ugknbfddgicrmopn", Nice;
        "aaa", Nice;
        "jchzalrnumimnmhp", Naughty;
        "haegwjzuvuyypxyu", Naughty;
        "dvszwmarrgswjxmb", Naughty
    |]
    let test = AdventUtils.testResultIsExpected "1" naughtyOrNice
    testData |> Array.iter test


part1()

test1()
1