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

/// whether a string has a pair of identical chars separated by any other, e.g. XYX
let hasXYXrepeatingLetter =
    Seq.windowed 3 >> Seq.exists (fun arr -> arr.[0] = arr.[2])

let hasADoubleNonOverlappingLetter str =
    /// check if any pair has a match in the list, ignoring the adjacent pair
    let rec containsNonAdjacentPair =
        function
        | [] | [_] | [_;_]
            -> false
        | x::y::xs
            -> if List.contains x xs then
                   true
               else
                   containsNonAdjacentPair (y::xs)

    str |> Seq.pairwise |> List.ofSeq |> containsNonAdjacentPair


/// Whether a string is Naughty or Nice according to Santa's rules
let naughtyOrNice str =
    let isNice =
        str |> countAnyOf ['a'; 'e'; 'i'; 'o'; 'u'] >= 3
        && str |> hasADoubleLetter
        && str |> doesNotContainAnyOf ["ab"; "cd"; "pq"; "xy"]
    if isNice then Nice else Naughty

let naughtyOrNice2 str =
    let isNice =
        str |> hasADoubleNonOverlappingLetter
        && str |> hasXYXrepeatingLetter
    if isNice then Nice else Naughty

let countNice f = Seq.map f >> Seq.filter ((=) Nice) >> Seq.length

//
// Main
//

let part1() =
    getInput()
    |> countNice naughtyOrNice
    |> printfn "Part 1 - %d strings are nice"

let part2() =
    getInput()
    |> countNice naughtyOrNice2
    |> printfn "Part 2 - %d strings are nice"

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

let test2() =
    let testData = [|
        "qjhvhtzxzqqjkmpb", Nice;
        "xxyxx", Nice;
        "uurcxstgmygtbstg", Naughty;
        "ieodomkazucvgmuy", Naughty
    |]
    let test = AdventUtils.testResultIsExpected "2" naughtyOrNice2
    testData |> Array.iter test

part1()
part2()

test1()
test2()
1