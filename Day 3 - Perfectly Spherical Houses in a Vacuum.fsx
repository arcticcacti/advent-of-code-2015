#load "AdventUtils.fsx"

let getInput() = AdventUtils.inputFromFile "day_3.txt" |> Array.head

/// parse a movement char and apply it to a position
let move (x,y) dir =
    match dir with
    | '<' -> x-1, y
    | '>' -> x+1, y
    | '^' -> x,   y+1
    | 'v' -> x,   y-1
    |  c  -> failwithf "Unrecognised movement char: %c" c


/// move from the last visited position and add the new position to the visited list
let doMove visited dir =
    match visited with
    | [] -> failwithf "No starting position!"
    | pos::xs -> move pos dir :: visited


/// split a sequence into odd and even elements
let alternateElements =
    let i = ref -1
    Seq.groupBy (fun x -> incr i; !i % 2)
    >> Seq.map snd
    >> Seq.take 2
    >> List.ofSeq


/// the positions visited when following a sequence of directions
let deliveryPath = Seq.fold doMove [0,0]


/// get the number of houses visited via a sequence of moves
let numberVisited =
    Seq.ofList
    >> Seq.collect deliveryPath
    >> Seq.distinct
    >> Seq.length


/// split directions between two santas and count how many houses are visited
let visitedBySantaOrRobo dirs = alternateElements dirs |> numberVisited

    

//
// Main functions
//

let part1() =
    [seq (getInput())]
    |> numberVisited
    |> printfn "Part 1 - %d houses visited"

let part2() =
    getInput()
    |> visitedBySantaOrRobo
    |> printfn "Part 2 - %d houses visited" 


// Tests

let test1() =
    let testData = [|
        ">", 2;
        "^>v<", 4;
        "^v^v^v^v^v", 2
    |]
    let test = AdventUtils.testResultIsExpected "1" (fun x -> numberVisited [x])
    testData |> Seq.iter test

let test2() =
    let testData = [|
        "^v", 3;
        "^>v<", 3;
        "^v^v^v^v^v", 11
    |]
    let test = AdventUtils.testResultIsExpected "2" visitedBySantaOrRobo
    testData |> Seq.iter test



part1()
part2()

test1()
test2()