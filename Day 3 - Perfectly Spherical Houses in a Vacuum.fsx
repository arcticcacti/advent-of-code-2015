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


/// get the number of houses visited via a sequence of moves
let numberVisited =
    Seq.fold doMove [0,0] >> Seq.distinct >> Seq.length


//
// Main functions
//

let part1() =
    getInput()
    |> numberVisited
    |> printfn "Part 1 - %d houses visited" 


// Tests

let test1() =
    let testData = [|
        ">", 2;
        "^>v<", 4;
        "^v^v^v^v^v", 2
    |]
    let test = AdventUtils.testResultIsExpected "1" numberVisited
    testData |> Seq.iter test


part1()

test1()