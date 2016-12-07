open System.Security.Cryptography
open System.Text
#load "AdventUtils.fsx"

let getInput() = "bgvyzdsv"

let md5 = MD5.Create()


/// combine two elements and produce its MD5 hash in hexadecimal
let hash key number =
    key + string number
    |> Encoding.ASCII.GetBytes
    |> md5.ComputeHash 
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)


let startsWith prefix = Seq.zip prefix >> Seq.forall (fun (x,y) -> x = y)

/// the lowest integer that pairs with a key to produces a hash with the given prefix
let mineCoin prefix key =
    let performHash = hash key
    Seq.initInfinite ((+)1)
    |> Seq.find (performHash >> startsWith prefix)



//
// Main functions
//

let part1() =
    getInput()
    |> mineCoin "00000"
    |> printfn "Part 1 - found hash with suffix: %d"


let part2() =
    getInput()
    |> mineCoin "000000"
    |> printfn "Part 2 - found hash with suffix: %d"

// Tests

let test1() =
    let testData = [|
        "abcdef", "609043";
        "pqrstuv", "1048970"
    |]
    let test = AdventUtils.testResultIsExpected "1" (mineCoin "00000" >> string)
    testData |> Array.iter test


part1()
part2()

test1()
1