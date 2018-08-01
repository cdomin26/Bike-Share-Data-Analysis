open System.Threading

//
// F# program to analyze Divvy daily ride data.
//
// <<Christian Dominguez>>
// U. of Illinois, Chicago
// CS 341, Spring 2018
// Project #04
//

#light

let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides

//------------------------------------------------------------------------//
let percentageFemale L = 
  let gender = List.filter (fun (e : int list) -> (List.item 2 e) = 2 ) L
  let numFemales = List.length gender
  numFemales
//------------------------------------------------------------------------//
let percentageMale L = 
  let gender = List.filter (fun (e : int list) -> (List.item 2 e) = 1 ) L
  let numMales = List.length gender
  numMales
//------------------------------------------------------------------------//
let rec _rideDuration start L =
  match L with
  | [] -> start
  | e::rest -> _rideDuration (start+e) rest
//------------------------------------------------------------------------//
let rideDuration L =
  let time = List.map (fun (e : int list) -> (List.item 1 e)) L
  let sum = _rideDuration 0 time
  sum
//------------------------------------------------------------------------//
let maleAge L =
  let males = List.filter (fun (e : int list) -> (List.item 2 e) = 1 ) L
  let ages = List.map (fun (e : int list) -> (List.item 3 e)) males
  let currentAge = List.map (fun e -> 2018 - e) ages
  List.sum currentAge
//------------------------------------------------------------------------//
let femaleAge L =
  let females = List.filter (fun (e : int list) -> (List.item 2 e) = 2 ) L
  let ages = List.map (fun (e : int list) -> (List.item 3 e)) females
  let currentAge = List.map (fun e -> 2018 - e) ages
  List.sum currentAge
//------------------------------------------------------------------------//

[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists --- [ [1308;321;2;1991]; ... ]
  //
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents
  //printfn "%A" ridedata
  
  //Data used in all of the analysis
  let N = List.length ridedata
  let Males = percentageMale ridedata
  let malePercentage = (double (Males)) / (double (N)) * 100.00
  let Females = percentageFemale ridedata
  let femalePercentage = (double (Females)) / (double (N)) * 100.00
  let seconds = rideDuration ridedata
  let avgMinutes = (double(seconds/60)) / (double(N))
  let maleAges = maleAge ridedata
  let avgAgeMale = (double (maleAges)) / (double(Males))
  let femaleAges = femaleAge ridedata
  let avgAgeFemale = (double(femaleAges)) / (double(Females))

  //Printing the data retrieved from the analysis
  printfn "# of rides: %A" N
  printfn "%% of male riders: %A" malePercentage
  printfn "%% of female riders: %A" femalePercentage
  printfn "Avg duration: %A mins" avgMinutes
  printfn "Avg age of male riders: %A" avgAgeMale
  printfn "Avg age of female riders: %A" avgAgeFemale
  0 
