open System

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F# !"

type Suit =
    | Spades
    | Hearts
    | Diamonds
    | Clubs

type Rank =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

    member inline x.toInt =
        match x with
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5
        | Six -> 6
        | Seven -> 7
        | Eight -> 8
        | Nine -> 9
        | Ten -> 10
        | Jack -> 11
        | Queen -> 12
        | King -> 13
        | Ace -> 14

    static member inline fromInt i =
        match i with
        | 2 -> Two
        | 3 -> Three
        | 4 -> Four
        | 5 -> Five
        | 6 -> Six
        | 7 -> Seven
        | 8 -> Eight
        | 9 -> Nine
        | 10 -> Ten
        | 11 -> Jack
        | 12 -> Queen
        | 13 -> King
        | 14 -> Ace
        | _ -> failwith "Invalid rank"


type Card = Suit * Rank


type PokerHand =
    | Nothing // theoretically impossible
    | HighCard of Rank
    | Pair of Rank
    | TwoPair of Rank * Rank
    | ThreeOfAKind of Rank
    | Straight of Rank
    | Flush of Suit
    | FullHouse of Rank * Rank
    | FourOfAKind of Rank
    | StraightFlush of Rank
    | RoyalFlush


let nextRank rank =
    match rank with
    | Ace -> Two
    | Two -> Three
    | Three -> Four
    | Four -> Five
    | Five -> Six
    | Six -> Seven
    | Seven -> Eight
    | Eight -> Nine
    | Nine -> Ten
    | Ten -> Jack
    | Jack -> Queen
    | Queen -> King
    | King -> Ace


let fullDeck =
    [ for suit in [ Spades; Hearts; Diamonds; Clubs ] do
          for rank in
              [ Ace
                Two
                Three
                Four
                Five
                Six
                Seven
                Eight
                Nine
                Ten
                Jack
                Queen
                King ] do
              yield (suit, rank) ]


let log msg v =
    printfn "%s: %A" msg v
    v


let rnd = System.Random()

let shuffle deck =
    deck |> List.sortBy (fun _ -> rnd.Next())

let pick n deck =
    let picked = deck |> List.take n
    let rest = deck |> List.skip n
    (picked, rest)


let removeCard card deck =
    deck |> List.filter (fun c -> c <> card)


let removeCards cards deck =
    cards
    |> List.fold (fun d c -> removeCard c d) deck


let removeRank rank deck =
    deck |> Array.filter (fun (_, r) -> r <> rank)


let removeSuit suit deck =
    deck |> List.filter (fun (s, _) -> s <> suit)


let safeSeqMax seq =
    match Seq.isEmpty seq with
    | true -> None
    | false -> Some(Seq.max seq)


let safeArrayMax arr =
    match Array.isEmpty arr with
    | true -> None
    | false -> Some(Array.max arr)


let inline fastArrayMax arr =
    if Array.isEmpty arr then
        None
    else
        for i in 1 .. (Array.length arr - 1) do
            if arr.[i] > arr.[0] then
                arr.[0] <- arr.[i]

        arr.[0] |> Some


let safeSeqMaxBy f seq =
    match Seq.isEmpty seq with
    | true -> None
    | false -> Some(Seq.maxBy f seq)

let setGroupBy f set =
    set
    |> Set.toArray
    |> Array.groupBy f
    |> Array.map (fun (k, v) -> (k, Set.ofSeq v))
    |> Set.ofArray


let inline fastCountRanks n (cards: Card array) =
    let counts = Array.zeroCreate 13

    for i in 0 .. (Array.length cards - 1) do
        let (_, rank) = cards.[i]
        let rankInt = rank.toInt - 2
        counts.[rankInt] <- counts.[rankInt] + 1

    let mutable highestRankWithN = 0
    for i in 0 .. 12 do
        if counts.[i] = n then
            highestRankWithN <- i + 2

    if highestRankWithN < 2 then
        None
    else
        highestRankWithN 
        |> Rank.fromInt 
        |> Some

let inline fastBestStraight (cards: Card array) =
    let ranks = Array.zeroCreate 13

    for i in 0 .. (Array.length cards - 1) do
        let (_, rank) = cards.[i]
        let rankInt = rank.toInt - 2
        ranks.[rankInt] <- ranks.[rankInt] + 1

    let mutable highestStraightRank = 0
    for i in 0 .. 8 do
        if ranks.[i] > 0 && ranks.[i + 1] > 0 && ranks.[i + 2] > 0 && ranks.[i + 3] > 0 && ranks.[i + 4] > 0 then
            highestStraightRank <- i + 2 + 4

    if highestStraightRank < 6 then
        None
    else
        highestStraightRank
        |> Rank.fromInt
        |> Straight
        |> Some

let slowBestStraight cards = 
    cards
    |> Array.map snd
    |> Seq.sortBy (fun rank -> rank)
    |> Seq.windowed 5
    |> Seq.filter (fun ranks -> Seq.forall2 (fun a b -> nextRank a = b) ranks (Seq.skip 1 ranks))
    |> Seq.map (fun ranks -> Seq.max ranks)
    |> safeSeqMax
    |> Option.map Straight


let bestPokerHand (visibleCards: Card Set) =
    let visibleCards = visibleCards |> Set.toArray

    let inline bestN_ofAKind n cards =
        fastCountRanks n cards

    let bestHighCard =
        visibleCards
        |> bestN_ofAKind 1
        |> Option.map HighCard

    let bestPair = visibleCards |> bestN_ofAKind 2 |> Option.map Pair

    let bestThreeOfAKind =
        visibleCards
        |> bestN_ofAKind 3
        |> Option.map ThreeOfAKind

    let bestFourOfAKind =
        visibleCards
        |> bestN_ofAKind 4
        |> Option.map FourOfAKind

    let bestTwoPair =
        let secondBestPair =
            match bestPair with
            | Some (Pair rank) -> visibleCards |> removeRank rank |> bestN_ofAKind 2
            | _ -> None

        match bestPair, secondBestPair with
        | Some (Pair best), Some secondBest -> Some(TwoPair(best, secondBest))
        | _ -> None

    let bestFullHouse =
        let pair =
            match bestThreeOfAKind with
            | Some (ThreeOfAKind rank) -> visibleCards |> removeRank rank |> bestN_ofAKind 2
            | _ -> None

        match bestPair, bestThreeOfAKind with
        | Some (Pair bestPair_), Some (ThreeOfAKind bestThree_) -> Some(FullHouse(bestThree_, bestPair_))
        | _ -> None

    let bestFlush =
        visibleCards
        |> Array.groupBy (fun (suit, _) -> suit)
        |> Array.filter (fun (_, cards) -> Seq.length cards >= 5)
        |> Array.map (fun (suit, _) -> suit)
        |> safeArrayMax
        |> Option.map Flush

    let bestStraight =
        fastBestStraight visibleCards 

    let bestStraightFlush =
        match bestFlush, bestStraight with
        | Some (Flush suit), Some (Straight rank) -> Some(StraightFlush rank)
        | _ -> None

    let bestRoyalFlush =
        match bestStraightFlush with
        | Some (StraightFlush rank) when rank = Ace -> Some RoyalFlush
        | _ -> None

    match (bestRoyalFlush,
           bestStraightFlush,
           bestFourOfAKind,
           bestFullHouse,
           bestFlush,
           bestStraight,
           bestThreeOfAKind,
           bestTwoPair,
           bestPair,
           bestHighCard)
        with
    | Some royalFlush, _, _, _, _, _, _, _, _, _ -> royalFlush
    | _, Some straightFlush, _, _, _, _, _, _, _, _ -> straightFlush
    | _, _, Some fourOfAKind, _, _, _, _, _, _, _ -> fourOfAKind
    | _, _, _, Some fullHouse, _, _, _, _, _, _ -> fullHouse
    | _, _, _, _, Some flush, _, _, _, _, _ -> flush
    | _, _, _, _, _, Some straight, _, _, _, _ -> straight
    | _, _, _, _, _, _, Some threeOfAKind, _, _, _ -> threeOfAKind
    | _, _, _, _, _, _, _, Some twoPair, _, _ -> twoPair
    | _, _, _, _, _, _, _, _, Some pair, _ -> pair
    | _, _, _, _, _, _, _, _, _, Some highCard -> highCard
    | _ -> Nothing


let randomHands myHand visibleCards numberOfOpponents =
    let deck =
        fullDeck
        |> removeCards myHand
        |> removeCards visibleCards
        |> shuffle

    let (opponentHands, _) =
        [ 1..numberOfOpponents ]
        |> List.fold
            (fun (hands, deck) _ ->
                let (hand, deck) = pick 2 deck
                (hand :: hands, deck))
            ([], deck)

    opponentHands


let winningHand hands visibleCards =
    let visibleSet = (Set.ofList visibleCards)

    hands
    |> List.map (fun hand ->
        let set = Set.union visibleSet (Set.ofList hand)
        bestPokerHand set)
    |> safeSeqMax

let stringToSuit s =
    match s with
    | "S" -> Ok Spades
    | "H" -> Ok Hearts
    | "D" -> Ok Diamonds
    | "C" -> Ok Clubs
    | _ -> Error "Invalid suit"

let stringToRank s =
    match s with
    | "2" -> Ok Two
    | "3" -> Ok Three
    | "4" -> Ok Four
    | "5" -> Ok Five
    | "6" -> Ok Six
    | "7" -> Ok Seven
    | "8" -> Ok Eight
    | "9" -> Ok Nine
    | "T" -> Ok Ten
    | "J" -> Ok Jack
    | "Q" -> Ok Queen
    | "K" -> Ok King
    | "A" -> Ok Ace
    | _ -> Error "Invalid rank"

let stringToCard (s: String) =
    match s.ToCharArray() with
    | [| rank; suit |] ->
        match stringToRank (string rank), stringToSuit (string suit) with
        | Ok rank, Ok suit -> Ok(suit, rank)
        | Error e1, Error e2 -> Error(e1 + " " + e2)
        | Error e, _ -> Error e
        | _, Error e -> Error e
    | _ -> Error "Invalid card"

let stringToCards (s: String) =
    s.Split(',')
    |> Array.map stringToCard
    |> Array.fold
        (fun acc card ->
            match acc, card with
            | Ok cards, Ok card -> Ok(card :: cards)
            | Error e1, Error e2 -> Error(e1 + " " + e2)
            | Error e, _ -> Error e
            | _, Error e -> Error e)
        (Ok [])


let safeStringToInt32 s =
    try
        Ok(Int32.Parse s)
    with
    | _ -> Error "Invalid number"


let simulatePossibleHands simulations myHand visibleCards numberOfOpponents =
    let remainingDeck =
        fullDeck
        |> removeCards myHand
        |> removeCards visibleCards

    [| 1..simulations |]
    |> Array.Parallel.map (fun _ ->
        let deck = remainingDeck |> shuffle
        let (opponentHands, deck) = pick (2 * numberOfOpponents) deck
        let opponentHands = opponentHands |> List.chunkBySize 2
        let extraVisibleCards = deck |> List.take (5 - List.length visibleCards)

        let counterArray = Array.zeroCreate 13
        let winningHand =
            winningHand (myHand :: opponentHands) (visibleCards @ extraVisibleCards)

        match winningHand with
        | Some hand when hand = bestPokerHand (Set.ofList (myHand @ visibleCards)) -> 1
        | _ -> 0)
    |> Array.sum



let startTimer () =
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    timer

let stopTimer (timer: Diagnostics.Stopwatch) =
    timer.Stop()
    timer.ElapsedMilliseconds

[<EntryPoint>]
let main argv =
    match argv with
    | [| myHand; visibleCards; numberOfOpponents; simulations |] ->
        match stringToCards myHand,
              stringToCards visibleCards,
              safeStringToInt32 numberOfOpponents,
              safeStringToInt32 simulations
            with
        | Ok myHand, Ok visibleCards, Ok numberOfOpponents, Ok simulations ->
            let timer = startTimer ()
            let wins = simulatePossibleHands simulations myHand visibleCards numberOfOpponents
            let probability = float wins / float simulations
            printfn "My hand: %A" myHand
            printfn "Visible cards: %A" visibleCards
            printfn "Number of opponents: %d" numberOfOpponents
            printfn "Number of simulations: %d" simulations
            printfn "My best hand: %A" (bestPokerHand (Set.ofList (myHand @ visibleCards)))
            printfn "Probability of winning: %f" probability

            float simulations / float (stopTimer timer)
            |> log "Time"
            |> ignore

            0
        | Error e, _, _, _ ->
            printfn "Error parsing your hand: %s" e
            1
        | _, Error e, _, _ ->
            printfn "Error parsing visible cards: %s" e
            1
        | _, _, Error e, _ ->
            printfn "Error parsing number of opponents: %s" e
            1
        | _, _, _, Error e ->
            printfn "Error parsing number of simulations: %s" e
            1
    | _ ->
        printfn "Usage: poker.exe <my hand> <visible cards> <number of opponents>"
        1
