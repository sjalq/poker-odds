module Main

open Program

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
            printfn "My best hand: %A" (bestPokerHand (Set.ofArray (myHand <!> visibleCards)))
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
