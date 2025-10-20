module Tests

open Xunit
open Program

// let arbitraryRank : Gen<Rank> =
//     Gen.elements [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace]

// let arbitrarySuit : Gen<Suit> =
//     Gen.elements [Spades; Hearts; Diamonds; Clubs]

// let arbitraryCard : Gen<Card> =
//     Gen.map2 (fun s r -> (s, r)) arbitrarySuit arbitraryRank

// let arbitraryCardArray : Gen<Card array> =
//     Gen.listOf arbitraryCard |> Gen.map List.toArray

// [<Property>]
// let ``fastBestStraight and slowBestStraight should return same results``() =
//     Prop.forAll (Arb.fromGen arbitraryCardArray) (fun cards ->
//         let counts = Array.zeroCreate 13
//         let fastResult = fastBestStraight counts cards
//         let slowResult = slowBestStraight cards
//         fastResult = slowResult
//    )

[<Fact>]
let ``Test that true equals true``() =
    Assert.True(true)