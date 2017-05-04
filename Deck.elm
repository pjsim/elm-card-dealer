module Deck exposing (..)

import Svg
import Svg.Attributes as SvgAtt


-- MODEL


type alias Card =
    { index : Int
    , suit : String
    , rank : Int
    , facing : Facing
    }


generateDeck : List Card
generateDeck =
    let
        deckIndex =
            13 * 4 |> List.range 1

        suits =
            [ "Spades", "Clubs", "Diamonds", "Hearts" ] |> List.repeat 13 |> List.concat |> List.sort

        ranks =
            List.range 2 13 |> List.repeat 4 |> List.concat
    in
        List.map3 createCard deckIndex suits ranks


createCard : Int -> String -> Int -> Card
createCard index rank suit =
    Card index rank suit Down


type Facing
    = Down
    | Up


deal : List Card -> ( Maybe Card, Maybe (List Card) )
deal deck =
    let
        dealtCard =
            List.head deck

        restOfDeck =
            List.tail deck
    in
        ( dealtCard, restOfDeck )


shuffleDeck : List Card -> List comparable -> List Card
shuffleDeck deck xs =
    List.map2 (,) deck xs
        |> List.sortBy Tuple.second
        |> List.unzip
        |> Tuple.first


displayCardSvg : Card -> Int -> Svg.Svg msg
displayCardSvg card position =
    let
        multipliedPosition =
            position * 20

        translate_text =
            "translate(" ++ toString (70 + multipliedPosition) ++ ",35)"
    in
        Svg.g []
            [ Svg.rect
                [ SvgAtt.x <| toString <| 10 + multipliedPosition
                , SvgAtt.y "10"
                , SvgAtt.width "85"
                , SvgAtt.height "100"
                , SvgAtt.rx "10"
                , SvgAtt.ry "10"
                , SvgAtt.style "fill:white;stroke:black;stroke-width:4"
                ]
                []
            , Svg.text_ [ SvgAtt.textAnchor "middle", SvgAtt.transform translate_text ] [ Svg.text <| shortDisplayCardString card ]
            ]


shortDisplayCardString : Card -> String
shortDisplayCardString card =
    if card.facing == Down then
        "Back"
    else
        let
            suit =
                String.left 1 card.suit
        in
            case card.rank of
                1 ->
                    "A " ++ suit

                13 ->
                    "K " ++ suit

                12 ->
                    "Q " ++ suit

                11 ->
                    "J " ++ suit

                _ ->
                    toString card.rank ++ " " ++ suit


displayCardString : Card -> String
displayCardString card =
    if card.facing == Down then
        "Face Down"
    else
        case card.rank of
            1 ->
                "Ace of " ++ card.suit

            13 ->
                "King of " ++ card.suit

            12 ->
                "Queen of " ++ card.suit

            11 ->
                "Jack of " ++ card.suit

            _ ->
                toString card.rank ++ " of " ++ card.suit
