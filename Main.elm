module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Deck exposing (..)
import Random
import Svg
import Svg.Attributes as SvgAtt


main : Program Never Model Msg
main =
    program { init = newGame, update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type GameState
    = Player1Turn
    | Player2Turn
    | Player3Turn
    | Player4Turn


type alias Model =
    { deck : List Card
    , player_1_hand : List Card
    , player_2_hand : List Card
    , player_3_hand : List Card
    , player_4_hand : List Card
    , game_state : GameState
    }


type Msg
    = Shuffle (List Int)
    | ShuffleDeck
    | SortDeck
    | Deal


newGame : ( Model, Cmd Msg )
newGame =
    ( Model generateDeck [] [] [] [] Player1Turn, randomList Shuffle 48 )


randomList : (List Int -> Msg) -> Int -> Cmd Msg
randomList msg len =
    Random.int 1 48
        |> Random.list len
        |> Random.generate msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle xs ->
            let
                newDeck =
                    shuffleDeck model.deck xs
            in
                Model newDeck [] [] [] [] model.game_state ! []

        ShuffleDeck ->
            ( model, randomList Shuffle 48 )

        SortDeck ->
            ( { model | deck = model.deck |> List.sortBy .index }, Cmd.none )

        Deal ->
            let
                ( dealtCard, restOfDeck ) =
                    deal model.deck

                ( player_hand, next_turn ) =
                    case model.game_state of
                        Player1Turn ->
                            ( model.player_1_hand, Player2Turn )

                        Player2Turn ->
                            ( model.player_2_hand, Player3Turn )

                        Player3Turn ->
                            ( model.player_3_hand, Player4Turn )

                        Player4Turn ->
                            ( model.player_4_hand, Player1Turn )
            in
                case dealtCard of
                    Just card ->
                        case restOfDeck of
                            Just cards ->
                                case model.game_state of
                                    Player1Turn ->
                                        { model | player_1_hand = { card | facing = Up } :: player_hand, deck = cards, game_state = next_turn } ! []

                                    Player2Turn ->
                                        { model | player_2_hand = { card | facing = Up } :: player_hand, deck = cards, game_state = next_turn } ! []

                                    Player3Turn ->
                                        { model | player_3_hand = { card | facing = Up } :: player_hand, deck = cards, game_state = next_turn } ! []

                                    Player4Turn ->
                                        { model | player_4_hand = { card | facing = Up } :: player_hand, deck = cards, game_state = next_turn } ! []

                            Nothing ->
                                model ! []

                    Nothing ->
                        model ! []



-- VIEW


displayDeckSvg : List Card -> List (Svg.Svg msg)
displayDeckSvg cards =
    List.map2 displayCardSvg (List.reverse cards) (List.range 0 (List.length cards))


view : Model -> Html Msg
view model =
    let
        displayDeck =
            displayDeckSvg model.deck

        displayPlayer1Hand =
            displayDeckSvg model.player_1_hand

        displayPlayer2Hand =
            displayDeckSvg model.player_2_hand

        displayPlayer3Hand =
            displayDeckSvg model.player_3_hand

        displayPlayer4Hand =
            displayDeckSvg model.player_4_hand
    in
        div []
            [ div []
                [ h4 [] [ text "Deck" ]
                , h5 [] [ text <| toString model.deck ]
                , Svg.svg
                    [ SvgAtt.width "1200", SvgAtt.height "200", SvgAtt.viewBox "0 0 400 200" ]
                    displayDeck
                ]
            , div []
                [ button [ onClick Deal ] [ text "Deal" ] ]
            , table []
                [ tr []
                    [ td []
                        [ div []
                            [ h4 [] [ text "Player 1 Hand" ]
                            , h5 [] [ text <| toString model.player_1_hand ]
                            , Svg.svg
                                [ SvgAtt.width "500", SvgAtt.height "200", SvgAtt.viewBox "0 0 400 100" ]
                                displayPlayer1Hand
                            ]
                        ]
                    , td []
                        [ div []
                            [ h4 [] [ text "Player 2 Hand" ]
                            , h5 [] [ text <| toString model.player_2_hand ]
                            , Svg.svg
                                [ SvgAtt.width "500", SvgAtt.height "200", SvgAtt.viewBox "0 0 400 100" ]
                                displayPlayer2Hand
                            ]
                        ]
                    ]
                , tr []
                    [ td []
                        [ div []
                            [ h4 [] [ text "Player 3 Hand" ]
                            , h5 [] [ text <| toString model.player_3_hand ]
                            , Svg.svg
                                [ SvgAtt.width "500", SvgAtt.height "200", SvgAtt.viewBox "0 0 400 100" ]
                                displayPlayer3Hand
                            ]
                        ]
                    , td []
                        [ div []
                            [ h4 [] [ text "Player 4 Hand" ]
                            , h5 [] [ text <| toString model.player_4_hand ]
                            , Svg.svg
                                [ SvgAtt.width "500", SvgAtt.height "200", SvgAtt.viewBox "0 0 400 100" ]
                                displayPlayer4Hand
                            ]
                        ]
                    ]
                ]
            ]
