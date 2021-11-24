module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array
import Browser
import Html exposing (Html, button, div, text, h1)
import Html.Events exposing (onClick)
import Html.Attributes exposing(class, classList)
import Http
import Json.Decode exposing (Decoder, field, float, int, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Random exposing (Generator)
import Dict
import Random.List
import Set


-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL

type alias Word =
    { spanish : String
    , italian : String
    , portuguese : String
    , french : String
    , norwegian : String
    , english : String
    }


type alias Model =
    { number : { a : Int, b : Int, c : Int, d : Int }
    , score : Int
    , streak : Int
    , words : List Word
    , numWords : Int
    , alternatives : List (Html Msg)
    , answer : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { number = { a = 0, b = 0, c = 0, d = 0 }
      , score = 0
      , streak = 0
      , words = []
      , numWords = 0
      , alternatives = []
      , answer = 0
      }
    , Cmd.batch [ getWords, getRandomNumber 1000 ]
    )



-- UPDATE


type Msg
    = GotWords (Result Http.Error (List Word))
    | ReceiveData
    | GotRandomInt { a : Int, b : Int, c : Int, d : Int }
    | GetRandomInt
    | GotShuffledList (List (Html Msg))
    | CorrectAnswer
    | WrongAnswer Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CorrectAnswer ->
            update GetRandomInt { model | score = model.score + 1, streak = model.streak + 1 }
        WrongAnswer n ->
            update GetRandomInt { model | streak = 0 }
        ReceiveData ->
            ( model, getWords )

        GotRandomInt n ->
            let
              {a, b, c, d} = n
            in
            case ([a, b, c, d] |> Set.fromList |> Set.toList |> List.length) of
               -- if there are any less than 4 unique numbers, that means at least one is not unique
               -- this results in problems, so we just go again and generate a new set of numbers
               4 -> ( { model | number = n }, shuffleAlternatives(viewLangAlternatives { model | number = n} "spanish") )
               _ -> update GetRandomInt model

        GotShuffledList alts ->
            ( {model | alternatives = alts } , Cmd.none )

        GetRandomInt ->
            ( model, getRandomNumber model.numWords )

        GotWords result ->
            case result of
                Ok words ->
                    update GetRandomInt { model | words = words, numWords = List.length words }

                Err _ ->
                    ( { model | words = [] }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "top-bar" ] [ text ("Score : " ++ String.fromInt model.score ++ " Streak : " ++ String.fromInt model.streak ) ]
        , h1 [] [ text ((getEntry model.words model.number.a).norwegian) ]
        , div [ class "alternatives" ] (model.alternatives)
        ]


viewLangAlternatives : Model -> String -> List (Html Msg)
viewLangAlternatives model lang =
    let
      accessors = Dict.fromList
                               [ ("spanish" , .spanish)
                               , ("italian" , .italian)
                               , ("portuguese" , .portuguese)
                               , ("french" , .french)
                               , ("norwegian" , .norwegian)
                               , ("english" , .english)]

      word1 =
        case Dict.get lang accessors of
           Nothing ->
              "unknown"
           Just acc ->
              acc (getEntry model.words model.number.a)

      word2 =
        case Dict.get lang accessors of
           Nothing ->
              "unknown"
           Just acc ->
              acc (getEntry model.words model.number.b)

      word3 =
        case Dict.get lang accessors of
           Nothing ->
              "unknown"
           Just acc ->
              acc (getEntry model.words model.number.c)

      word4 =
        case Dict.get lang accessors of
           Nothing ->
              "unknown"
           Just acc ->
              acc (getEntry model.words model.number.d)                                          
    in
    [
      button [ classList [("btn-primary", True),("btn-success", model.answer == 1)], onClick CorrectAnswer ] [ text (word1) ]
    , button [ classList [("btn-primary", True),("btn-danger", model.answer == 2)], onClick (WrongAnswer 2) ] [ text (word2) ]
    , button [ classList [("btn-primary", True),("btn-danger", model.answer == 3)], onClick (WrongAnswer 3) ] [ text (word3) ]
    , button [ classList [("btn-primary", True),("btn-danger", model.answer == 4)], onClick (WrongAnswer 4) ] [ text (word4) ]
    ]

getRandomNumber : Int -> Cmd Msg
getRandomNumber max =
    let
        randomQuad =
            quad (Random.int 0 (max - 1)) (Random.int 0 (max - 1)) (Random.int 0 (max - 1)) (Random.int 0 (max - 1))
    in
    Random.generate GotRandomInt randomQuad


getEntry : List Word -> Int -> Word
getEntry words n =
    let
        maybeVal =
            Array.fromList words |> Array.get n

        val =
            Maybe.withDefault { spanish = "null", italian = "null", portuguese = "null", french = "null", norwegian = "null", english = "null" } maybeVal
    in
    val

shuffleAlternatives : List (Html Msg) -> Cmd Msg
shuffleAlternatives list =
    Random.generate GotShuffledList (list |> Random.List.shuffle)

-- Generates a record with 4 random numbers
quad : Generator a -> Generator b -> Generator c -> Generator d -> Generator { a : a, b : b, c : c, d : d }
quad genA genB genC genD =
    Random.map4 (\a b c d -> { a = a, b = b, c = c, d = d }) genA genB genC genD



-- HTTP


getWords : Cmd Msg
getWords =
    Http.get
        { url = "/words.json"
        , expect = Http.expectJson GotWords wordsDecoder
        }



-- JSON


wordDecoder : Decoder Word
wordDecoder =
    Json.Decode.succeed Word
        |> required "spanish" string
        |> required "italian" string
        |> required "portuguese" string
        |> required "french" string
        |> required "norwegian" string
        |> required "english" string


wordsDecoder : Decoder (List Word)
wordsDecoder =
    Json.Decode.list wordDecoder
