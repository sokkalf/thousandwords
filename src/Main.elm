module Main exposing (..)

import Array
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, float, int, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Process
import Random exposing (Generator)
import Random.List
import Set
import Task



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


type alias Alternative =
    { word : String
    , correct : Bool
    }


type alias LanguageMetadata =
    { flag : String
    , code : String
    , localName : String
    }


type alias Model =
    { number : { a : Int, b : Int, c : Int, d : Int }
    , score : Int
    , streak : Int
    , words : List Word
    , numWords : Int
    , alternatives : List Alternative
    , answered : Bool
    , targetLanguage : String
    , sourceLanguage : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { number = { a = 0, b = 0, c = 0, d = 0 }
      , score = 0
      , streak = 0
      , words = []
      , numWords = 0
      , alternatives = []
      , answered = False
      , targetLanguage = "spanish"
      , sourceLanguage = "norwegian"
      }
    , Cmd.batch [ getWords, getRandomNumber 1000 ]
    )



-- UPDATE


type Msg
    = GotWords (Result Http.Error (List Word))
    | ReceiveData
    | GotRandomInt { a : Int, b : Int, c : Int, d : Int }
    | GetRandomInt
    | GotShuffledList (List Alternative)
    | CorrectAnswer
    | WrongAnswer
    | DisplayAnswer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DisplayAnswer ->
            ( { model | answered = True }, delay 1000.0 GetRandomInt )

        CorrectAnswer ->
            update DisplayAnswer { model | answered = True, score = model.score + 1, streak = model.streak + 1 }

        WrongAnswer ->
            update DisplayAnswer { model | answered = True, streak = 0 }

        ReceiveData ->
            ( model, getWords )

        GotRandomInt n ->
            let
                { a, b, c, d } =
                    n
            in
            case [ a, b, c, d ] |> Set.fromList |> Set.toList |> List.length of
                -- if there are any less than 4 unique numbers, that means at least one is not unique
                -- this results in problems, so we just go again and generate a new set of numbers
                4 ->
                    ( { model | number = n, answered = False }, shuffleAlternatives (genLangAlternatives { model | number = n } model.targetLanguage) )

                _ ->
                    update GetRandomInt model

        GotShuffledList alts ->
            ( { model | alternatives = alts }, Cmd.none )

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
    let
        targetLang =
            langMetadata model.targetLanguage

        sourceLang =
            langMetadata model.sourceLanguage
    in
    div []
        [ div [ class "top-bar" ]
            [ div [ class "source-language-select" ] [ p [] [ text sourceLang.flag ] ]
            , div [ class "score" ]
                [ p [] [ text ("Score : " ++ String.fromInt model.score ++ " Streak : " ++ String.fromInt model.streak) ]
                ]
            , div [ class "target-language-select" ] [ p [] [ text targetLang.flag ] ]
            ]
        , div [ class "container" ]
            [ h1 [ class "question" ] [ text (getSourceWord model) ]
            , div [ class "alternatives" ] (viewLangAlternatives model)
            ]
        ]


viewLangAlternatives : Model -> List (Html Msg)
viewLangAlternatives model =
    List.map
        (\alt ->
            button
                ([ class "rounded-pill btn-primary" ]
                    ++ [ classList [ ( "btn-success", alt.correct && model.answered ), ( "btn-danger", (alt.correct == False) && model.answered ) ]
                       , onClick
                            (if alt.correct == True then
                                CorrectAnswer

                             else
                                WrongAnswer
                            )
                       ]
                )
                [ text alt.word ]
        )
        model.alternatives


langMetadata : String -> LanguageMetadata
langMetadata lang =
    let
        languages =
            Dict.fromList
                [ ( "spanish", LanguageMetadata "🇪🇸" "ES" "español" )
                , ( "italian", LanguageMetadata "🇮🇹" "IT" "italiano" )
                , ( "french", LanguageMetadata "🇫🇷" "FR" "français" )
                , ( "portuguese", LanguageMetadata "🇵🇹" "PT" "português" )
                , ( "norwegian", LanguageMetadata "🇳🇴" "NO" "norsk" )
                , ( "english", LanguageMetadata "🇬🇧" "EN" "english" )
                ]

        maybeLang =
            Dict.get lang languages
    in
    Maybe.withDefault (LanguageMetadata "🏴\u{200D}☠" "??" "unknown") maybeLang


getWord : Model -> String -> Int -> String
getWord model lang num =
    let
        accessors =
            Dict.fromList
                [ ( "spanish", .spanish )
                , ( "italian", .italian )
                , ( "portuguese", .portuguese )
                , ( "french", .french )
                , ( "norwegian", .norwegian )
                , ( "english", .english )
                ]

        word =
            case Dict.get lang accessors of
                Nothing ->
                    "unknown"

                Just acc ->
                    acc (getEntry model.words num)
    in
    word


getSourceWord : Model -> String
getSourceWord model =
    getWord model model.sourceLanguage model.number.a


genLangAlternatives : Model -> String -> List Alternative
genLangAlternatives model lang =
    let
        word1 =
            getWord model model.targetLanguage model.number.a

        word2 =
            getWord model model.targetLanguage model.number.b

        word3 =
            getWord model model.targetLanguage model.number.c

        word4 =
            getWord model model.targetLanguage model.number.d
    in
    [ { word = word1, correct = True }
    , { word = word2, correct = False }
    , { word = word3, correct = False }
    , { word = word4, correct = False }
    ]


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


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


shuffleAlternatives : List Alternative -> Cmd Msg
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
