module Main exposing (..)

import Array
import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, p, small, span, text)
import Html.Attributes exposing (class, classList, disabled, href, title)
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
      , targetLanguage = ""
      , sourceLanguage = ""
      }
    , getWords
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
    | GotLanguages String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotLanguages source destination ->
            update GetRandomInt { model | sourceLanguage = source, targetLanguage = destination }

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
            [ div [ class "source-language-select" ] [ a [ title (sourceLang.localName |> capitalize) ] [ p [] [ text sourceLang.flag ] ] ]
            , div [ class "score" ]
                [ div [ class "scoreboard" ]
                    [ span [] [ text "Score :" ]
                    , span [ class "score-value" ] [ text (String.fromInt model.score) ]
                    , span [] [ text "Streak :" ]
                    , span [ class "score-value" ] [ text (String.fromInt model.streak) ]
                    ]
                ]
            , div [ class "target-language-select" ] [ a [ title (targetLang.localName |> capitalize) ] [ p [] [ text targetLang.flag ] ] ]
            ]
        , div [ class "container" ]
            (if model.targetLanguage == "" || model.sourceLanguage == "" then
                viewLangSelector model

             else
                [ h1 [ class "question" ] [ text (getSourceWord model) ]
                , div [ class "alternatives" ] (viewLangAlternatives model)
                ]
            )
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


viewLangSelector : Model -> List (Html Msg)
viewLangSelector model =
    [ div [ class "lang-selector" ]
        [ h1 [] [ text "I want to guess what words in this language..." ]
        , div [ class "lang-selector-list" ]
            (List.map
                (\l ->
                    div [ class "list-lang" ]
                        [ button
                            [ class
                                (if l == model.sourceLanguage then
                                    "btn-success"

                                 else if l == model.targetLanguage then
                                    "btn-danger"

                                 else
                                    "btn-primary"
                                )
                            , onClick (GotLanguages l model.targetLanguage)
                            , disabled (l == model.targetLanguage)
                            ]
                            [ text (langMetadata l).flag ]
                        , p [] [ text ((langMetadata l).localName |> capitalize) ]
                        ]
                )
                [ "english", "norwegian", "portuguese", "french", "italian", "spanish" ]
            )
        , h1 [] [ text "...means in this language" ]
        , div [ class "lang-selector-list" ]
            (List.map
                (\l ->
                    div [ class "list-lang" ]
                        [ button
                            [ class
                                (if l == model.targetLanguage then
                                    "btn-success"

                                 else if l == model.sourceLanguage then
                                    "btn-danger"

                                 else
                                    "btn-primary"
                                )
                            , onClick (GotLanguages model.sourceLanguage l)
                            , disabled (l == model.sourceLanguage)
                            ]
                            [ text (langMetadata l).flag ]
                        , p [] [ text ((langMetadata l).localName |> capitalize) ]
                        ]
                )
                [ "spanish", "italian", "french", "portuguese", "norwegian", "english" ]
            )
        ]
    ]


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
    Maybe.withDefault (LanguageMetadata "" "??" "unknown") maybeLang


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


capitalize : String -> String
capitalize string =
    case String.uncons string of
        Nothing ->
            ""

        Just ( head, tail ) ->
            String.cons (Char.toUpper head) tail



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
