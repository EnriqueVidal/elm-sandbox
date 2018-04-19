module Main exposing (..)

import Array
import Html exposing (Html, a, button, code, div, h1, img, input, nav, p, text)
import Html.Attributes exposing (class, disabled, href, rel, src, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, at, field)


type alias Pagination =
    { limit : Int
    , total : Int
    , offset : Int
    }


type alias Image =
    { id : String
    , url : String
    , username : String
    , title : String
    , originalUrl : String
    , fixedHeight : String
    }


type alias Model =
    { pagination : Pagination
    , images : List Image
    , error : Maybe String
    }


type alias Offset =
    Int


type alias Limit =
    Int


type Msg
    = FetchGifs Offset Limit
    | UpdateList (Result Http.Error Model)
    | PreviousImage
    | NextImage


type Direction
    = Left
    | Right


initialState : Model
initialState =
    Model
        { limit = 25, total = 0, offset = 0 }
        []
        Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchGifs offset limit ->
            ( model, fetchGifs offset limit )

        PreviousImage ->
            ( { model | pagination = updateOffset Left model.pagination }, Cmd.none )

        NextImage ->
            ( { model | pagination = updateOffset Right model.pagination }, refetchMaybe model )

        UpdateList (Ok payload) ->
            ( processPayload model payload, Cmd.none )

        UpdateList (Err error) ->
            ( { model | error = Just (toString error) }, Cmd.none )


refetchMaybe : Model -> Cmd Msg
refetchMaybe model =
    let
        pagination =
            model.pagination

        offset =
            pagination.offset + 1

        limit =
            pagination.limit

        inState =
            List.length model.images
    in
        if (offset % limit == 0) && (offset >= inState) then
            fetchGifs offset limit
        else
            Cmd.none


processPayload : Model -> Model -> Model
processPayload model payload =
    let
        pagination =
            payload.pagination

        images =
            List.append model.images payload.images
    in
        { model | pagination = pagination, images = images }


updateOffset : Direction -> Pagination -> Pagination
updateOffset direction pagination =
    case direction of
        Left ->
            { pagination | offset = pagination.offset - 1 }

        Right ->
            { pagination | offset = pagination.offset + 1 }


fetchGifs : Offset -> Limit -> Cmd Msg
fetchGifs offset limit =
    let
        url =
            "https://api.giphy.com/v1/gifs/trending?api_key=dc6zaTOxFJmzC&offset=" ++ toString offset ++ "&limit=" ++ toString limit

        request =
            Http.get url modelDecoder
    in
        Http.send UpdateList request


modelDecoder : Decoder Model
modelDecoder =
    Decode.map3 Model
        (field "pagination" paginationDecoder)
        (field "data" (Decode.list imagesDecoder))
        (Decode.maybe Decode.string)


paginationDecoder : Decoder Pagination
paginationDecoder =
    Decode.map3 Pagination
        (field "count" Decode.int)
        (field "total_count" Decode.int)
        (field "offset" Decode.int)


imagesDecoder : Decoder Image
imagesDecoder =
    Decode.map6 Image
        (field "id" Decode.string)
        (field "url" Decode.string)
        (field "username" Decode.string)
        (field "title" Decode.string)
        (at [ "images", "original", "url" ] Decode.string)
        (at [ "images", "fixed_height", "url" ] Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    let
        model =
            initialState

        pagination =
            model.pagination
    in
        ( model, fetchGifs 0 pagination.limit )


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ alertView model
        , div [ class "container-fluid" ]
            [ div [ class "jumbotron-fluid" ] [ imagesView model ]
            ]
        ]


alertView : Model -> Html Msg
alertView model =
    case model.error of
        Just error ->
            div [ class "alert alert-danger" ] [ text error ]

        Nothing ->
            text ""


imagesView : Model -> Html Msg
imagesView model =
    let
        pagination =
            model.pagination

        inList =
            Array.get pagination.offset <| Array.fromList model.images
    in
        case inList of
            Just image ->
                imageView image pagination

            Nothing ->
                h1 [] [ text "Loading..." ]


imageView : Image -> Pagination -> Html Msg
imageView image pagination =
    div [ class "row" ]
        [ div [ class "col-sm" ]
            [ a [ href image.originalUrl, target "_blank", rel "noreferrer" ]
                [ img [ src image.fixedHeight ] []
                ]
            , div [ class "row" ]
                [ nav [ class "controls" ]
                    [ prevButton pagination
                    , nextButton pagination
                    ]
                ]
            ]
        , div [ class "col-sm" ] [ imageDetails image ]
        ]


prevButton : Pagination -> Html Msg
prevButton pagination =
    let
        disable =
            pagination.offset < 1
    in
        navigation PreviousImage disable "Back"


nextButton : Pagination -> Html Msg
nextButton pagination =
    let
        offset =
            pagination.offset

        total =
            pagination.total

        disable =
            offset == total
    in
        navigation NextImage disable "Next"


navigation : Msg -> Bool -> String -> Html Msg
navigation msg disable caption =
    div [ class "col-sm" ]
        [ button [ class "btn btn-light", onClick msg, disabled disable ] [ text caption ] ]


imageDetails : Image -> Html Msg
imageDetails image =
    div []
        [ p [ class "font-weight-bold" ] [ text image.title ]
        , p [] [ text image.username ]
        , p [] [ a [ href image.url ] [ text "Giphy Url" ] ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { update = update
        , init = init
        , view = view
        , subscriptions = subscriptions
        }
