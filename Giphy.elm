module Main exposing (..)

import Html exposing (Html, a, button, div, h1, img, input, p, text)
import Html.Attributes exposing (class, rel, href, src, target, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, at, field)
import Array


type alias Pagination =
    { count : Int
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
    }


type Msg
    = FetchGif
    | UpdateList (Result Http.Error Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchGif ->
            ( model, fetchGif )

        UpdateList (Ok newModel) ->
            ( newModel, Cmd.none )

        UpdateList (Err err) ->
            ( model, Cmd.none )


fetchGif : Cmd Msg
fetchGif =
    let
        url =
            "https://api.giphy.com/v1/gifs/trending?api_key=dc6zaTOxFJmzC"

        request =
            Http.get url modelDecoder
    in
        Http.send UpdateList request


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2 Model
        (field "pagination" paginationDecoder)
        (field "data" (Decode.list imagesDecoder))


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
    ( initialState, fetchGif )


initialState : Model
initialState =
    Model
        { count = 0, total = 0, offset = 0 }
        []


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ div [ class "jumbotron-fluid" ] [ imagesView model ]
        ]


imagesView : Model -> Html Msg
imagesView model =
    let
        offset =
            model.pagination.offset

        inList =
            Array.get offset <| Array.fromList model.images
    in
        case inList of
            Just image ->
                imageView image

            Nothing ->
                div [] []


imageView : Image -> Html Msg
imageView image =
    div [ class "row" ]
        [ div [ class "col-sm" ]
            [ a [ href image.originalUrl, target "_blank", rel "noreferrer" ]
                [ img [ src image.fixedHeight ] []
                ]
            ]
        , div [ class "col-sm" ] [ imageDetails image ]
        ]


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
