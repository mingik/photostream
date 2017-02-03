module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random
import Http
import Json.Decode exposing (..)

main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }

-- MODEL

type alias Photo =
    { url : String }

type ThumbnailSize
    = Small
    | Medium
    | Large

type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }

urlPrefix : String
urlPrefix =
    "./images/"

initialModel : Model
initialModel =
    { photos = 
          [ { url = urlPrefix ++ "1.jpg" }
          , { url = urlPrefix ++ "2.jpg" }
          , { url = urlPrefix ++ "3.jpg" }
          ]
    , selectedUrl = urlPrefix ++ "1.jpg"
    , chosenSize = Medium
    }
             
photoArray : Model -> Array Photo
photoArray model =
    Array.fromList model.photos

-- VIEW
        
view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Cats Stream" ]
        , button
            [ onClick ChooseCat ]
            [ text "ChooseCat!" ]
        , button
              [ onClick AddCat ]
              [ text "AddCat!" ]
        , button
              [ onClick RemoveCats ]
              [ text "RemoveCats" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src (model.selectedUrl)
            ]
            []
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (thumbnail.url)
        , classList [ ( "selected", selectedUrl == thumbnail.url ) ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (SetSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"
        Medium ->
            "med"
        Large ->
            "large"
            
-- UPDATE

type Msg
    = SelectByUrl String
    | SelectByIndex Int
    | ChooseCat
    | ChosenCat Int
    | AddCat
    | NewImg (Result Http.Error String)
    | RemoveCats
    | SetSize ThumbnailSize


getPhotoUrl : Int -> Model -> String
getPhotoUrl index model =
    case Array.get index (photoArray model) of
        Just photo ->
            photo.url

        Nothing ->
            ""

fetchImage : Cmd Msg
fetchImage =
    let
        url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats"
    in
        Http.send NewImg (Http.get url decodeImg)

chooseCat : Model -> Cmd Msg
chooseCat model =
    Random.generate ChosenCat (Random.int 0 ((Array.length (photoArray model)) - 1))
    
            
decodeImg : Decoder String
decodeImg =
    at ["data", "image_url"] string

removeCat : Photo -> Bool
removeCat = \photo -> not (String.contains "giphy" photo.url)
        
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectByIndex index ->
            ( { model | selectedUrl = getPhotoUrl index model }, Cmd.none )

        SelectByUrl url ->
            ( { model | selectedUrl = url }, Cmd.none )

        ChooseCat ->
            (model, chooseCat model)

        ChosenCat index ->
            ( { model | selectedUrl = case Array.get index (photoArray model) of
                                          Just photo ->
                                              photo.url
                                          Nothing ->
                                              ""
              }, Cmd.none )
        AddCat ->
            (model, fetchImage)

        NewImg (Ok newImg) ->
            ( { model | photos = model.photos ++ [{url = newImg}] }, Cmd.none )

        NewImg (Err _) ->
            (model, Cmd.none)

        RemoveCats ->
            ( { model | photos = List.filter removeCat model.photos, selectedUrl = urlPrefix ++ "2.jpg"  }, Cmd.none )

        SetSize size ->
            ( { model | chosenSize = size }, Cmd.none )


