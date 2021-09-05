module Client exposing (..)

import Browser
import Browser.Navigation
import Html exposing (Html, blockquote, div, h1, p, strong, text)
import Html.Attributes exposing (href, rel)
import Http
import Json.Decode
import Json.Encode
import Platform exposing (Task)
import Protocol
import Protocol.Auto
import Task
import Url
import Url.Parser
import Webapp.Client



-- port websocketConnected : (Int -> msg) -> Sub msg
--
--
-- port websocketIn : (String -> msg) -> Sub msg
--
--
-- port websocketOut : String -> Cmd msg


webapp :
    { application : Webapp.Client.Program Flags Model Msg
    , sendToServer : Protocol.MsgFromClient -> Task Http.Error (Result String Protocol.MsgFromServer)
    }
webapp =
    Webapp.Client.application
        { application =
            { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            , onUrlRequest = OnUrlRequest
            , onUrlChange = OnUrlChange
            }
        , ports =
            { websocketConnected = \_ -> Sub.none -- websocketConnected
            , websocketIn = \_ -> Sub.none -- websocketIn
            }
        , protocol =
            { updateFromServer = updateFromServer
            , clientMsgEncoder = Protocol.Auto.encodeProtocolMsgFromClient
            , serverMsgDecoder =
                Json.Decode.oneOf
                    [ Protocol.Auto.decodeProtocolMsgFromServer
                    , Json.Decode.map Protocol.ClientServerVersionMismatch Json.Decode.value
                    , Json.Decode.succeed (Protocol.ClientServerVersionMismatch Json.Encode.null)
                    ]
            , errorDecoder = Json.Decode.string
            }
        }


main : Webapp.Client.Program Flags Model Msg
main =
    webapp.application


{-| Clients send messages to Server with this
-}
sendToServer : Protocol.MsgFromClient -> Cmd Msg
sendToServer =
    webapp.sendToServer >> Task.attempt OnMsgFromServer


type alias Flags =
    {}


type alias Model =
    { navKey : Browser.Navigation.Key
    , alerts : List Protocol.Alert
    , page : Protocol.Page
    }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | OnMsgFromServer (Result Http.Error (Result String Protocol.MsgFromServer))
    | SendMessage Protocol.MsgFromClient


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { navKey = navKey
            , alerts = []
            , page = Protocol.HomePage
            }

        cmd =
            Cmd.none
    in
    updateFromURL url model
        |> Tuple.mapSecond (\otherCmd -> Cmd.batch [ cmd, otherCmd ])


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Elm Webapp Client"
        [ Html.node "link"
            [ rel "stylesheet"
            , href "https://cdn.jsdelivr.net/npm/@exampledev/new.css@1.1.2/new.min.css"
            ]
            []
        , div [] (List.map viewAlert model.alerts)
        , case model.page of
            Protocol.NotFoundPage ->
                viewHomepage

            Protocol.HomePage ->
                viewHomepage
        ]


viewHomepage : Html msg
viewHomepage =
    div []
        [ h1 []
            [ text "Welcome to "
            , Html.a [ href "https://github.com/choonkeat/elm-webapp" ] [ text "elm-webapp" ]
            ]
        ]


viewAlert : Protocol.Alert -> Html msg
viewAlert alert =
    blockquote []
        [ strong [] [ text alert.title ]
        , p [] [ text alert.body ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest (Browser.Internal url) ->
            ( { model | alerts = [] }
            , Browser.Navigation.pushUrl model.navKey (Url.toString url)
            )

        OnUrlRequest (Browser.External "") ->
            -- when we have `a` with `onClick` but without `href`
            -- we'll get this event; should ignore
            ( model, Cmd.none )

        OnUrlRequest (Browser.External urlString) ->
            ( model, Browser.Navigation.load urlString )

        OnUrlChange url ->
            updateFromURL url model

        OnMsgFromServer (Err err) ->
            -- http error
            ( { model | alerts = Protocol.Alert "Error" (Debug.toString err) :: model.alerts }, Cmd.none )

        OnMsgFromServer (Ok (Err err)) ->
            -- error from Server.elm
            ( { model | alerts = Protocol.Alert "Error" err :: model.alerts }, Cmd.none )

        OnMsgFromServer (Ok (Ok serverMsg)) ->
            updateFromServer serverMsg model

        SendMessage clientMsg ->
            -- ( model, websocketOut (Json.Encode.encode 0 (Protocol.encodeProtocolMsgFromClient clientMsg)) )
            ( model, sendToServer clientMsg )


updateFromServer : Protocol.MsgFromServer -> Model -> ( Model, Cmd Msg )
updateFromServer serverMsg model =
    case serverMsg of
        Protocol.ManyMsgFromServer msglist ->
            -- Handling a batched list of `MsgFromServer`
            let
                overModelAndCmd nextMsg ( currentModel, currentCmd ) =
                    updateFromServer nextMsg currentModel
                        |> Tuple.mapSecond (\nextCmd -> Cmd.batch [ nextCmd, currentCmd ])
            in
            List.foldl overModelAndCmd ( model, Cmd.none ) msglist

        Protocol.ClientServerVersionMismatch _ ->
            ( { model | alerts = Protocol.clientServerMismatchAlert :: model.alerts }, Cmd.none )

        Protocol.ShowAlert alert ->
            ( { model | alerts = alert :: model.alerts }, Cmd.none )

        Protocol.RedirectTo newPage ->
            ( model, Browser.Navigation.pushUrl model.navKey (Protocol.pagePath newPage) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


updateFromURL : Url.Url -> Model -> ( Model, Cmd Msg )
updateFromURL url model =
    case Url.Parser.parse Protocol.pageRouter url of
        Just page ->
            updateFromPage { model | page = page }

        Nothing ->
            updateFromPage
                { model
                    | page = Protocol.NotFoundPage
                    , alerts = Protocol.Alert "Oops! Page Not Found" (Url.toString url) :: model.alerts
                }


updateFromPage : Model -> ( Model, Cmd Msg )
updateFromPage model =
    case model.page of
        Protocol.NotFoundPage ->
            ( model, Cmd.none )

        Protocol.HomePage ->
            ( model, Cmd.none )
