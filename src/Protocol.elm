module Protocol exposing (..)

import Json.Encode
import Protocol.Foobar
import Url.Parser


{-| All messages that Client can send to Server
-}
type MsgFromClient
    = ManyMsgFromClient (List MsgFromClient)
    | MsgFromFoobar Protocol.Foobar.MsgFromClient


{-| All messages that Server can reply to Client
-}
type MsgFromServer
    = ManyMsgFromServer (List MsgFromServer)
    | ClientServerVersionMismatch Json.Encode.Value
    | ShowAlert Alert
    | RedirectTo Page
    | MsgToFoobar Protocol.Foobar.MsgFromServer


{-| Http headers will be parsed into a RequestContext
Failure to parse means error; keep an always successful scenario, e.g. Anonymous
-}
type RequestContext
    = Cookied String
    | Anonymous



--


type alias Alert =
    { title : String
    , body : String
    }


clientServerMismatchAlert : Alert
clientServerMismatchAlert =
    Alert "Oops! Page has expired" "Please reload this page on your browser"


type Page
    = NotFoundPage
    | HomePage
    | FoobarPage Protocol.Foobar.Page


pageRouter : Url.Parser.Parser (Page -> a) a
pageRouter =
    Url.Parser.oneOf
        [ Url.Parser.map HomePage Url.Parser.top
        , Url.Parser.map FoobarPage Protocol.Foobar.pageRouter
        ]


pagePath : Page -> String
pagePath page =
    case page of
        NotFoundPage ->
            "/"

        HomePage ->
            "/"

        FoobarPage subPage ->
            Protocol.Foobar.pagePath subPage
