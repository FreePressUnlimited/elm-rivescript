module Examle exposing (main)

import Html
import Html.Attributes as Attrs
import Html.Events

import Bot

{-| PROGRAM
-}
main : Program Never Model Msg
main =
  Html.program
    { init = { history = [], draft = "", bot = Bot.bot "HAL" } ! [ Cmd.none ]
    , view = view
    , update = update
    , subscriptions = (\model -> Bot.listen Listen model.bot)
    }


-- MODEL

type alias Model =
  { history : List String
  , draft : String
  , bot : Bot.Bot
  }


-- UPDATE

type AltMsg
  = AltListen String Bot.Bot

type Msg
  = Listen ( String, Bot.Bot )
  | Update
  | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Listen ( reply, bot ) ->
      model ! [ Cmd.none ]
    Update ->
      model ! [ Cmd.none ]
    Submit ->
      model ! [ Cmd.none ]

-- VIEW

view : Model -> Html.Html Msg
view model =
  Html.div [ Attrs.class "container" ]
    [ Html.div [ Attrs.class "history" ]
      [
        Html.h1 [ ] [ Html.text "Hello, world!" ]
      ]
    , Html.div [ Attrs.class "controls" ]
      [
      ]
    ]
