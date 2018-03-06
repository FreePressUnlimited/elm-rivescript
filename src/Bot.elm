module Bot exposing
  ( Bot, Farm, bot
  , name
  , Request, To, reply
  , Response, With, listen)

{-| **An Elm RiveScript library** This library provides an Elm interface for RiveScript built on top of the [rivescript-js](https://github.com/aichaos/rivescript-js) public API.

  The library depends on [ports](https://guide.elm-lang.org/interop/javascript.html#ports) for interop with the rivescript-js library. See the [readme](http://package.elm-lang.org/packages/publeaks/elm-rivescript/latest#javascript-interop) for instructions on how to wire up these ports in your Elm app, and [elm-rivescript.js](https://github.com/Publeaks/elm-rivescript/blob/master/src/elm-rivescript.js) for a template and instructions on how to wire up these ports on the javascript side.

  Use [`bot name`](#bot) to create a named bot of type [`Bot`](#Bot). Receive replies from your bot by subscribing to [`listen msg`](#listen). You can subsequenty query the bot for a reply with [`reply message bot`](#reply).

  The library supports multiple bots. See the documentation of [`Farm`](#Farm) to learn more about managing conversation state when there's more than one bot in your bot farm.

# State

Create bots and manage the state of those bots.

@docs Bot

@docs Farm

@docs bot

@docs name

# Requesting replies

Query bots for replies.

@docs Request

@docs To

@docs reply

# Subscribing to bot responses

@docs Response

@docs With

@docs listen
-}


import Array exposing (Array)
import Process
import Regex exposing (..)
import Task exposing (Task)
import Time

import Rivescript exposing (Processor, apply)


{-| A record of type `Bot` encapsulsates the internal state of a bot. Use [`bot : String -> Bot`](#bot) to create a new bot.
-}
type Bot =
  Bot
    { uid   : String
    , pid   : Maybe Process.Id
    }


{-| A `Farm` is a collection of zero or more `Bot`. Bots come in farms, not in armies. I'm no believer of the bot apocalyse.

  The `Farm` type is provided for convenience. The library has no facilities for managing the state of a farm of bots. The [`List`](http://package.elm-lang.org/packages/elm-lang/core/latest/List) module provides all the functionality you need to manage a bot farm.

  Bots are identified by name; the `name` field is used as a unique identifier. **In order for a farm to remain internally consistent, you must avoid duplicate names.**

  Both [`reply`](#reply) and [`listen`](#listen) return a bot as part of the return value. This bot is always newly created internally with [`bot`](#bot). The name of this bot corresponds to the name of the bot that was queried ([`reply`](#reply)), or that submits a reply ([`listen`](#listen)). Use this bot to update (*insert-or-replace*) your bot farm.
-}
type alias Farm = List Bot


{-| Create a new named bot. See also [`name`](#name).

    bot "Marvin" == Bot
    name (bot "Marvin") == "Marvin"
-}
bot : String -> Bot
bot name =
  Bot
    { uid = name
    , pid = Nothing
    }


{-| Query the name of your bot.

    name (bot "Marvin") == "Marvin"
-}
name : Bot -> String
name = uid


uid : Bot -> String
uid (Bot { uid }) = uid


pid : Bot -> Maybe Process.Id
pid (Bot { pid }) = pid


{-| type alias Request
-}
type alias Request a = (Bot, Cmd a)


{-| type alias To
-}
type alias To a = List String -> Cmd a


{-| Request replies from your bot.

  You must update your application state to replace your bot with the bot returned to you. You must also pass the command returned to you to the Elm runtime for your query to be submitted to the RiveScript interpreter.

    reply "Hello, Bot!" to (bot "Marvin") == ( Bot, Cmd msg )

  Where `to` is an outgoing port of type:

    port to : List String -> Cmd msg
-}
reply : String -> To a -> Bot -> Request a
reply str port_ (Bot bot) =
  let
    -- Kill any running lightweight processes Bot bot.pid
    cmd = case bot.pid of
      Just n ->
        Process.kill n
          |> Task.perform identity
          |> always Cmd.none
      Nothing ->
        Cmd.none
  in
    Bot { bot | pid = Nothing } ! [ cmd, port_ [ bot.uid, str ] ]


{-| type alias Response
-}
type alias Response a =
  ({ reply : Maybe String, bot : Bot }, Cmd a)


{-| type alias With
-}
type alias With a
  -- Using List instead of Array will be more conventional while Array will likely be more performant.
  = ( Array.Array String -> a ) -> Sub a


{-| Subscribe to replies from your bot.

  Returns `Ok ( reply, bot )` when a reply arrives from the RiveScript interpreter. You must update your application state to replace your bot with the bot returned to you.

  Returns `Err "Bad javascript input (bot name or reply)"` if the RiveScript interpreter either returns no bot name or returns no reply. If elm-rivescript is wired up correctly on the javascript side this **should** never occur.

    listen with (\Result error (reply, bot, cmd) -> msg) == Sub msg

  Where `with` is an incoming port of type:

    port with : (Array.Array String -> msg) -> Sub msg
-}
listen
  : ( With ( Maybe String, Maybe String ) )
  ->  ( Result String (Response a) -> a )
  -> List Processor
  -> Sub a
listen with msg pipeline =
  let
    wrap = (\val -> msg (Result.map (uncurry (process msg pipeline)) val))
  in
    with (\data -> (Array.get 0 data, Array.get 1 data) )
      |> Sub.map (\val -> wrap (unpack val))


unpack
  : (Maybe String, Maybe String)
  -> Result String (String, String)
unpack data =
  case data of
    (Just name, Just reply) ->
      Ok (name, reply)
    (Just _, Nothing) ->
      Err "Bad javascript input (bot name)"
    (Nothing, Just _) ->
      Err "Bad javascript input (reply)"
    (Nothing, Nothing) ->
      Err "Bad javascript input (bot name and reply)"


process
  : ( Result String (Response a) -> a )
  -> List Processor
  -> String
  -> String
  -> Response a
process msg pipeline name string =
  let
    deferred = (\val -> msg (Ok (process msg pipeline name val)))
  in
    case apply pipeline string of
      (Just reply, Just task) ->
        { reply = (Just reply), bot = bot name } ! [ Task.perform deferred task ]
      (Just reply, Nothing) ->
        { reply = (Just reply), bot = bot name } ! [ Cmd.none ]
      (Nothing, Just task) ->
        { reply = Nothing, bot = bot name } ! [ Task.perform deferred task ]
      (Nothing, Nothing) ->
        { reply = Nothing, bot = bot name } ! [ Cmd.none ]
