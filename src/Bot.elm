port module Bot exposing
  ( Bot, Farm, bot
  , name
  , reply, listen)

{-| An Elm RiveScript library. This is just an Elm interface built on top of the [rivescript-js](https://github.com/aichaos/rivescript-js) public API.

# State

@docs Bot

@docs Farm

@docs bot

@docs name

# Conversation

@docs reply

@docs listen
-}


import Array exposing (Array)
import Process
import Task


{-| A record of type `Bot` represents a bot and encapsulsates the internal state of that bot. Use [`bot : String -> Bot`](#bot) to create a new bot.
-}
type Bot =
  Bot
    { uid   : String
    , pid   : Maybe Process.Id
    }


{-| A `Farm` is a collection of zero or more `Bot`. Bots come in farms, not in armies. I'm no believer of a bot apocalyse.
-}
type alias Farm = List Bot



{-| Create a new bot.
-}
bot : String -> Bot
bot name =
  Bot
    { uid = name
    , pid = Nothing
    }


{-| Query the name of your bot.
-}
name : Bot -> String
name = uid


uid : Bot -> String
uid (Bot { uid }) = uid


pid : Bot -> Maybe Process.Id
pid (Bot { pid }) = pid


update : Bot -> Farm -> Farm -> Farm
update bot acc farm =
  case List.head farm of
    Nothing ->
      bot :: acc
    Just i -> case uid i == uid bot of
      True -> (bot :: acc) ++ (Maybe.withDefault [] <| List.tail farm)
      False -> update bot (i :: acc) (Maybe.withDefault [] <| List.tail farm)


port request : List String -> Cmd a


{-| Request replies from your bot
-}
reply : String -> Bot -> ( Bot, Cmd a )
reply str (Bot bot) =
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
    Bot { bot | pid = Nothing } ! [ cmd, request [ bot.uid, str ] ]


port respond : (Array String -> a) -> Sub a


{-| Subscribe to replies from your bot.
-}
listen : ( Result String (String, Bot) -> a ) -> Sub a
listen msg =
  -- Split incoming message based on directions (see Dexter docs at http://docs.rundexter.com/writing/bot/directions/); spawn lightweight processes and batch subscriptions as appropriate. I want to support the <send>, <delay> and <noreply> directions. The <get>, <set> and <star> directions seem to be supported by RiveScript out of the box.
  Sub.map
    (\(name, reply) ->
      case (Maybe.map2 (\n r -> (r, bot n) ) name reply) of
        Nothing ->
          msg <| Err "Bad javascript input (bot name or reply)"
        Just tuple ->
          msg <| Ok tuple
    ) ( respond (\data -> ( Array.get 0 data, Array.get 1 data) ) )


-- `monitor` is a special case `listen`, which takes a farm as input. `monitor` calls into `update` (which puts *or* sets bots in a farm) when a bot with a given name replies. The library makes no guarantees about the existence of a bot or the consistency of the farm.
-- monitor : ( Result String (String, Bot, Farm)  -> a ) -> Farm -> Sub a
-- monitor msg farm =
