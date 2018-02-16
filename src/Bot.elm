port module Bot exposing
  ( Bot, bot
  , name
  , reply, receive)

{-| An Elm RiveScript library. This is just an Elm interface built on top of the [rivescript-js](https://github.com/aichaos/rivescript-js) public API.

# State

@docs Bot

@docs bot

@docs name

# Conversation

@docs reply

@docs receive
-}


import Process
import Task


{-| A record of type `Bot` represents a bot and encapsulsates the internal state of that bot. Use [`bot : String -> Bot`](#bot) to create a new bot.
-}
type Bot =
  Bot
    { uid   : String
    , pid   : Maybe Process.Id
    }


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


port request : String -> Cmd a


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
    Bot { bot | pid = Nothing } ! [cmd, request str]


port respond : (String -> a) -> Sub a


{-| Subscribe to replies from your bot
-}
receive : ( (String, Bot) -> a ) -> Bot -> Sub a
receive msg bot =
  -- Split incoming message based on directions (see Dexter docs at http://docs.rundexter.com/writing/bot/directions/); spawn lightweight processes and batch subscriptions as appropriate. I want to support the <send>, <delay> and <noreply> directions. The <get>, <set> and <star> directions seem to be supported by RiveScript out of the box.
  Sub.map (\tuple -> msg <| tuple) (respond (\str -> ( str, bot ) ) )
