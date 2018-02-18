port module Bot exposing
  ( Bot, Farm, bot
  , update, name
  , reply, listen)

{-| An Elm RiveScript library. This is just an Elm interface built on top of the [rivescript-js](https://github.com/aichaos/rivescript-js) public API.

# State

@docs Bot

@docs Farm

@docs bot

@docs update

@docs name

# Conversation

@docs reply

@docs listen
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


{-| Update `Bot` in `Farm`
-}
update : Bot -> Farm -> Farm
update bot farm =
  farm


{-| Query the name of your bot.
-}
name : Bot -> String
name = uid


uid : Bot -> String
uid (Bot { uid }) = uid


pid : Bot -> Maybe Process.Id
pid (Bot { pid }) = pid


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


port respond : (String -> a) -> Sub a


{-| Subscribe to replies from your bot. There's currently no facility to tell replies from one bot apart from the replies from another bot. One can, by definition of `Listen` relying on a single port, only receive replies from all bots or none.
-}
listen : ( (String, Bot) -> a ) -> Bot -> Sub a
listen msg bot =
  -- Split incoming message based on directions (see Dexter docs at http://docs.rundexter.com/writing/bot/directions/); spawn lightweight processes and batch subscriptions as appropriate. I want to support the <send>, <delay> and <noreply> directions. The <get>, <set> and <star> directions seem to be supported by RiveScript out of the box.
  Sub.map (\tuple -> msg <| tuple) (respond (\str -> ( str, bot ) ) )
