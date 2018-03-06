module Rivescript.Extensions.Errors exposing
  ( deeprecursion, noreply )

import Process
import Task exposing (Task)


import Parser exposing
  ( Parser, run
  , (|.), succeed
  , Count(..), ignore, oneOrMore
  , keyword, end
  )


import Rivescript.Types exposing (Processor)


head : Parser ()
head =
  keyword "ERR: "


deeprecursionParser : Parser ()
deeprecursionParser =
  succeed ()
    |. head
    |. keyword "Deep Recursion Detected"
    |. end


deeprecursion : Processor
deeprecursion data =
  case run deeprecursionParser data of
    Ok () ->
      Ok (Nothing, Nothing)
        |> Debug.log ("(rivescript.js) `ERR: Deep Recursion Detected`")
    Err error ->
      Err error


noreplyParser : Parser ()
noreplyParser =
  succeed ()
    |. head
    |. keyword "No Reply Matched"
    |. end


noreply : Processor
noreply data =
  case run noreplyParser data of
    Ok () ->
      Ok (Nothing, Nothing)
        |> Debug.log ("(rivescript.js) `ERR: No Reply Matched`")
    Err error ->
      Err error
