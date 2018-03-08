module Rivescript.Extensions.Errors exposing
  ( deeprecursion, nomatch, noreply
  )

{-| Processors for all errors described in the [rivescript-js  docs](https://github.com/aichaos/rivescript-js/blob/master/docs/rivescript.md#custom-error-messages)

@docs deeprecursion

@docs nomatch

@docs noreply
-}


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


errorParser : String -> Parser ()
errorParser string =
  succeed ()
    |. head
    |. keyword string
    |. end


{-| Processor that catches `ERR: Deep Recursion Detected` in bot responses.
-}
deeprecursion : Processor
deeprecursion data =
  case run (errorParser "Deep Recursion Detected") data of
    Ok () ->
      Ok (Nothing, Nothing)
        |> Debug.log ("(rivescript.js) `ERR: Deep Recursion Detected`")
    Err error ->
      Err error


{-| Processor that catches `ERR: No Reply Matched` errors in bot responses.
-}
nomatch : Processor
nomatch data =
  case run (errorParser "No Reply Matched") data of
    Ok () ->
      Ok (Nothing, Nothing)
        |> Debug.log ("(rivescript.js) `ERR: No Reply Matched`")
    Err error ->
      Err error


{-| Processor that catches `ERR: No Reply Found` errors in bot responses.
-}
noreply : Processor
noreply data =
  case run (errorParser "No Reply Found") data of
    Ok () ->
      Ok (Nothing, Nothing)
        |> Debug.log ("(rivescript.js) `ERR: No Reply Found`")
    Err error ->
      Err error
