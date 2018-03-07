module Rivescript exposing
  ( apply
  , directions, errors
  , extensions
  )

{-| The Rivescript modules provide functions and supporting types that implement additional functionality on top of the [rivescript-js](https://github.com/aichaos/rivescript-js) RiveScript library. These functions are implemented as parser that may be run on Bot responses.

# Running processor pipelines

@docs apply

@docs extensions

# Utilities

@docs directions

@docs errors
-}


import Rivescript.Types exposing (Processor, Pipeline)
import Rivescript.Extensions.Directions as Directions
import Rivescript.Extensions.Errors as Errors


import Task exposing (Task)


{-| Apply a `Pipeline` to a `String` (typically a bot reponse). This function is best explained by looking at some example runs, documented under [directions](#directions) and [errors](#errors).
-}
apply
  : Pipeline
  -> String
  -> ( Maybe String, Maybe (Task Never String) )
apply pipeline string =
  case pipeline of
    [] ->
      (Just string, Nothing)
    proc :: rest ->
      Result.withDefault (apply rest string) (proc string)


{-| Convenience function that appends all utility pipelines into a single pipeline.
-}
extensions : Pipeline
extensions =
  List.foldl List.append [] [ directions, errors ]


{-| Utility pipeline that appends all [directions](Rivescript-Extensions-Directions) into a single pipeline.

    apply directions "Hello, World!" == ( Just "Hello, World!", Nothing )
    apply directions "Hello<send>World!" == ( Just "Hello", Just sendTask "World!" )
    apply directions "Hello<delay seconds=1.0>How do you do?" == ( Just "Hello", Just delayTask "How Do you do?" )
    apply directions "<noreply>" == ( Nothing, Nothing )
-}
directions : Pipeline
directions =
  [ Directions.delay, Directions.noreply, Directions.send ]


{-| Utility pipeline that appends all [errors](Rivescript-Extensions-Errors) into a single pipeline. The errors pipeline silenty supresses errors that occur in rivescript-js and logs these to the console rather than returning them as bot responses.

    apply errors "Hello, World!" == ( Just "Hello, World!", Nothing )
    apply errors "ERR: No Reply Matched" == ( Nothing, Nothing )
    apply errors "ERR: Deep Recursion Detected" == ( Nothing, Nothing )
-}
errors : Pipeline
errors =
  [ Errors.deeprecursion, Errors.nomatch, Errors.noreply ]
