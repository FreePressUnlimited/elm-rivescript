module Rivescript exposing
  ( Processor, Pipeline
  , apply
  , directions, errors
  , extensions
  )

{-| Rivescript provides functions that implement additional functionality on top of the rivescript-js RiveScript library. These functions are implemented as parser that may be run on Bot responses.

# Types

@docs Processor

@docs Pipeline

# Running pipelines

@docs apply

@docs extensions

# Utilities

@docs directions

@docs errors
-}


import Rivescript.Types
import Rivescript.Extensions.Directions as Directions
import Rivescript.Extensions.Errors as Errors


import Task exposing (Task)


{-| Processor

  Imported from Rivescript.Types and re-exported here for convenience.
-}
type alias Processor = Rivescript.Types.Processor


{-| Pipeline
-}
type alias Pipeline = List Processor


{-| apply
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


{-| extensions
-}
extensions : Pipeline
extensions =
  List.foldl List.append [] [ directions, errors ]


{-| directions
-}
directions : Pipeline
directions =
  [ Directions.delay, Directions.noreply, Directions.send ]


{-| errors
-}
errors : Pipeline
errors =
  [ Errors.deeprecursion, Errors.noreply ]
