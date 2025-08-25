module Log exposing
    ( error
    , info
    , log
    , toString
    , warn
    , withConsole
    )

{-| Simple logging helpers.
Swap this file with the no-op version for production builds.
-}

import Debug


{-| Generic logger. Passes through the value.
-}
log : String -> a -> a
log label value =
    Debug.log label value


{-| Info-level logger (namespaced).
-}
info : String -> a -> a
info label value =
    Debug.log ("INFO " ++ label) value


{-| Warn-level logger (namespaced).
-}
warn : String -> a -> a
warn label value =
    Debug.log ("WARN " ++ label) value


{-| Error-level logger (namespaced).
-}
error : String -> a -> a
error label value =
    Debug.log ("ERROR " ++ label) value


{-| Log a one-off message in an update pipeline without exposing your model.

    Usage:

        update msg model =
            ( model, Cmd.none )
                |> Log.withConsole ("@update " ++ msgToString msg)

-}
withConsole : String -> ( model, Cmd msg ) -> ( model, Cmd msg )
withConsole message ( model, cmd ) =
    let
        _ =
            Debug.log message ()
    in
    ( model, cmd )


{-| Stub stringifier to keep call sites consistent between dev/prod.
Elm 0.19 doesn’t have a general-purpose toString; keep it opaque.
-}
toString : a -> String
toString _ =
    "<opaque>"
