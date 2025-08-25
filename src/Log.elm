module Log exposing (log, toString)

{-| Flip-safe logging shim.

In dev: uses `Debug.*`.
In prod: build-prod.sh comments out the DEV block and uncomments the PROD block.

-}

-- import Debug
-- DEV START
--
--
-- log : String -> a -> a
-- log =
--     Debug.log
--
--
-- toString : a -> String
-- toString =
--     Debug.toString
--
--
--
-- DEV END
-- PROD START


log _ x =
    x


toString _ =
    "<value>"



-- PROD END
