module Log exposing (log)

-- Pure, pipe-friendly logger used by Utils.info/call/fail
-- Works under --optimize (no Debug import)


log : String -> a -> a
log =
    Debug.log



-- log _ x =
--     x
