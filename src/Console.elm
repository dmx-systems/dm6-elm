-- src/Console.elm
port module Console exposing (log)

port log : String -> Cmd msg
