module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
import Some.CallGraph.Rule as CallGraph


config : List Rule
config =
    [ CallGraph.rule
        -- give the extract a stable name; this is what shows under .extracts.<name>
        |> Review.Rule.withExtract "ExtractCallGraph"
        |> Review.Rule.toRule
    ]
