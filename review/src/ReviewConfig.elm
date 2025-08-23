module ReviewConfig exposing (config)

import ExtractCallGraph
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ ExtractCallGraph.rule ]
