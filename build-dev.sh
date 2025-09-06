#!/bin/sh

set -e

js="main.js"
logger="DevLogger.elm"

cp src/Logger/$logger src/Logger.elm
elm make src/Main.elm --output=$js
