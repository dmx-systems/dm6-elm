# DM6 Elm

## Version History

**0.2** -- *unreleased*

* Features:
    * Search
        * Preview result items on hover
        * Internal *pinned* state for topics/assocs
    * Hide topics/assocs
    * Import/Export JSON
* Fixes:
    * Drag topic in double revealed nested map
    * Draw assoc in double revealed nested map
    * Drop container on container
    * On drop set container display in *all* maps
    * Auto-size container on delete-topic
    * Toolbar radio buttons don't fire twice
* Code/Build:
    * Modularization: "components" with state and messages
    * `build-dev`/`build-prod` scripts (swaps logger)

**0.1** -- Aug 6, 2025

* Features:
    * Map display (DOM/SVG)
        * Create topics, draw assocs
        * Edit topics, plain text
        * Icon menu (for topic decoration)
        * Delete topics
    * Nested maps
        * Add topics to container
        * 2 Monad Display modes: *Label Only*, *Detail*
        * 3 Container Display modes: *Black Box*, *White Box*, *Unboxed*
        * Map auto-sizing
    * Fullscreen
        * For both, monads and containers
        * Back-navigation (multi-step)

Project begin -- Jun 11, 2025

---
Jörg Richter  
Sep 7, 2025
