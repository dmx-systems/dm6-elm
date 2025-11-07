# DM6 Elm

## Version History

**0.2** -- Nov 7, 2025

* Features:
    * Search / Traverse
        * Search topics by text input
        * Traverse alongside associations
        * In both cases: preview result as "limbo" items on hover
    * Hide topics/associations
    * Undo/Redo
    * Import/Export JSON
* Improvements:
    * Internal *pinned* state to support stability in conjunction with un/boxing
* Fixes:
    * Drag topic in double revealed nested map
    * Draw association in double revealed nested map
    * Drop container on container
    * Auto-size container on delete-topic
    * Toolbar radio buttons don't fire twice
* Code/Build:
    * Modularization: "components" with state and messages
    * `build-dev`/`build-prod` scripts (swap logger)

**0.1** -- Aug 6, 2025

* Features:
    * Map display (DOM/SVG)
        * Create topics, draw associations
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
    * localStorage persistence

Project begin -- Jun 11, 2025

---
Jörg Richter  
Nov 7, 2025
