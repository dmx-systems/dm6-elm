# DM6 Elm

## Version History

**0.3.1** -- Dec 25, 2025

* Fix:
    * Search/Traversal result's "Fullscreen" buttons don't create cycle
* Improvement:
    * Disable Fullscreen button if box is displayed fullscreen already

**0.3** -- Dec 24, 2025

* Markdown
    * Topic content is rendered as Markdown
    * Text edit mode has individual toolbar
    * Insert images from local filesystem (file picker)
    * Images are stored (blobs) in browser's Indexed DB
* Router
    * URLs for addressing fullscreen maps
    * Browser back/forward buttons for history navigation
* UI controls/toolbars (replacing 0.2's prototypical controls)
    * Toolbars
        * Global tools
        * Map tools
        * Topic tools (including Box tools)
        * Text editor tools
    * Expand/collapse control
        * Switches display modes (for both, topics and boxes)
    * "Home" button goes to home map
* Search/Traversal results show "Fullscreen" button for boxes, to go to that box
    * Search result is revealed in selected box, if any (otherwise in fullscreen map)
    * Results are displayed along their icons
* Support for maps larger than window
    * The fullscreen map scrolls independently from app header
    * The map scroll values are persistent, restored when map is revisited
    * Create/reveal topic position respects map scroll values
    * Fix: associations (SVG) render properly on large maps
* Changes / Wording
    * Differentiate "Topic" and "Box" explicitly
        * Only boxes can be drop targets
        * Only boxes can be displayed 'fullscreen'
    * "Container" (for boxes) is obsolete
    * "Hide" is obsolete, now called "Remove"
    * "Map" describes the rendering of a box content (both, fullscreen or nested)
* Still runs as standalone HTML file, no web server needed

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
    * Modularization: feature modules with state and messages
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
    * `localStorage` persistence

Project begin -- Jun 11, 2025

---
Jörg Richter  
Dec 25, 2025
