# Advguild Complete Port to Weir

Comprehensive plan for porting [advguild](file:///home/nathan/dev/advguild) (~13,700 lines of Common Lisp across 77 source files) to Weir. The `weir-rewrite` branch at `/home/nathan/dev/advguild` is the target.

---

## 1. Source Game Overview

### Architecture

Adventure guild management / autobattler with ECS architecture, OpenGL rendering, and four scenes:

- **City scene**: 60x40 tile grid, building placement, production economy with A* worker pathfinding
- **Combat scene**: ATB (active time battle) system with gambit-based AI, floating damage numbers
- **Dungeon scene**: Procedural corridor generation, party exploration, encounter triggers
- **World map scene**: 32x24 tile grid, quest selection, travel animation, party management
- **Rendering**: OpenGL 4.6 core profile, ortho 2D, textured sprites + bitmap font, shader-based (3 modes: textured, solid color, tinted)
- **Architecture**: ECS via `cl-fast-ecs`, 3-phase game loop (INPUT at frame rate, SIMULATION at fixed 60Hz, RENDER at frame rate)

### CL Dependencies

| Library | Purpose | Weir Replacement |
|---|---|---|
| `cl-opengl` | OpenGL 4.6 bindings | Extend `weir-opengl` C helper |
| `glfw` | Window + input (Shirakumo wrapper) | Already in `weir-opengl`, extend with mouse/scroll |
| `pngload` | PNG texture decoding | Embed `stb_image.h` in `weir-opengl` |
| `zpng` | PNG encoding (screenshots) | `gl_screenshot` BMP capture already exists |
| `cl-fast-ecs` | ECS macros (component storage, system iteration) | MutMap-per-component pattern (no separate library) |
| `bordeaux-threads` | Threading (REPL dev runner) | Not needed — Weir has native live reloading |

### CL Features and Translation

**Direct translations (no language work needed):**

| CL Feature | Weir Equivalent |
|---|---|
| `defstruct` | `defstruct` |
| Keyword enums (`:fighter`, `:city`) | ADTs with exhaustive `match` |
| `mapcar`/`find-if`/`remove-if-not` | `map`/`find`/`filter` |
| `format` strings | `str` function |
| `lambda` | `fn` |
| `case` dispatch | `match` (exhaustive) |
| `defparameter` globals | `defglobal` / `defglobal mut` |
| `setf` on struct fields | `set!` on `.field` of `mut` local |
| `dolist` / `dotimes` | `for-each` / `for` |
| `incf`/`decf` | `set! (x (+ x 1))` |
| Hash tables | `MutMap` |
| Lists as accumulators | `MutVec` |

**Translations requiring care but no new features:**

| CL Pattern | Weir Approach |
|---|---|
| CLOS dispatch (GLFW callbacks) | Callback registration via C FFI |
| `define-symbol-macro` for globals | `defglobal mut` with field access |
| `cl-fast-ecs` `defsystem` DSL | Manual MutMap iteration with `for-each` |
| Association lists (inventory) | `Map` type or `MutMap` |
| 2D arrays (`aref`) | `MutVec` with index math `(+ (* row width) col)` |
| `plist` sprites `(:x 0 :y 0 :texture "path")` | `defstruct Sprite` with fields |
| `handler-case` error handling | Explicit checks (no exceptions in Weir) |

**Not used (no concern):** `eval`, reader macros, `multiple-value-bind`, conditions/restarts, `apply` on symbols, runtime code generation.

### Language Additions Status

All recommended additions have been shipped:

- [x] Mutable struct field assignment (`set!` on `.field`)
- [x] Imperative iteration (`for` / `for-each`)
- [x] Mutable collections (`MutVec` / `MutMap`)
- [x] Type-name-as-cast (`f64 x` instead of `i64->f64 x`)
- [x] Global mutable bindings (`defglobal mut`)

---

## 2. Library Work — weir-opengl Extension

The existing `weir-opengl` at `/home/nathan/dev/weir-opengl` provides:
- OpenGL 2.1 immediate-mode rendering
- `gl_init`, `gl_should_close`, `gl_begin_frame`, `gl_end_frame`
- `gl_draw_rect` (solid color quad), `gl_draw_text` (8x8 bitmap font)
- `gl_get_key`, `gl_get_key_pressed`
- `gl_time_ms`, `gl_screenshot` (BMP), `gl_cleanup`

Advguild requires OpenGL 3.3+ core profile with shaders, textures, alpha blending, mouse input, camera/viewport transforms, and sprite batching. The C helper needs a substantial upgrade.

### New C Functions (~400 lines added to `gl_helper.c`)

#### GL 3.3 Core Upgrade
The current OpenGL 2.1 immediate mode (`glBegin`/`glEnd`) must be replaced with a shader-based pipeline:

- **Shader program** with 3 modes matching the CL version:
  - Mode 0: Textured (sample texture at UV coords)
  - Mode 1: Solid color (uniform `uColor`)
  - Mode 2: Tinted texture (texture alpha * uniform color RGB — for font rendering)
- **VAO/VBO/EBO** for a single quad, updated per-sprite via `glBufferSubData`
- **Orthographic projection** via uniform `mat4 uProjection`
- Alpha blending enabled (`GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA`)

```c
// Upgrade gl_init to request OpenGL 3.3 core profile
// Add internal shader compilation, VAO/VBO/EBO setup
// Keep the existing API shape — callers don't change
```

#### Texture Loading (stb_image embedded)

| Function | Signature | Purpose |
|---|---|---|
| `gl_load_texture` | `(path: String) -> i64` | Load PNG via stb_image, return GL texture ID. Linear filtering, RGBA format. Returns 0 on failure. |
| `gl_bind_texture` | `(tex_id: i64) -> Unit` | Bind texture for subsequent draw calls |
| `gl_delete_texture` | `(tex_id: i64) -> Unit` | Delete a texture |

`stb_image.h` is embedded directly (single-header library, `#define STB_IMAGE_IMPLEMENTATION`). No external dependency.

#### Sprite Drawing

| Function | Signature | Purpose |
|---|---|---|
| `gl_draw_sprite` | `(x y w h: i64) -> Unit` | Draw bound texture at position, full UV (0,0)-(1,0) |
| `gl_draw_sprite_uv` | `(x y w h: i64, u0 v0 u1 v1: f64) -> Unit` | Draw with custom UV region (for sprite sheets, font atlas) |
| `gl_draw_sprite_tinted` | `(x y w h color: i64) -> Unit` | Draw bound texture with color tint (mode 2, for font) |
| `gl_draw_rect_alpha` | `(x y w h color alpha: i64) -> Unit` | Solid color rectangle with alpha (alpha 0-255). For overlays, health bars. |

Note: `gl_draw_rect` is kept for backwards compatibility (Tetris). The new functions add texture and alpha support.

#### Camera and Viewport

| Function | Signature | Purpose |
|---|---|---|
| `gl_set_camera` | `(x y: f64) -> Unit` | Offset all subsequent draws by (-x, -y). For city scrolling. |
| `gl_reset_camera` | `() -> Unit` | Reset camera to (0, 0). Called before HUD rendering. |
| `gl_set_viewport` | `(x y w h: i64) -> Unit` | Set GL viewport (for split rendering if needed) |
| `gl_reset_viewport` | `() -> Unit` | Reset to full window viewport |

Camera is implemented by adjusting the projection matrix uniform. `set_camera` recalculates `uProjection` with the offset; `reset_camera` restores the default.

#### Mouse Input

| Function | Signature | Purpose |
|---|---|---|
| `gl_mouse_x` | `() -> f64` | Current mouse X in window coords |
| `gl_mouse_y` | `() -> f64` | Current mouse Y (top-left origin, matching Weir's coord system) |
| `gl_mouse_button` | `(button: i64) -> i64` | Button held: 0=left, 1=right, 2=middle. Returns 1/0. |
| `gl_mouse_button_pressed` | `(button: i64) -> i64` | Button just pressed this frame. Edge-detected like `gl_get_key_pressed`. |
| `gl_mouse_scroll_y` | `() -> f64` | Scroll delta this frame (positive = up). Cleared each `gl_begin_frame`. |

Mouse state is tracked via GLFW callbacks registered in `gl_init`:
- `glfwSetCursorPosCallback` — updates position
- `glfwSetMouseButtonCallback` — updates button state with edge detection
- `glfwSetScrollCallback` — accumulates scroll delta

#### Updated `lib.weir`

```lisp
;; weir-opengl — GLFW + OpenGL 3.3 bindings for Weir
;;
;; Provides a 2D rendering API via a native C helper.
;; Colors are packed 0xRRGGBB. Coordinates use top-left origin.

(extern "C"
  ;; Window lifecycle
  (defn gl_init ((w : i64) (h : i64) (title : String)) : i64)
  (defn gl_should_close () : i64)
  (defn gl_begin_frame () : Unit)
  (defn gl_end_frame () : Unit)
  (defn gl_cleanup () : Unit)

  ;; Drawing — solid color
  (defn gl_draw_rect ((x : i64) (y : i64) (w : i64) (h : i64) (color : i64)) : Unit)
  (defn gl_draw_rect_alpha ((x : i64) (y : i64) (w : i64) (h : i64) (color : i64) (alpha : i64)) : Unit)

  ;; Drawing — textured
  (defn gl_load_texture ((path : String)) : i64)
  (defn gl_bind_texture ((tex_id : i64)) : Unit)
  (defn gl_delete_texture ((tex_id : i64)) : Unit)
  (defn gl_draw_sprite ((x : i64) (y : i64) (w : i64) (h : i64)) : Unit)
  (defn gl_draw_sprite_uv ((x : i64) (y : i64) (w : i64) (h : i64)
                           (u0 : f64) (v0 : f64) (u1 : f64) (v1 : f64)) : Unit)
  (defn gl_draw_sprite_tinted ((x : i64) (y : i64) (w : i64) (h : i64) (color : i64)) : Unit)

  ;; Drawing — text (8x8 bitmap font, kept from Tetris)
  (defn gl_draw_text ((x : i64) (y : i64) (text : String) (color : i64)) : Unit)

  ;; Camera / viewport
  (defn gl_set_camera ((x : f64) (y : f64)) : Unit)
  (defn gl_reset_camera () : Unit)
  (defn gl_set_viewport ((x : i64) (y : i64) (w : i64) (h : i64)) : Unit)
  (defn gl_reset_viewport () : Unit)

  ;; Keyboard input
  (defn gl_get_key ((key : i64)) : i64)
  (defn gl_get_key_pressed ((key : i64)) : i64)

  ;; Mouse input
  (defn gl_mouse_x () : f64)
  (defn gl_mouse_y () : f64)
  (defn gl_mouse_button ((button : i64)) : i64)
  (defn gl_mouse_button_pressed ((button : i64)) : i64)
  (defn gl_mouse_scroll_y () : f64)

  ;; Utilities
  (defn gl_time_ms () : i64)
  (defn gl_screenshot ((path : String)) : i64))
```

---

## 3. Game Architecture

### File Structure (~20 .weir files consolidated from 77 CL files)

The 77 CL files are heavily fragmented (13 component files averaging 7 lines each, 11 system files, etc.). The Weir port consolidates by functional area:

```
advguild/
  weir.pkg                    # Package manifest
  src/
    main.weir                 # Entry point, game loop, scene switching (~200 lines)
    globals.weir              # Global mutable state, constants (~80 lines)
    input.weir                # Keyboard/mouse input wrappers (~100 lines)
    renderer.weir             # Sprite struct, texture cache, render-frame (~150 lines)
    font.weir                 # Bitmap font UV lookup, draw-text helper (~80 lines)

    # Data definitions
    types.weir                # All ADTs: Scene, BuildingType, ItemType, Class, etc. (~150 lines)
    adventurers.weir          # Adventurer struct, party init, equipment, goals (~350 lines)
    items.weir                # Item definitions, recipes, resources (~300 lines)
    buildings.weir            # Building types, production recipes, worker state (~400 lines)
    quests.weir               # Quest struct, dungeon config, world map data (~300 lines)
    combat-data.weir          # Enemy stats, gambit definitions (~200 lines)

    # ECS layer
    ecs.weir                  # Component MutMaps, entity creation/destruction (~300 lines)
    systems.weir              # All ECS systems (combat, animation, production, movement) (~500 lines)

    # Scenes
    city.weir                 # City scene: grid, camera, buildings, terrain, pathfinding, HUD (~1500 lines)
    dungeon.weir              # Dungeon scene: grid generation, exploration, encounters (~800 lines)
    combat.weir               # Combat scene: ATB, gambits, actions, floating text (~600 lines)
    world-map.weir            # World map scene: travel, quest management (~400 lines)

    # UI
    ui-layout.weir            # Box layout system: padding, splits, nesting (~400 lines)
    ui-components.weir        # Reusable widgets: labels, buttons, sliders (~600 lines)
    party-menu.weir           # Equipment screen, inventory, goals (~650 lines)
    quest-menu.weir           # Quest list, dungeon selection (~350 lines)
    gambit-popover.weir       # Gambit condition/action editor (~230 lines)

    # Procedural generation
    town-gen.weir             # Town layout, decorative buildings (~280 lines)
    dungeon-gen.weir          # Dungeon floor generation, rooms, corridors (~440 lines)
    pathfinding.weir          # A* pathfinding for city workers (~265 lines)

    # Testing support
    headless.weir             # Headless runner: same game loop, no GL calls (~175 lines)

  tests/
    testing-utils.weir        # Test harness, assertions, sprite helpers (~250 lines)
    test-adventurers.weir     # 5 tests: party, equipment, goals (~150 lines)
    test-combat-ecs.weir      # 1 test: ECS entity creation, components (~80 lines)
    test-combat-scenarios.weir # 7 tests: timers, damage, healing, victory (~200 lines)
    test-combat-gameloop.weir # 1 test: game loop combat execution (~60 lines)
    test-dungeon-combat.weir  # 1 test: dungeon-to-combat flow (~80 lines)
    test-headless-combat.weir # 1 test: headless runner execution (~60 lines)
    test-scene-rendering.weir # 9 tests: sprite counts, textures, visibility (~200 lines)
    # Visual tests
    test-visual-worldmap.weir      # 2 tests: travel + progress screenshots
    test-visual-dungeon.weir       # 2 tests: view + progress screenshots
    test-visual-party-menu.weir    # 1 test: equipment UI screenshot
    test-visual-quest-menu.weir    # 1 test: quest menu screenshot
    test-visual-placement.weir     # 2 tests: building placement screenshots
```

### Type Definitions Strategy

**ADTs for enums** (exhaustive `match` catches missing cases):

```lisp
(deftype Scene City WorldMap Dungeon Combat)
(deftype AdventurerClass Fighter Mage Healer Rogue)
(deftype BuildingType Smelter Blacksmith Armorer Tanner Alchemist HerbalistHut Depot)
(deftype ItemType IronSword SteelSword LeatherArmor ChainArmor IronHelm
                  Herbs HealthPotion ManaPotion SpeedElixir ShieldCharm)
(deftype TerrainType Grass Hills Trees)
(deftype TileType Wall Floor Entrance Stairs)
(deftype WorkerState Idle Working Walking)
(deftype WorkerTask Fetch Produce Deliver)
(deftype CombatResult Victory Defeat)
(deftype QuestState Preparing Traveling InDungeon Returning Complete)
(deftype Team Party Enemy)
```

**Structs for data** (mutable fields via `set!` on `.field`):

```lisp
(defstruct Adventurer
  (id : i64) (name : String) (class : AdventurerClass) (level : i64)
  (base-max-hp : i64) (base-attack : i64) (base-defense : i64) (base-speed : i64)
  (current-hp : i64)
  (head : (Option ItemType)) (body : (Option ItemType))
  (left-hand : (Option ItemType)) (right-hand : (Option ItemType))
  (accessory : (Option ItemType))
  (inventory : (Vector (Option (Pair ItemType i64))))
  (gambits : (Vector Gambit)))

(defstruct Sprite
  (x : i64) (y : i64) (width : i64) (height : i64)
  (texture : (Option String)) (color : (Option i64))
  (uv : (Option (Vector f64))) (flip-x : Bool))
```

### ECS Pattern (MutMap per component, no separate library)

The CL version uses `cl-fast-ecs` which provides `defcomponent`, `defsystem`, and `make-storage` macros. The Weir port uses plain `MutMap` collections — one per component type:

```lisp
;; Component storage — one MutMap per component type
(defglobal mut *positions* : (MutMap i64 Position) (mut-map))
(defglobal mut *stats* : (MutMap i64 Stats) (mut-map))
(defglobal mut *combat-states* : (MutMap i64 CombatState) (mut-map))
(defglobal mut *teams* : (MutMap i64 Team) (mut-map))
(defglobal mut *images* : (MutMap i64 Image) (mut-map))
(defglobal mut *gambits-store* : (MutMap i64 (Vector Gambit)) (mut-map))
(defglobal mut *health-bars* : (MutMap i64 HealthBar) (mut-map))
(defglobal mut *animations* : (MutMap i64 Animation) (mut-map))
(defglobal mut *buildings-store* : (MutMap i64 Building) (mut-map))
(defglobal mut *next-entity-id* : i64 0)

;; Entity creation
(defn create-entity () : i64
  (let ((id *next-entity-id*))
    (set! *next-entity-id* (+ *next-entity-id* 1))
    id))

;; System iteration — explicit, no DSL
(defn run-combat-timer-system ((dt : f64)) : Unit
  (for-each ((id cs) *combat-states*)
    (when (mut-map-has *stats* id)
      (let ((mut cs-val (mut-map-get *combat-states* id)))
        (set! (.action-timer cs-val)
              (+ (.action-timer cs-val) (* dt (f64 (.speed (mut-map-get *stats* id))))))
        (mut-map-set! *combat-states* id cs-val)))))
```

This is simpler than a full ECS library and sufficient for the game's ~13 component types and ~15 systems.

### Rendering Strategy (sprite list + flush)

Matches the CL version: each frame, systems and scene code push `Sprite` structs to a mutable list. After all pushes, `render-frame` iterates the list and issues draw calls.

```lisp
(defglobal mut *temp-sprites* : (MutVec Sprite) (mut-vec))

;; During render phase — systems push sprites
(defn push-sprite! ((s : Sprite)) : Unit
  (mut-push! *temp-sprites* s))

;; End of frame — flush all sprites to GL
(defn render-frame () : Unit
  (for-each (sprite *temp-sprites*)
    (match (.texture sprite)
      ((Some tex) (render-textured-sprite sprite tex))
      (None (render-colored-sprite sprite))))
  (set! *temp-sprites* (mut-vec)))  ;; clear for next frame
```

### Game Loop Structure (INPUT -> fixed 60Hz SIM -> RENDER)

```lisp
(defn game-loop ((last-time : f64) (accumulator : f64)) : Unit
  (if (= (unsafe (gl_should_close)) 1)
    (unsafe (gl_cleanup))
    (let ((now (f64 (unsafe (gl_time_ms))))
          (frame-dt (- now last-time))
          (mut acc accumulator))

      ;; PHASE 1: INPUT (once per frame, never miss clicks on 120Hz+)
      (unsafe (gl_begin_frame))
      (clear-frame-input)
      (handle-scene-switch)
      (process-scene-input frame-dt)
      (process-menu-input)

      ;; PHASE 2: SIMULATION (fixed 60Hz timestep)
      (set! acc (+ acc frame-dt))
      (let ((logic-dt (/ 1000.0 60.0)))  ;; ~16.67ms
        (for (_ 0 (>= acc logic-dt))
          (run-simulation-systems logic-dt)
          (run-scene-simulation logic-dt)
          (set! acc (- acc logic-dt))))

      ;; PHASE 3: RENDER (once per frame)
      (set! *temp-sprites* (mut-vec))
      (render-scene-background)
      (run-render-systems)
      (render-scene-hud)
      (render-menus)
      (render-frame)
      (unsafe (gl_end_frame))

      (game-loop now acc))))
```

---

## 4. Implementation Phases

### Phase 0: weir-opengl Extension

**Goal:** Upgrade `weir-opengl` from OpenGL 2.1 immediate mode to 3.3 core with textures, mouse input, and camera.

**Work items:**
- [x] Download `stb_image.h` into weir-opengl directory
- [x] Upgrade `gl_init` to request OpenGL 3.3 core profile
- [x] Add internal shader compilation (vertex + fragment, 3 render modes)
- [x] Add VAO/VBO/EBO setup for single-quad rendering
- [x] Replace `glBegin`/`glEnd` in `gl_draw_rect` and `gl_draw_text` with shader-based draws
- [x] Add `gl_load_texture` (stb_image), `gl_bind_texture`, `gl_delete_texture`
- [x] Add `gl_draw_sprite`, `gl_draw_sprite_uv`, `gl_draw_sprite_tinted`
- [x] Add `gl_draw_rect_alpha`
- [x] Add GLFW mouse callbacks, `gl_mouse_x/y`, `gl_mouse_button`, `gl_mouse_button_pressed`, `gl_mouse_scroll_y`
- [x] Add `gl_set_camera`, `gl_reset_camera`, `gl_set_viewport`, `gl_reset_viewport`
- [x] Update `lib.weir` with all new extern declarations
- [x] Verify Tetris demo still works (backwards compatibility)
- [ ] Write a minimal texture test (load PNG, draw sprite, screenshot, compare)

**Verification:** Tetris demo runs unchanged. New test loads a texture and draws it at known position; screenshot matches expected output.

### Phase 1: Game Skeleton

**Goal:** Window opens, game loop runs, scene switching works, empty scenes render colored backgrounds.

**Work items:**
- [x] Create `advguild/weir.pkg` with weir-opengl dependency
- [x] `src/globals.weir` — scene state, viewport constants (2048x1536), game clock
- [x] `src/types.weir` — Scene ADT, all game enums
- [x] `src/input.weir` — keyboard/mouse wrappers around weir-opengl functions
- [x] `src/main.weir` — 3-phase game loop skeleton, `init-game-world`, scene dispatch
- [x] Scene switch mechanism: `defglobal mut *scene-switch-pending*`
- [x] Stub scene modules: `city.weir`, `dungeon.weir`, `combat.weir`, `world-map.weir` (each renders a solid color)
- [x] Key binding: 1/2/3/4 to switch scenes, ESC to quit

**Verification:** Window opens at 2048x1536. Pressing 1-4 switches between differently colored screens. ESC exits cleanly. 60fps confirmed via `gl_time_ms` delta logging.

### Phase 2: Rendering Foundation

**Goal:** Textures load and display, bitmap font renders text, UI layout system works.

**Work items:**
- [ ] `src/renderer.weir` — Sprite struct, `push-sprite!`, `render-frame` (iterates sprites, dispatches by type)
- [ ] Texture cache: `MutMap String i64` mapping paths to GL texture IDs
- [ ] `src/font.weir` — bitmap font rendering using the existing 8x8 font in gl_helper.c via `gl_draw_text`, or upgrade to font-atlas approach matching CL version
- [ ] `src/ui-layout.weir` — port `layout.lisp` box model: `Box` struct with x/y/w/h, `pad`, `split-h`, `split-v`
- [ ] `src/ui-components.weir` — port `components.lisp`: labels, buttons (with hover/click detection), panels, scrollable lists
- [ ] Port placeholder color textures (magenta missing-texture, category colors)

**Verification:** Can load a PNG texture and render it on screen. Text renders correctly at multiple sizes. UI layout produces correct box coordinates (unit-testable without GL).

### Phase 3: Data Layer

**Goal:** All game data types defined, static data tables populated, adventurer system complete.

**Work items:**
- [ ] `src/items.weir` — 10 item types with stat bonuses, recipes, resource types
- [ ] `src/buildings.weir` — 7 building types, production recipes, worker state struct
- [ ] `src/adventurers.weir` — Adventurer struct, party initialization (4 members), equipment system, inventory, goals
- [ ] `src/combat-data.weir` — Enemy stat tables, gambit condition/action definitions
- [ ] `src/quests.weir` — Quest struct, dungeon configuration, loot tables
- [ ] Port `test-adventurers.weir` (5 tests, 15 assertions)

**Verification:** All 5 adventurer tests pass. Can construct adventurers, equip items, compute stat bonuses, check goals.

### Phase 4: ECS + Entity Management

**Goal:** ECS component storage works, entities can be created/destroyed, combat entities spawn correctly.

**Work items:**
- [ ] `src/ecs.weir` — MutMap-per-component storage, `create-entity`, `destroy-entity` (removes from all maps)
- [ ] Component structs: Position, Stats, CombatState, Team, Image, Animation, HealthBar, Gambits, CombatantInfo, Building, Tilemap, Placeholder, CircularMotion
- [ ] Entity factory functions: `create-combat-entity`, `create-building-entity`, `create-tilemap-entity`
- [ ] `src/systems.weir` — combat timer, animation update, circular motion
- [ ] Port `test-combat-ecs.weir` (1 test, 9 assertions: entity creation, component presence)

**Verification:** `test-combat-ecs` passes. Can create combat entities with all required components, query components by entity ID.

### Phase 5: World Map Scene

**Goal:** World map renders terrain, shows town/dungeon markers, party travels between locations, quest system works.

**Work items:**
- [ ] World map terrain data (32x24 grid, terrain types with colors)
- [ ] Tilemap rendering system (iterate grid, push colored sprites per tile)
- [ ] Town and dungeon markers (static/dynamic positions)
- [ ] Travel animation (smooth lerp between positions)
- [ ] Quest creation, state machine (Preparing -> Traveling -> InDungeon -> Returning -> Complete)
- [ ] World map input (click to travel, quest selection)
- [ ] `src/quest-menu.weir` — quest list UI, dungeon selection
- [ ] Port world map rendering tests from `test-scene-rendering.weir`

**Verification:** World map renders 32x24 colored tiles. Town marker visible. Can create quest, see party travel. Quest menu opens/closes. Sprite count assertions pass.

### Phase 6: Combat System

**Goal:** Full ATB combat works: entities spawn, timers tick, gambits evaluate, damage resolves, combat ends in victory/defeat.

**Work items:**
- [ ] Combat entity spawning from adventurer data + enemy tables
- [ ] ATB timer system: action timer increments by speed * dt, triggers at 1.0
- [ ] Gambit evaluation: condition check -> action selection -> execution
- [ ] Damage calculation: base attack vs defense with variance
- [ ] Healing, item consumption
- [ ] Floating damage numbers with pop animation
- [ ] Combat end detection (all party dead = defeat, all enemies dead = victory)
- [ ] Combat rendering: combatant placeholders, health bars, combat log text
- [ ] `src/gambit-popover.weir` — gambit editor UI
- [ ] Port `test-combat-scenarios.weir` (7 tests, 22 assertions)
- [ ] Port `test-combat-gameloop.weir` (1 test, 6 assertions)
- [ ] Port combat rendering tests from `test-scene-rendering.weir`

**Verification:** All 8 combat tests pass. Combat runs for 100 frames without crash. Timers increment, damage occurs, combat can end in victory/defeat. HP never goes negative.

### Phase 7: Dungeon Scene

**Goal:** Dungeon procedurally generates floors, party explores corridors, encounters trigger combat, floor progression works.

**Work items:**
- [ ] `src/dungeon-gen.weir` — floor generation: rooms, corridors, entrance, stairs, markers
- [ ] Dungeon rendering: grid tiles (wall/floor/entrance/stairs), party sprite, encounter markers
- [ ] Party movement: left-to-right corridor traversal with smooth animation
- [ ] Encounter system: position-triggered, links to combat scene
- [ ] Floor progression: stairs -> next floor, boss encounter on final floor
- [ ] Dungeon context struct (current quest, encounters, floor state, party position)
- [ ] Dungeon -> combat scene transition
- [ ] Port `test-dungeon-combat.weir` (1 test, 13 assertions)
- [ ] Port `test-headless-combat.weir` (1 test, 10 assertions) — requires headless runner
- [ ] `src/headless.weir` — headless game loop for testing
- [ ] Port dungeon rendering tests from `test-scene-rendering.weir`

**Verification:** All 3 dungeon/headless tests pass. Floor generates with valid entrance/stairs. Party moves through corridor. Encounter triggers combat scene transition. Can complete dungeon -> combat -> return flow.

### Phase 8: City Scene

The largest scene. Broken into sub-phases.

#### Phase 8a: Grid + Camera

**Work items:**
- [ ] City context struct (buildings map, terrain, camera, level, threat)
- [ ] 60x40 terrain grid with 3 terrain types
- [ ] Camera panning (mouse drag + edge scrolling)
- [ ] Zoom levels (5 levels, 0.5x - 2.0x)
- [ ] Terrain rendering (colored tiles per terrain type)

**Verification:** City shows 60x40 colored grid. Camera pans smoothly. Zoom works.

#### Phase 8b: Production Economy

**Work items:**
- [ ] Building placement (click grid position, validate, create entity)
- [ ] Worker state machine: idle -> fetch -> produce -> deliver
- [ ] `src/pathfinding.weir` — A* with cache for worker routes
- [ ] Production system: input/output storage, recipe processing
- [ ] Depot system (resource collection point)
- [ ] Resource stockpile tracking
- [ ] `src/systems.weir` — production system (worker behavior each tick)

**Verification:** Can place buildings. Workers animate between buildings. Resources accumulate in depot. Production chain runs (e.g., smelter produces iron bars).

#### Phase 8c: City Rendering

**Work items:**
- [ ] Building sprites (or placeholder colored rectangles)
- [ ] Worker sprites walking paths
- [ ] Decorative buildings (`src/town-gen.weir`)
- [ ] Building selection highlight

**Verification:** Buildings render at correct grid positions. Workers visible during production.

#### Phase 8d: City HUD

**Work items:**
- [ ] Building palette UI (select building type to place)
- [ ] Stockpile display (resource counts)
- [ ] Capacity meter, threat indicator
- [ ] Building info panel (production state, worker status)
- [ ] Port city rendering tests from `test-scene-rendering.weir` (3 tests: terrain + HUD, building visibility, scene transitions)

**Verification:** All city rendering tests pass. HUD shows correct resource counts. Building palette allows placement.

### Phase 9: Quest Integration + Polish

**Goal:** Full gameplay loop works end-to-end. Party menu, quest flow, all scenes connected.

**Work items:**
- [ ] `src/party-menu.weir` — equipment screen, inventory management, goal setting
- [ ] Quest flow: city -> world map -> travel -> dungeon -> combat -> return -> city
- [ ] Loot integration: combat rewards added to quest, distributed on return
- [ ] Threat system: dungeon runs increase threat, unlock new buildings
- [ ] Polish: pause, speed controls, game-over handling
- [ ] Port remaining `test-scene-rendering.weir` tests (sprite texture key validation, item sprite rendering)
- [ ] Port all 8 visual/screenshot tests

**Verification:** Can play full loop: place buildings -> select quest -> travel -> explore dungeon -> fight -> return with loot. Party menu shows equipment correctly. All 22+ unit tests pass. All 8 screenshot tests produce comparable output to CL version.

---

## 5. Test Porting Strategy

### Test Infrastructure

**`tests/testing-utils.weir`** (port of `testing-utils.lisp`, ~250 lines):

Core test functions:
- `test-assert (condition : Bool) (message : String)` — pass/fail tracking
- `test-assert-equal (actual : a) (expected : a) (message : String)` — value comparison
- `test-assert-gte (actual : i64) (minimum : i64) (message : String)` — numeric comparison
- `test-reset ()` — clear state, seed RNG to 42
- `test-summary ()` — print results, exit with code 0/1

Sprite assertion helpers:
- `count-all-sprites ()` — total sprites in `*temp-sprites*`
- `count-sprites-with-texture (substring : String)` — filter by texture path
- `count-sprites-with-color ()` — count color-only sprites
- `sprite-in-region-p (x-min y-min x-max y-max : i64)` — spatial check
- `validate-sprite-texture-keys ()` — catch invalid sprite data

Scene setup helpers:
- `setup-fresh-game ()` — reset all state, create ECS storage, init party
- `switch-to-scene (scene : Scene)` — scene transition during tests
- `run-render-phase ()` — populate `*temp-sprites*` for assertions

**`src/headless.weir`** (port of `headless-runner.lisp`, ~175 lines):

Deterministic headless game loop that mirrors the real 3-phase loop without GL calls:
- `headless-init-world ()` — initialize game state
- `headless-run-frame (dt : f64)` — single frame (input -> sim -> render)
- `headless-run (max-frames : i64)` — run up to N frames
- `headless-run-until (pred : (-> Bool)) (max-frames : i64)` — stop when predicate true
- `headless-queue-input (frame : i64) (action : (-> Unit))` — deterministic input injection

### Unit Test Files (7 files, 22 test functions, 106 assertions)

| Test File | Tests | Assertions | Phase | What It Validates |
|---|---|---|---|---|
| `test-adventurers.weir` | 5 | 15 | 3 | Party init (4 members), equipment bonuses, equipment goals, inventory goals, manual goals |
| `test-combat-ecs.weir` | 1 | 9 | 4 | Entity creation, component presence (combat-state, stats, team), destruction |
| `test-combat-scenarios.weir` | 7 | 22 | 6 | Timer increment, damage, healing, messages, victory/defeat, required components, HP bounds |
| `test-combat-gameloop.weir` | 1 | 6 | 6 | 3-phase loop combat execution, encounter trigger, scene switch |
| `test-dungeon-combat.weir` | 1 | 13 | 7 | Dungeon init, quest, floor generation, combat trigger, entity creation, component presence |
| `test-headless-combat.weir` | 1 | 10 | 7 | Headless runner: party init, dungeon quest, encounter, combat, entity components, actions |
| `test-scene-rendering.weir` | 9 | 29 | 5-8 | Sprite counts per scene, texture keys, building visibility, entity visibility, scene transitions |

Note: `test-scene-rendering` tests are ported progressively — world map tests in Phase 5, combat in Phase 6, dungeon in Phase 7, city in Phase 8d.

### Visual / Screenshot Tests (8 tests)

| Test File | CL Original | Phase | Setup | Duration |
|---|---|---|---|---|
| `test-visual-worldmap.weir` | `test-world-map-travel.lisp` | 5 | World map, create quest, show party | 5 frames |
| `test-visual-worldmap.weir` | `test-world-map-travel-progress.lisp` | 5 | World map, create quest | 3 seconds |
| `test-visual-dungeon.weir` | `test-dungeon.lisp` | 7 | Dungeon floor 1, progress 0 | 5 frames |
| `test-visual-dungeon.weir` | `test-dungeon-progress.lisp` | 7 | Dungeon with encounters | 5 seconds |
| `test-visual-party-menu.weir` | `test-party-menu.lisp` | 9 | City, equip Ada, open party menu | 5 frames |
| `test-visual-quest-menu.weir` | `test-quest-menu.lisp` | 9 | City, open quest menu | 5 frames |
| `test-visual-placement.weir` | `test-full-placement.lisp` | 8 | City, place herbalist at (30,30) | 5 frames |
| `test-visual-placement.weir` | `test-placement-visual.lisp` | 8 | City, mouse at center, placement mode | 5 frames |

**Screenshot test approach:**
1. Initialize game state programmatically (same as CL test)
2. Run game loop for N frames or N seconds
3. Call `gl_screenshot` to capture BMP
4. Compare against reference screenshots from CL version (visual diff, not pixel-perfect)

### Test Execution Approach

- Each phase ports its relevant tests alongside the game code
- Tests use the same deterministic RNG seed (42) as the CL originals
- Headless runner reuses exact game loop code with GL calls skipped (render phase populates `*temp-sprites*` but doesn't draw)
- Screenshot tests run the real renderer and capture via `gl_screenshot`
- All 22 unit tests + 8 visual tests must pass before the port is considered complete

---

## 6. Potential Language Gaps

Monitor during port — don't pre-solve. If encountered, file as issues and use workarounds:

| Gap | Workaround | Likelihood |
|---|---|---|
| MutMap iteration (`for-each` over entries) | Already supported via `for-each` on MutMap | Confirmed needed |
| MutVec indexed set (`set-nth!`) | If missing, use `mut-set!` or rebuild | High — 2D grid access |
| Random numbers | `random-int` exists; may need `random-float` for combat variance | High |
| Float math builtins (`sqrt`, `floor`, `ceil`, `abs`, `min`, `max`) | FFI to C `math.h` if not built-in | High — pathfinding, combat |
| String operations (`substring`, `string-length`, `string-contains`) | FFI to C string functions if not built-in | Medium — UI text |
| Printf-style formatting | `str` concatenation covers most cases | Low |
| Option type (`Some`/`None`) | If no built-in, define as ADT | Medium — equipment slots |

---

## 7. Estimated Line Counts

| Component | Lines | Notes |
|---|---|---|
| weir-opengl C additions | ~400 | Shader, texture, mouse, camera |
| weir-opengl lib.weir update | ~50 | New extern declarations |
| Game source code | ~8,500 | 25 .weir files |
| UI code | ~1,900 | Layout, components, menus |
| Test infrastructure | ~250 | testing-utils.weir |
| Unit tests | ~600 | 7 test files, 22 functions |
| Visual tests | ~200 | 8 screenshot tests |
| **Total** | **~11,900** | Comparable to CL original (~13,700) |

The Weir version is more compact because:
- Consolidated file structure (25 files vs 77)
- No CL package boilerplate (565 lines in `packages.lisp`)
- No ECS library overhead (direct MutMap access vs `defcomponent`/`defsystem` macros)
- ADTs replace keyword enums + case dispatch
- `for`/`for-each` replace recursive iteration patterns

---

## 8. Verification Strategy

### Per-Phase Verification

Each phase has explicit verification criteria (see Phase descriptions above). A phase is complete when:
1. All listed work items are checked off
2. All tests assigned to that phase pass
3. The game runs without crashes through the implemented scenes

### Screenshot Comparison

- Reference screenshots captured from the CL version at known game states
- Weir screenshots captured at the same game states (same RNG seed, same frame count)
- Visual comparison (not pixel-perfect — rendering may differ slightly due to GL version / font rendering)
- Stored in `tests/screenshots/` for manual review

### Full Gameplay Loop Test

After Phase 9, verify the complete player journey:
1. Start in city, place buildings
2. Open quest menu, select dungeon
3. Travel on world map
4. Explore dungeon floors
5. Fight encounters (ATB combat)
6. Return to city with loot
7. Use loot to equip adventurers
8. Repeat with increased threat level

### Performance Target

- 60fps at 2048x1536 with full city scene (60x40 tiles + buildings + workers + HUD)
- No frame drops during combat (ATB timer accuracy depends on consistent timestep)
- Measure via `gl_time_ms` delta: frame time should be consistently under 16.7ms

### Final Checklist

- [ ] All 22 unit tests pass
- [ ] All 8 visual tests produce acceptable screenshots
- [ ] Full gameplay loop completable without crashes
- [ ] 60fps in all scenes
- [ ] No missing textures (magenta placeholder) in normal gameplay
- [ ] Headless runner can simulate 1000 frames without error
- [ ] `weir-opengl` Tetris demo still works (no regressions)
