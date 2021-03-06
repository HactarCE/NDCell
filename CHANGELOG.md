# Changelog

All notable changes to NDCell will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), except for minor stylistic changes to organize features and accomodate named versions. This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html) with respect to the Rust API for `ndcell_core`, the NDCA API for `ndcell_lang`, and the combined Lua/NDCA API for `ndcell_ui`, the main application.

## [Unreleased]

## [0.2.5] Blinker (2021-02-26)

### Fixed

- Crash on startup with an NVIDIA GPU

## [0.2.4] Blinker (2021-02-25)

### Optimized

- 3D rendering performance (5x-20x FPS)

## [0.2.3] Blinker (2021-02-22)

### Added

- Application icon for Windows, macOS, and Linux

### Changed

- Disabled console window on Windows
- Updated Linux build to `libffi7`

### Fixed

- Missing executable permission on macOS and Linux
- Selection rendering glitch ([#9][i9])
- Selection pasting not stopping simulation ([#10][i10])

[i9]: https://github.com/HactarCE/NDCell/issues/9
[i10]: https://github.com/HactarCE/NDCell/issues/10

## [0.2.2] Blinker (2020-02-21)

### Fixed

- Crash on placing grid plane with an NVIDIA GPU

## [0.2.1] Blinker (2020-02-21)

### Fixed

- Crash on startup with an NVIDIA GPU

## [0.2.0] Blinker (2020-02-21)

![Blinker](https://user-images.githubusercontent.com/6060305/108616710-f3316780-73dd-11eb-858f-1cda97cad993.png)

### Added

- **Simulation**
  - 3D rendering and simulation
  - Advance one generation (<kbd>Space</kbd>)
  - Advance one step (<kbd>Tab</kbd>)
  - Place grid plane on cell face (<kbd>E</kbd>)
- **Selection**
  - Added edge resize indicator
  - Cancel selection drag (<kbd>Esc</kbd>)
- **Navigation**
  - 3D orbit (right mouse drag)
  - 3D pan (<kbd>↑</kbd>/<kbd>←</kbd>/<kbd>↓</kbd>/<kbd>→</kbd>, <kbd>W</kbd>/<kbd>A</kbd>/<kbd>S</kbd>/<kbd>D</kbd>, or middle mouse drag)
  - 3D pan horizontally (middle mouse drag with <kbd>Shift</kbd>)
  - Zoom (right mouse drag with <kbd>Ctrl</kbd>)
- **GUI**
  - Load/save file

### Changed

- **Simulation**
  - Cells align better to pixel boundaries when zoomed out, appearing crisper
  - Optimized 2D rendering of empty areas
- **Selection**
  - Selection edge resizing now clamps to the opposite corner
- **GUI**
  - Disabled rounded window borders
  - Display "RUNNING" or "STEPPING" accordingly instead of "SIMULATING"
  - Relabeled "Trigger garbage collection" button to "Clear cache"
  - Replaced inaccurate maximum simulation speed with average simulation time.
- Tweaked colors

### Fixed

- Changing the step size while the simulation is running now takes effect immediately ([#6][i6])
- Selected cells no longer appear to be tiled infinitely
- Touchpad scrolling now zooms in/out at a reasonable pace
- Crash when pressing an exotic mouse button

[i6]: https://github.com/HactarCE/NDCell/issues/6

## [0.1.1] Block (2020-12-17)

### Fixed

- Crash when selecting cells

## [0.1.0] Block (2020-12-17)

![Block](https://user-images.githubusercontent.com/6060305/102452302-21727f80-4008-11eb-891d-6a2ac2bd410f.png)

### Added

- **Simulation**
  - Display and simulate unbounded 2D Conway's Game of Life using HashLife algorithm
  - Toggle simulation (<kbd>Enter</kbd>)
  - Double/halve step size (<kbd>+</kbd>/<kbd>-</kbd>)
  - Highlight with crosshairs on hovered cell
    - Blue for drawing
    - White for selecting
- **Navigation**
  - 2D pan (<kbd>↑</kbd>/<kbd>←</kbd>/<kbd>↓</kbd>/<kbd>→</kbd>, <kbd>W</kbd>/<kbd>A</kbd>/<kbd>S</kbd>/<kbd>D</kbd>, or right/middle mouse drag)
  - Zoom (scroll wheel or <kbd>Q</kbd>/<kbd>Z</kbd>)
  - Pan/zoom faster while holding <kbd>Shift</kbd>
  - Reset view (<kbd>Ctrl</kbd>+<kbd>M</kbd>)
  - Fit pattern in view (<kbd>Ctrl</kbd>+<kbd>F</kbd>)
- **Drawing**
  - Toggle cells (left mouse click/drag)
  - Select numbered cell state (<kbd>0</kbd>-<kbd>9</kbd>)
  - Cycle selected cell state (<kbd>[</kbd>/<kbd>]</kbd>)
  - Cancel drawing (<kbd>Esc</kbd>)
- **Selection**
  - Select rectangle (left mouse drag with <kbd>Ctrl</kbd>)
  - Select all (<kbd>Ctrl</kbd>+<kbd>A</kbd>)
  - Resize selection rectangle (left mouse drag with <kbd>Ctrl</kbd>+<kbd>Shift</kbd>)
  - Resize selection edge (left mouse drag from selection edge)
  - Resize selection to cursor (left mouse click/drag with <kbd>Ctrl</kbd>+<kbd>Shift</kbd>)
  - Move selected cells (left mouse drag from selection)
  - Duplicate selected cells (left mouse drag with <kbd>Ctrl</kbd> from selection)
  - Move selection (left mouse drag with <kbd>Shift</kbd> from selection)
  - Delete selected cells (<kbd>Delete</kbd>)
  - Deselect (<kbd>Esc</kbd>)
- **Copy/paste**
  - Cut/copy selection as RLE (<kbd>Ctrl</kbd>+<kbd>X</kbd>/<kbd>C</kbd>)
  - Cut/copy selection as Macrocell (<kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>X</kbd>/<kbd>C</kbd>)
  - Paste RLE or Macrocell (<kbd>Ctrl</kbd>+<kbd>V</kbd>)
- **Undo history**
  - Undo (<kbd>Ctrl</kbd>+<kbd>Z</kbd>)
  - Redo (<kbd>Ctrl</kbd>+<kbd>Y</kbd> or <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Z</kbd>)
  - Reset (<kbd>Ctrl</kbd>+<kbd>R</kbd>)
- **GUI**
  - Simulation breakpoint
  - Display framerate (with color)
  - Display estimated maximum simulation update rate (with color)
  - Display generation count
  - Display total population
  - Display view scale and position
  - Display cursor position
  - Display estimated HashLife node pool memory usage
  - Display selected cell state
