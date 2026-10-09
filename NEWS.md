# fireSense_summary 1.0.1.9003

- reqdPkgs now lists `archive`, which the module calls with `::` but did not list. Version 1.0.1.9003.

# fireSense_summary 1.0.1.9002

- `loadOrder` now runs after `fireSense_burn` (the burn module was renamed from `fireSense`); with the old name the ordering was silently ignored.

# fireSense_summary 1.0.1.9001

- The parameter `studyAreaName` is renamed `.studyAreaName`, the name every other fireSense and Biomass module
  uses for it. A project passing `studyAreaName` to this module must rename it. If it is `NA` (the default), a
  hash of `rasterToMatch` is used, as other modules use a hash of `studyArea`.
- `years` (already `NA`) defaults to the simulation's start and end, so a project need not repeat them here.
- Requires SpaDES.core >= 3.2.1.9001, for `resolveSimYears()`.
# fireSense_summary (development version)

# fireSense_summary 1.1.0

This release fixes the multi-run summary, which stopped partway because of leftover debugging code, and the fire-size calculation when fire polygons are downloaded. The module now runs after the renamed `fireSense_burn` module; with the old name the order was silently ignored.

The module also has a documentation website, its manual is shown on the repository page, and the package list is complete.

## Multi mode

* Without `ignitionFirePoints`, `InitMulti()` now gets the NFDB points from `fireregimetools::fetch_nfdb_points()`. It used to download `.../fire_pnt/current_version/NFDB_point.zip`, which returns 404, so multi mode stopped in `InitMulti()` unless the points were supplied.
* `reqdPkgs` lists `fireregimetools` and has floors for the functions the module calls: SpaDES.core 3.2.1.9001 (`dirnamesFromSet()`, `resolveSimYears()`, `padYears()`) and fireSenseUtils 0.2.3.9001 (`simFiles`).
* The module is a child of the `fireSense` parent, so a project that selects modules by name pattern (e.g. "summar") has to add `fireSense_summary` itself; otherwise nothing runs.

# fireSense_summary 1.0.5

- `InitMulti()` no longer stops at a leftover `browser()` call. Dead code removed; the functions, metadata and Rmd are documented.
- `SIZE_HA` is built from `mod$firePolys`, so the downloaded-polygons branch works.
- An unknown event type now gives a warning.

# fireSense_summary 1.0.4

- reqdPkgs now lists `archive`, which the module calls with `::` but did not list. Version 1.0.4.

# fireSense_summary 1.0.3

- `loadOrder` now runs after `fireSense_burn` (the burn module was renamed from `fireSense`); with the old name the ordering was silently ignored.

# fireSense_summary 1.0.2

Documentation and CI only; no change to the module's behaviour.

## Documentation

- The module `.Rmd` now has a single level-1 heading, `# fireSense_summary Module`, with its sections at level 2. Every section had been a level-1 heading, which is fine when the file is knitted on its own but makes each one a separate top-level chapter in a project manual -- this module contributed seven chapters to the fireSense manual, including generic "Overview", "Usage" and "Parameters" entries (#7).
- `README.md` symlinks to `fireSense_summary.md`, as in the other fireSense modules, so the repository landing page shows the rendered module manual.

## Continuous integration

- A pkgdown site is published for this module (#6).
- The module's own rendered `.html` is no longer gitignored, so the `render-module-rmd` workflow can commit it; that step had been failing on every push to `development`.

# fireSense_summary 1.0.1

First release from `development` since `main` was last updated (2024-04-19). Full history: https://github.com/PredictiveEcology/fireSense_summary/compare/4e1ac80...v1.0.1

## Breaking changes

- Removed input `uploadTo` (character).
- Input `burnMap` is now `SpatRaster` (was `RasterLayer`).
- Input `rasterToMatch` is now `SpatRaster` (was `RasterLayer`).
- Removed parameters: `climateScenarios`, `studyAreaNames`, `upload`.

## New features

- New parameters: `climateScenario`, `mode`, `studyAreaName`.

## Dependencies

- No longer depends on `disk.frame`, `qs`.
- Now depends on `qs2`, `terra`, `tidyterra`.

## Testing

- testthat suite and CI (`testthat-module`), including a snapshot of the module's inputs, outputs and parameters in `tests/testthat/test-metadata.R`.
