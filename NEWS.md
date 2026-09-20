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
