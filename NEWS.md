# fireSense_summary (development version)

- The hindcast passes `flammableRTM` (new optional input, from `fireSense_burn`) to `fireSenseUtils::hindcastObserved()`, so the observed area burned and fire sizes are counted on the pixels the simulation can burn, as the simulated ones are. The polygon-area values are also written (`*Polygon` columns of `hindcast_observed.csv`). The hindcast stops if `flammableRTM` is missing. Needs fireSenseUtils#190.

- The hindcast tables `hindcast_reps.csv` and `hindcast_sizes.csv` have a `paramSet` column: the row of the fitted spread parameter sets each rep used, read from `sim$fireSense_SpreadParamSet` (from `fireSense_spreadPredict` with `paramSetByReplicate = TRUE`, or with `.rep`). `NA` when that object is absent, so older configurations run as before.

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

* Without `outputsDF`, `InitMulti()` takes the replicate folder names from the files found under `simOutputPath`, so `rep01` (as written by SpaDES.project experiments) is found as well as `rep1`. It used to expect `rep1` whatever was on disk. Needs SpaDES.core 3.2.1.9038 or later.
* Single mode writes one metrics row per run, `ELFmetrics_<.runName>.csv`, for the national map (`fireSenseUtils::readELFmetrics()`, `plotELFs(metrics =)`). It names the run, the ELF and the spread and ignition ledger files the run's fits came from, then fires and area burned per year, or for a hindcast the mean of its rep-years and the observed year. New parameter `.runName` and optional input `.ELFind`. Needs fireSenseUtils with `ELFmetricsFilename()`.
* The hindcast also writes what the ignition-location test needs: per rep, `hindcast_ignitionProb_rep<rep>.tif` and `hindcast_escapeProb_rep<rep>.tif` (the two layers of `fireSense_IgAndEscapeProbRas`, on the ignition-fit grid), and `hindcast_observedPoints.gpkg`, the year's natural NFDB ignition points inside the core (from `fireSenseUtils::hindcastObserved()$points`). `fireSense_IgAndEscapeProbRas` is a new input and is required when `hindcastYear` is set. Needs fireSenseUtils with `hindcastObserved()$points` (PredictiveEcology/fireSenseUtils, "hindcast observed points").
* `hindcast_reps.csv`: `year` is now the integer hindcast year (it held the rep time, e.g. 1991.001); the rep time moves to the new column `time`, and `rep` is unchanged. Readers that used `year` as the rep time should read `time`.

* The National Fire Database polygons are fetched and unzipped with `reproducible::prepInputs()` instead of `download.file()` and `archive::archive_extract()`, so the access is checked against `CHECKSUMS.txt` and appears in the download ledger (`urlLog(sim)`). The `archive` package is no longer needed.

- Fixed: All events (`init`, `save_single`, `hindcastRecord`, `hindcast`) are declared in the new `.neverCache` parameter, so they are never cached, even when `.useCache` (or `.globals`) is set. They read the run's output files and write maps, csv files and figures, which are not part of the event's cache key, and a cache hit writes nothing. Needs SpaDES.core >= 3.2.1.9037.
- Fixed: All events (`init`, `save_single`, `hindcastRecord`, `hindcast`) are declared in the new `.neverCache` parameter, so they are never cached, even when `.useCache` (or `.globals`) is set. They read the run's output files and write maps, csv files and figures, which are not part of the event's cache key, and a cache hit writes nothing. Needs SpaDES.core >= 3.2.1.9037.
* The National Fire Database polygons are fetched and unzipped with `reproducible::prepInputs()` instead of `download.file()` and `archive::archive_extract()`, so the access is checked against `CHECKSUMS.txt` and appears in the download ledger (`urlLog(sim)`). The `archive` package is no longer needed.

* The parameter `studyAreaName` is renamed `.studyAreaName`, the name every other fireSense and Biomass module uses. The old name is still accepted, with a warning, and used when `.studyAreaName` is `NA`. If both are `NA`, a hash of `rasterToMatch` is used, as other modules use a hash of `studyArea`. `reproducible` is added to `reqdPkgs`.

* New parameter `hindcastYear` (default `NA`: no change) for the hindcast validation. Single mode records each rep-year (ignitions, escapes, area burned and fires of at least `escapeSizeHa` in the ELF core, from `fireSenseUtils::hindcastSimulated()`) every `hindcastInterval` and, at `end(sim)`, writes `hindcast_reps.csv`, `hindcast_sizes.csv`, `hindcast_observed.csv` (`fireSenseUtils::hindcastObserved()`), `hindcast_observedSizes.csv`, `hindcast_observedPolys.gpkg`, `hindcast_burnMap.tif` and `hindcast_burnID_rep<rep>.tif`. Rep-years may be decimal years (`rep = round((time - hindcastYear) / hindcastInterval) + 1`); the files are not named by `time(sim)`. Multi mode pools the replicate folders (parameter sets) into three `Plots()` figures: `hindcast_distributions`, `hindcast_sizes`, `hindcast_map`. New inputs `ignitionsAndEscapes`, `rasterToMatchELF`, `rstAnnualBurnID`; new parameters `escapeSizeHa`, `hindcastInterval`. Needs fireSenseUtils > 0.2.3.9091.

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
