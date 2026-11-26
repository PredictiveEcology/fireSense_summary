# fireSense_summary (development version)

- Multi mode passes `escapeSizeHa` to `fireSenseUtils::plotHistoricFires()`, so the observed escapes in its figures are fires of at least the escape size (50 ha by default), as in the hindcast validation. Needs fireSenseUtils >= 0.2.3.9193.

- Multi mode takes a replicate's files only from that replicate's folder, so `rep1` no longer picks up `rep10`'s, and without `outputsDF` a replicate's number no longer matches any path containing it (e.g. `1` in `_year2013`). A replicate in `reps` with no outputs now stops, naming it; with `outputsDF` it used to take every replicate's files. Needs SpaDES.core >= 3.2.1.9077 (`dirnamesFromSet()` named by replicate id, PredictiveEcology/SpaDES.core#543).

- `mode = "ELFmap"` with `nrvComparisonPath` reads each ELF's count from NRV_summary's `nrvInside.csv` (columns `poly`, `group`, `nTests`, `nInside`, `pctInside`; one row per analysis group) instead of recomputing it from `lm_nrvComparison.csv` and `st_nrvComparison.csv`, so the lead-nowhere rule lives in NRV_summary only. `ELFmap` is coloured by all groups' tests; a second map, `ELFmap_stand`, on the same 0-100 scale, by the stand-level tests only (`pctInsideStand` in `ELFmap_insideNRV.csv`), and one map per stand-level metric, `ELFmap_st_<metric>`, each ELF coloured by whether the metric is below, inside or above its NRV range (from `st_nrvComparison.csv`, `tested` rows only; ELFs where it is not a test are unfilled). An ELF without the file is warned about and left out. Depends-on: PredictiveEcology/NRV_summary (nrvInside.csv by group PR).

- Multi mode no longer makes the cumulative burn map (`plotCumulativeBurns()`) or the per-year burn figure (`plotBurnSummary()`); `burnSummaries` makes both. `raster` and `rasterVis` are no longer in `reqdPkgs`.

- Multi mode's `simulated_*` figures compare with the observed fires of the study area only: undated fires (`YEAR` -9999) and fires outside `rasterToMatch` are dropped, for the NFDB polygons and the points.

- The test helper `runInitMulti()` calls `SpaDES.core::start()`: in the package rendition `start` was `stats::start`, which gave a two-row `current` and failed 12 `InitMulti` tests.

- `mode = "ELFmap"` also writes the national maps of annual burn probability, `NRVburnProbability.tif` (simulated) and `NRVburnProbabilityHistoric.tif` (observed), to `simOutputPath`: cloud-optimized GeoTIFFs in EPSG:3978 made by `fireSenseUtils::mosaicELFs()` from the reported ELFs' `meanAnnualCumulBurnMap.tif` and `meanAnnualCumulBurnMapHistoric.tif` (burnSummaries), blended across the overlaps of the ELFs' buffers. New parameter `burnProbabilityMap` (default `TRUE`); `FALSE` draws only the ELF map. Needs fireSenseUtils with `mosaicELFs()`.

- The metrics row records when the run's fits were written: `spreadFitWrittenAt` and `ignitionFitWrittenAt` (UTC; the oldest over the run's ELFs), from the ledger rows' `writtenAt` (`spreadFitPreRun`, and fireSense_ELFs' `ignitionFitWrittenAt`). `fireSenseUtils::readELFmetrics()` orders runs by them, since ledger file names are labels and need not carry a date.

- New parameter `.plots` (default `"screen"`), passed to `Plots()` for the hindcast figures (multi mode) and the ELF map (`mode = "ELFmap"`). It was used but not declared, so it was `NULL` (no plots) unless `.globals` set it. `NA` or `NULL` for none.

- The metrics row's `spreadFitFile` and `ignitionFitFile` now name the ledger files the run's fits were read from. With `"latest"` they named the file a new fit would be written to (`spreadFitFilenameFor(fireYears, fitFileSuffix)`), which with the default suffix is not where `"latest"` reads from. They come from the attributes `fireSenseUtils::latestSpreadFits()` and `latestIgnitionFits()` set (`spreadFitPreRun`, `fireSense_IgnitionFittedList`, now declared inputs), for the run's ELFs, joined by ", " when they differ; `NA` when unknown.

- Multi mode: the NFDB fire polygons the module reads itself (no `firePolys` supplied) are now clipped to `rasterToMatch`, as the ignition points already were (`fireregimetools::load_nfdb_polys()`); the observed area burned and escaped fires in `plotHistoricFires()` were Canada's. Fires whose NFDB `YEAR` is -9999 (unknown) are dropped; they were plotted as a year -9999. `purrr` is no longer needed; needs fireregimetools >= 0.1.0.9002.

- Multi mode without `outputsDF` (replicate folders found under `simOutputPath`) stopped with "`simFiles` is required": `InitMulti()` set `simFiles` only from `outputsDF`. It is now set from the files found either way.

- New mode `"ELFmap"` with parameter `reportedELFs`: draws only the map of Canada's ELFs, the reported ones filled green (`fireSenseUtils::plotELFs()` through `Plots()`), from the `ELFs` input. For a multi-ELF report, e.g. the NRV page, redrawn as ELFs are added.

- Mode `"ELFmap"` with the new parameter `nrvComparisonPath` (a folder with one subfolder per ELF holding NRV_summary's `lm_nrvComparison.csv` and `st_nrvComparison.csv`) colours each reported ELF red to green by the percentage of its NRV tests inside the natural range (`nrvInsideTable()`; `lead_` tree types with `current` and `hi` both 0 are not tests, as on the NRV page) and writes the table as `ELFmap_insideNRV.csv`. An ELF missing either table is warned about and not filled. `NA` (default): green as before. Needs fireSenseUtils with the `metricCol`, `metricRange` and `metricTitle` arguments of `plotELFs()` (Depends-on: PredictiveEcology/fireSenseUtils, plotELFs metricCol PR).

* A run over several ELFs (`.ELFind` of a `studyAreaLarge` names them all) writes one metrics row; it wrote one row per ELF, each with the run's whole metrics. The row's `ELF` is the run's `.studyAreaName`, or, when that is not set, all its ELFs (`"6.6.1, 6.6.2"`).

- `SIZE_HA` is the one fire-size column. The block that copied `ADJ_HA` or `POLY_HA` into `SIZE_HA` before `plotHistoricFires()` is gone: `fireSenseUtils::plotHistoricFires()` now reads `SIZE_HA` alone. The NFDB polygons the module downloads itself already carry `SIZE_HA`; supplied `firePolys` must too (as `fireregimetools` and `fireSenseUtils::getFirePolygons()` give). Needs `fireSenseUtils` >= 0.2.3.9145.

- New parameter `fireCauses` (default `fireSenseUtils::fireSenseFireCauses`, `c("natural", "unknown")`, as in `fireSense_dataPrepFit`), passed to `fireSenseUtils::hindcastObserved()`, so the observed ignitions, escapes and area burned have the causes the model was fitted to. `.inputObjects()` checks it against the other modules and errors if they differ (see below); `NULL` is no longer the default. Needs fireSenseUtils with `fireCauseCodes()` (Depends-on: PredictiveEcology/fireSenseUtils#193).

- The hindcast passes `flammableRTM` (new optional input, from `fireSense_burn`) to `fireSenseUtils::hindcastObserved()`, so the observed area burned and fire sizes are counted on the pixels the simulation can burn, as the simulated ones are. The polygon-area values are also written (`*Polygon` columns of `hindcast_observed.csv`). The hindcast stops if `flammableRTM` is missing. Needs fireSenseUtils#190.

- The hindcast tables `hindcast_reps.csv` and `hindcast_sizes.csv` have a `paramSet` column: the row of the fitted spread parameter sets each rep used, read from `sim$fireSense_SpreadParamSet` (from `fireSense_spreadPredict` with `paramSetByReplicate = TRUE`, or with `.rep`). `NA` when that object is absent, so older configurations run as before.

* Without `outputsDF`, `InitMulti()` takes the replicate folder names from the files found under `simOutputPath`, so `rep01` (as written by SpaDES.project experiments) is found as well as `rep1`. It used to expect `rep1` whatever was on disk. Needs SpaDES.core 3.2.1.9038 or later.
* New parameter `repMaps` (default `TRUE`: no change). `FALSE` makes `hindcastRecord()` skip the per-rep tifs (`hindcast_burnID_rep<rep>.tif`, `hindcast_ignitionProb_rep<rep>.tif`, `hindcast_escapeProb_rep<rep>.tif`) and the module no longer needs `fireSense_IgAndEscapeProbRas`, so a replay job without an ignition module can run it. The tables, observed files and burn summary are unchanged.
* `.inputObjects()` checks the escape definition with `fireSenseUtils::escapeDefinitionOf()` (PredictiveEcology/fireSenseUtils#222), not its own `paramCheckOtherMods()` loop. The hindcast's core raster is `rasterToMatchELF`, a core-only mask, which is `hindcastObserved()` and `hindcastSimulated()`'s default `coreIs = "mask"`. Needs fireSenseUtils with that PR.
* The hindcast counts only fires that start in the ELF core, each at its whole extent (core and buffer), in the observed year (`fireSenseUtils::hindcastObserved(coreStart = TRUE)`) and in every simulated rep-year (`hindcastSimulated(coreStart = TRUE)`); buffer-start fires are in neither. The hard-coded 50 ha is gone: `escapeSizeHa` (default `fireSenseUtils::fireSenseEscapeSizeHa`, now the size of the whole fire polygon) and the new `fireCauses` (default `fireSenseUtils::fireSenseFireCauses`) are checked against the other modules' (`paramCheckOtherMods`, error). New input `flammableRTM` (the core and buffer burnable pixels, from `fireSense_burn`), required when `hindcastYear` is set. Needs fireSenseUtils with core-start fires (PredictiveEcology/fireSenseUtils#221).
* Single mode writes one metrics row per run, `ELFmetrics_<.runName>.csv`, for the national map (`fireSenseUtils::readELFmetrics()`, `plotELFs(metrics =)`). It names the run, the ELF and the spread and ignition ledger files the run's fits came from, then fires and area burned per year, or for a hindcast the mean of its rep-years and the observed year. New parameter `.runName` and optional input `.ELFind`. Needs fireSenseUtils with `ELFmetricsFilename()`.
* The hindcast also writes what the ignition-location test needs: per rep, `hindcast_ignitionProb_rep<rep>.tif` and `hindcast_escapeProb_rep<rep>.tif` (the two layers of `fireSense_IgAndEscapeProbRas`, on the ignition-fit grid), and `hindcast_observedPoints.gpkg`, the year's natural NFDB ignition points inside the core (from `fireSenseUtils::hindcastObserved()$points`). `fireSense_IgAndEscapeProbRas` is a new input and is required when `hindcastYear` is set. Needs fireSenseUtils with `hindcastObserved()$points` (PredictiveEcology/fireSenseUtils, "hindcast observed points").
* `hindcast_reps.csv`: `year` is now the integer hindcast year (it held the rep time, e.g. 1991.001); the rep time moves to the new column `time`, and `rep` is unchanged. Readers that used `year` as the rep time should read `time`.

* The National Fire Database polygons are fetched and unzipped with `reproducible::prepInputs()` instead of `download.file()` and `archive::archive_extract()`, so the access is checked against `CHECKSUMS.txt` and appears in the download ledger (`urlLog(sim)`). The `archive` package is no longer needed.

- Fixed: All events (`init`, `save_single`, `hindcastRecord`, `hindcast`) are declared in the new `.neverCache` parameter, so they are never cached, even when `.useCache` (or `.globals`) is set. They read the run's output files and write maps, csv files and figures, which are not part of the event's cache key, and a cache hit writes nothing. Needs SpaDES.core >= 3.2.1.9037.
- Fixed: All events (`init`, `save_single`, `hindcastRecord`, `hindcast`) are declared in the new `.neverCache` parameter, so they are never cached, even when `.useCache` (or `.globals`) is set. They read the run's output files and write maps, csv files and figures, which are not part of the event's cache key, and a cache hit writes nothing. Needs SpaDES.core >= 3.2.1.9037.
* The National Fire Database polygons are fetched and unzipped with `reproducible::prepInputs()` instead of `download.file()` and `archive::archive_extract()`, so the access is checked against `CHECKSUMS.txt` and appears in the download ledger (`urlLog(sim)`). The `archive` package is no longer needed.

* The parameter `studyAreaName` is renamed `.studyAreaName`, the name every other fireSense and Biomass module uses. The old name is still accepted, with a warning, and used when `.studyAreaName` is `NA`. If both are `NA`, a hash of `rasterToMatch` is used, as other modules use a hash of `studyArea`. `reproducible` is added to `reqdPkgs`.

* New parameter `hindcastYear` (default `NA`: no change) for the hindcast validation. Single mode records each rep-year (ignitions, escapes, area burned and fires of at least `escapeSizeHa` in the ELF core, from `fireSenseUtils::hindcastSimulated()`) every `hindcastInterval` and, at `end(sim)`, writes `hindcast_reps.csv`, `hindcast_sizes.csv`, `hindcast_observed.csv` (`fireSenseUtils::hindcastObserved()`), `hindcast_observedSizes.csv`, `hindcast_observedPolys.gpkg`, `hindcast_burnMap.tif` and `hindcast_burnID_rep<rep>.tif`. Rep-years may be decimal years (`rep = round((time - hindcastYear) / hindcastInterval) + 1`); the files are not named by `time(sim)`. Multi mode pools the replicate folders (parameter sets) into three `Plots()` figures: `hindcast_distributions`, `hindcast_sizes`, `hindcast_map`. New inputs `ignitionsAndEscapes`, `rasterToMatchELF`, `rstAnnualBurnID`; new parameters `escapeSizeHa`, `hindcastInterval`. Needs fireSenseUtils > 0.2.3.9091.

- reqdPkgs now lists `archive`, which the module calls with `::` but did not list. Version 1.0.1.9003.

- `loadOrder` now runs after `fireSense_burn` (the burn module was renamed from `fireSense`); with the old name the ordering was silently ignored.

- The parameter `studyAreaName` is renamed `.studyAreaName`, the name every other fireSense and Biomass module
  uses for it. A project passing `studyAreaName` to this module must rename it. If it is `NA` (the default), a
  hash of `rasterToMatch` is used, as other modules use a hash of `studyArea`.
- `years` (already `NA`) defaults to the simulation's start and end, so a project need not repeat them here.
- Requires SpaDES.core >= 3.2.1.9001, for `resolveSimYears()`.

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
