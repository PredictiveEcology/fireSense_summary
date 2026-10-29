---
title: "fireSense_summary"
author: "Alex Chubaty"
date: "29 October 2026"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Saves and summarizes `fireSense` burn results. It has two modes (parameter `mode`):

- `"single"` (default): run inside a simulation, after `fireSense_burn`. At `end(sim)` it writes `burnMap_year<end>.tif` and `fireSense_burnSummary.csv` to `outputPath(sim)`.
- `"multi"`: run on its own after several replicates have finished. It finds each replicate's burn map and burn summary and makes figures with `fireSenseUtils::plotBurnSummary()`, `plotCumulativeBurns()` and `plotHistoricFires()`.

# Usage

Single mode needs no setup beyond adding the module to a simulation that includes `fireSense`.

Multi mode:


``` r
library(SpaDES.core)

mySim <- simInit(
  times = list(start = 2011, end = 2100),
  modules = list("fireSense_summary"),
  params = list(fireSense_summary = list(
    mode = "multi",
    simOutputPath = "outputs/myStudyArea", ## holds rep01/, rep02/, ...
    .studyAreaName = "myStudyArea",
    climateScenario = "CanESM5_SSP370",
    reps = 1L:10L
  )),
  objects = list(rasterToMatch = rasterToMatch),
  paths = list(modulePath = "..")
)
spades(mySim)
```

The replicate files are found either from `outputsDF` (the row-bound `outputs(sim)` of the replicates) or, without it, by searching `simOutputPath/rep*/`; in that case the module stops if a burn map or burn summary is missing.
Historical fires for the comparison figure come from `firePolys` and `ignitionFirePoints`, or are downloaded from the Canadian National Fire Database.

# Parameters


|paramName        |paramClass |default      |min |max |paramDesc                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |cacheKey |
|:----------------|:----------|:------------|:---|:---|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:--------|
|.neverCache      |character  |init, sa.... |NA  |NA  |Events that are never cached, whatever `.useCache` says (read by SpaDES.core, 3.2.1.9037 or later): every event reads the run's output files or writes files (maps, csv, figures), none of which is in the event's cache key, and a cache hit would write nothing.                                                                                                                                                                                                                                                                                                                                                  |TRUE     |
|climateScenario  |character  |NA           |NA  |NA  |Name of the CMIP6 climate scenario including SSP, formatted as in `ClimateNA`, e.g. 'CanESM5_SSP370'. Used in figure filenames (multi mode).                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |TRUE     |
|mode             |character  |single       |NA  |NA  |'single': run within a simulation, saving `burnMap` and `burnSummary` at `end(sim)`. 'multi': summarize the saved outputs of several replicates in figures.                                                                                                                                                                                                                                                                                                                                                                                                                                                         |TRUE     |
|simOutputPath    |character  |/tmp/Rtm.... |NA  |NA  |Directory holding the replicate output directories, and where figures are written (multi mode).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |TRUE     |
|.runName         |character  |NA           |NA  |NA  |Name of this run, e.g. '13.1_NRV_370_1'; names its metrics file (`fireSenseUtils::ELFmetricsFilename()`). `NA`: the name of `outputPath(sim)`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |TRUE     |
|.studyAreaName   |character  |NA           |NA  |NA  |Human-readable name for the study area; used in figure paths and filenames (multi mode). If `NA`, a hash of `rasterToMatch` will be used.                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |TRUE     |
|reps             |integer    |1, 2, 3,.... |1   |NA  |Replicate numbers to summarize (multi mode). Files are read with `mclapply`; set `options(mc.cores = )` to run in parallel.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |TRUE     |
|escapeSizeHa     |numeric    |50           |0   |NA  |Fire size (ha) counted as an escape in the hindcast (`nFires50`); as in the fit.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |TRUE     |
|fireCauses       |character  |             |NA  |NA  |Cause groups counted as the observed fire of the hindcast (`natural`, `unknown`, `human`, `other`; `fireSenseUtils::fireCauseCodes()`). `NULL` (default): NFDB L and N, NBAC Natural. Set it to the `fireCauses` of `fireSense_dataPrepFit` so the observed fire has the fit's causes.                                                                                                                                                                                                                                                                                                                              |TRUE     |
|hindcastInterval |numeric    |1            |0   |NA  |Hindcast: time between the simulated rep-years. A job for year Y runs times Y, Y + interval, ...; rep = round((time - hindcastYear) / interval) + 1. Set it to the predict modules' interval (e.g. 0.001).                                                                                                                                                                                                                                                                                                                                                                                                          |TRUE     |
|hindcastYear     |integer    |NA           |NA  |NA  |Hindcast validation: the observed year the simulated rep-years are compared with. `NA` (default): no hindcast. Single mode records each rep-year and, at `end(sim)`, writes `hindcast_reps.csv`, `hindcast_sizes.csv`, `hindcast_observed.csv`, `hindcast_observedSizes.csv`, `hindcast_observedPolys.gpkg`, `hindcast_observedPoints.gpkg`, `hindcast_burnMap.tif` and, per rep, `hindcast_burnID_rep<rep>.tif`, `hindcast_ignitionProb_rep<rep>.tif` and `hindcast_escapeProb_rep<rep>.tif`. Multi mode pools the replicate folders (parameter sets) in hindcast figures instead of the usual multi-mode figures. |TRUE     |
|years            |integer    |NA, NA       |NA  |NA  |Which two simulation years should be compared? Typically start and end years. Defaults to the simulation's own start and end times.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |TRUE     |

# Events

- `init`: in single mode, schedules `save_single` at `end(sim)`. In multi mode, does all the work: locates the files and makes the figures.
- `save_single`: writes the burn map and burn summary (single mode only).

## Plotting

Multi mode only. Figures are `.png` files under `simOutputPath`, registered in `outputs(sim)`: burn summaries across replicates, cumulative burns, and simulated versus historical ignitions, escapes and area burned.

## Saving

Single mode only; see `save_single`.

# Data dependencies

## Input data


|objectName                   |objectClass |desc                                                                                                                                                                                                                                                       |sourceURL |
|:----------------------------|:-----------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------|
|burnMap                      |SpatRaster  |Cumulative burn map from `fireSense_burn`. Required in single mode.                                                                                                                                                                                        |NA        |
|burnSummary                  |data.table  |Fire summary table from `fireSense_burn`. Required in single mode.                                                                                                                                                                                         |NA        |
|firePolys                    |list        |Optional; multi mode. List of annual historical fire polygons. If missing, the NFDB polygons are downloaded.                                                                                                                                               |NA        |
|ignitionFirePoints           |SpatVector  |Optional; multi mode. Historical fire ignition points. If missing, the NFDB points are downloaded.                                                                                                                                                         |NA        |
|ignitionsAndEscapes          |data.table  |Optional; hindcast (single mode). This year's ignitions (`pixelID`, `escaped`) from `fireSense_ignitionPredict`.                                                                                                                                           |NA        |
|fireSense_IgAndEscapeProbRas |SpatRaster  |Optional; hindcast (single mode). This year's layers `ignitionProb` and `escapeProb` from `fireSense_ignitionPredict`, written for every rep.                                                                                                              |NA        |
|rasterToMatchELF             |SpatRaster  |Optional; hindcast (single mode). The ELF core; the simulation and the observations are clipped to it.                                                                                                                                                     |NA        |
|flammableRTM                 |SpatRaster  |Optional; hindcast (single mode). The pixels the simulation can burn (from `fireSense_burn`); the observed area burned and fire sizes are counted on them.                                                                                                 |NA        |
|rstAnnualBurnID              |SpatRaster  |Optional; hindcast (single mode). This year's fire IDs from `fireSense_burn`.                                                                                                                                                                              |NA        |
|fireSense_SpreadParamSet     |integer     |Optional; hindcast (single mode). The row of the fitted parameter sets this year's spread prediction used, from `fireSense_spreadPredict`; written as the `paramSet` column of the hindcast tables (`NA` if absent or, with several ELFs, not one number). |NA        |
|outputsDF                    |data.table  |Optional; multi mode. `outputs(sim)` of all replicates, row-bound. Its `file` column locates the burn maps and summaries. If missing, `simOutputPath` is searched.                                                                                         |NA        |
|.ELFind                      |character   |Optional; single mode. The ELF of this run, recorded in its metrics row.                                                                                                                                                                                   |NA        |
|rasterToMatch                |SpatRaster  |Template raster of the simulations. Required in multi mode.                                                                                                                                                                                                |NA        |

## Output data

None; results are files on disk.


|objectName |objectClass |desc |
|:----------|:-----------|:----|
|NA         |NA          |NA   |

# Links to other modules

Uses `burnMap` and `burnSummary` from `fireSense_burn`.
