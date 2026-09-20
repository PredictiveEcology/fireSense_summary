---
title: "fireSense_summary"
author: "Alex Chubaty"
date: "20 September 2026"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Saves and summarizes `fireSense` burn results. It has two modes (parameter `mode`):

- `"single"` (default): run inside a simulation, after `fireSense`. At `end(sim)` it writes `burnMap_year<end>.tif` and `fireSense_burnSummary.csv` to `outputPath(sim)`.
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
    studyAreaName = "myStudyArea",
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


|paramName       |paramClass |default      |min |max |paramDesc                                                                                                                                                   |
|:---------------|:----------|:------------|:---|:---|:-----------------------------------------------------------------------------------------------------------------------------------------------------------|
|climateScenario |character  |NA           |NA  |NA  |Name of the CMIP6 climate scenario including SSP, formatted as in `ClimateNA`, e.g. 'CanESM5_SSP370'. Used in figure filenames (multi mode).                |
|mode            |character  |single       |NA  |NA  |'single': run within a simulation, saving `burnMap` and `burnSummary` at `end(sim)`. 'multi': summarize the saved outputs of several replicates in figures. |
|simOutputPath   |character  |outputPa.... |NA  |NA  |Directory holding the replicate output directories, and where figures are written (multi mode).                                                             |
|studyAreaName   |character  |NA           |NA  |NA  |Study area name; used in figure paths and filenames (multi mode).                                                                                           |
|reps            |integer    |1, 2, 3,.... |1   |NA  |Replicate numbers to summarize (multi mode). Files are read with `mclapply`; set `options(mc.cores = )` to run in parallel.                                 |
|years           |integer    |NA, NA       |NA  |NA  |Which two simulation years should be compared? Typically start and end years. Defaults to the simulation's own start and end times.                         |

# Events

- `init`: in single mode, schedules `save_single` at `end(sim)`. In multi mode, does all the work: locates the files and makes the figures.
- `save_single`: writes the burn map and burn summary (single mode only).

## Plotting

Multi mode only. Figures are `.png` files under `simOutputPath`, registered in `outputs(sim)`: burn summaries across replicates, cumulative burns, and simulated versus historical ignitions, escapes and area burned.

## Saving

Single mode only; see `save_single`.

# Data dependencies

## Input data


|objectName         |objectClass |desc                                                                                                                                                               |sourceURL |
|:------------------|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------|
|burnMap            |SpatRaster  |Cumulative burn map from `fireSense`. Required in single mode.                                                                                                     |NA        |
|burnSummary        |data.table  |Fire summary table from `fireSense`. Required in single mode.                                                                                                      |NA        |
|firePolys          |list        |Optional; multi mode. List of annual historical fire polygons. If missing, the NFDB polygons are downloaded.                                                       |NA        |
|ignitionFirePoints |SpatVector  |Optional; multi mode. Historical fire ignition points. If missing, the NFDB points are downloaded.                                                                 |NA        |
|outputsDF          |data.table  |Optional; multi mode. `outputs(sim)` of all replicates, row-bound. Its `file` column locates the burn maps and summaries. If missing, `simOutputPath` is searched. |NA        |
|rasterToMatch      |SpatRaster  |Template raster of the simulations. Required in multi mode.                                                                                                        |NA        |

## Output data

None; results are files on disk.


|objectName |objectClass |desc |
|:----------|:-----------|:----|
|NA         |NA          |NA   |

# Links to other modules

Uses `burnMap` and `burnSummary` from `fireSense`.
