---
title: "fireSense_summary Manual"
author: "Alex Chubaty"
date: "18 September 2026"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
pkgdown:
  as_is: true
---



# fireSense_summary Module

## Overview

Summarizes the results of multiple `fireSense` simulations, across multiple study areas, climate scenarios, and replicates.

## Usage

Intended to be used for post-simulation processing of multiple LandR Biomass simulations, following a `LandR-fs` project structure and workflow described and templated in the [`SpaDES.project`](https://github.com/PredictiveEcology/SpaDES.project) package.

### Plotting and saving

Several figures are produced, as `.png` files, and summary rasters are written to disk.

### Uploading

Figures can optionally be uploaded to Google Drive.

## Parameters

Provide a summary of user-visible parameters.


|paramName       |paramClass |default      |min |max |paramDesc                                                                                                                                                             |
|:---------------|:----------|:------------|:---|:---|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|climateScenario |character  |NA           |NA  |NA  |name of CIMP6 climate scenarios including SSP, formatted as in `ClimateNA`, using underscores as separator. E.g., 'CanESM5_SSP370'.                                   |
|mode            |character  |single       |NA  |NA  |use 'single' to run part of a simulation; use 'multi' to run as part of postprocessing multiple runs.                                                                 |
|simOutputPath   |character  |/tmp/Rtm.... |NA  |NA  |Directory specifying the location of the simulation outputs.                                                                                                          |
|studyAreaName   |character  |NA           |NA  |NA  |name of study areas simulated.                                                                                                                                        |
|reps            |integer    |1, 2, 3,.... |1   |NA  |number of replicates/runs per study area and climate scenario. NOTE: `mclapply` is used internally, so you should set `options(mc.cores = nReps)` to run in parallel. |
|years           |integer    |2011, 2100   |NA  |NA  |Which two simulation years should be compared? Typically start and end years.                                                                                         |

## Events

Describe what happens for each event type.

### Plotting

Write what is plotted.

### Saving

Write what is saved.

## Data dependencies

### Input data

Description of the module inputs.


|objectName    |objectClass |desc                                                          |sourceURL |
|:-------------|:-----------|:-------------------------------------------------------------|:---------|
|burnMap       |SpatRaster  |Cumulative burn map. Required in single mode.                 |NA        |
|burnSummary   |data.table  |Fire summary table from `fireSense`. Required in single mode. |NA        |
|rasterToMatch |SpatRaster  |template raster used by the simulations for summary reporting |NA        |

### Output data

Description of the module outputs.


|objectName |objectClass |desc |
|:----------|:-----------|:----|
|NA         |NA          |NA   |

## Links to other modules

Originally developed for *post hoc* use with the `fireSense` suite of wildfire modules.
