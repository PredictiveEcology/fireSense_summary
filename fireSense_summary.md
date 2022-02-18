---
title: "fireSense_summary"
author: "Alex Chubaty"
date: "17 February 2022"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Summarizes the results of multiple `fireSense` simulations, across multiple study areas, climate scenarios, and replicates.

# Usage

Intended to be used for post-simulation processing of multiple LandR Biomass simulations, following a `LandR-fs` project structure and workflow described and templated in the [`SpaDES.project`](https://github.com/PredictiveEcology/SpaDES.project) package.

## Plotting and saving

Several figures are produced, as `.png` files, and summary rasters are written to disk.

## Uploading

Figures can optionally be uploaded to Google Drive.

# Parameters

Provide a summary of user-visible parameters.


|paramName        |paramClass |default      |min |max |paramDesc                                                                                                                          |
|:----------------|:----------|:------------|:---|:---|:----------------------------------------------------------------------------------------------------------------------------------|
|climateScenarios |character  |NA           |NA  |NA  |names of CIMP6 climate scenarios including SSP, formatted as in ClimateNA, using underscores as separator. E.g., 'CanESM5_SSP370'. |
|simOutputPath    |character  |outputPa.... |NA  |NA  |Directory specifying the location of the simulation outputs.                                                                       |
|studyAreaNames   |character  |NA           |NA  |NA  |names of study areas simulated.                                                                                                    |
|reps             |integer    |1, 2, 3,.... |1   |NA  |number of replicates/runs per study area and climate scenario.                                                                     |
|upload           |logical    |FALSE        |NA  |NA  |if TRUE, uses the `googledrive` package to upload figures.                                                                         |
|years            |integer    |2011, 2100   |NA  |NA  |Which two simulation years should be compared? Typically start and end years.                                                      |

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data

Description of the module inputs.


|objectName    |objectClass |desc                                                                                            |sourceURL |
|:-------------|:-----------|:-----------------------------------------------------------------------------------------------|:---------|
|rasterToMatch |RasterLayer |DESCRIPTION NEEDED                                                                              |NA        |
|uploadTo      |character   |if `upload = TRUE`, a named list of Google Drive folder ids, corresponding to `studyAreaNames`. |NA        |

## Output data

Description of the module outputs.


|objectName |objectClass |desc |
|:----------|:-----------|:----|
|NA         |NA          |NA   |

# Links to other modules

Originally developed for *post hoc* use with the `fireSense` suite of wildfire modules.
