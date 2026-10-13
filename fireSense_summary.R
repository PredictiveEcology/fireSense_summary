defineModule(sim, list(
  name = "fireSense_summary",
  description = paste("Summarizes the results of multiple fireSense simulations,",
                      "across multiple study areas, climate scenarios, and replicates."),
  keywords = "fireSense",
  authors = c(
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre")),
    person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = "aut"),
    person("Ian MS", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "aut")
  ),
  childModules = character(0),
  version = list(fireSense_summary = "1.1.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "fireSense_summary.Rmd"), ## same file
  loadOrder = list(after = c("fireSense_burn")),
  reqdPkgs = list(
    "archive", "assertthat", "cowplot", "data.table", "fs", "ggplot2", "googledrive",
    "FOR-CAST/fireregimetools@main (>= 0.1.0.9001)", ## fetch_nfdb_points()
    "purrr", "qs2", "RColorBrewer", "terra", "tidyterra",
    "raster", "rasterVis", ## TODO: remove these once fireSenseUtils::plotCumulativeBurns switched to ggplot2/tidyterra
    "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9092)", ## hindcast*() (after plot*(simFiles =))
    "PredictiveEcology/SpaDES.core@development (>= 3.2.1.9001)", ## dirnamesFromSet(), resolveSimYears(), padYears()
    "PredictiveEcology/SpaDES.tools@development (>= 2.1.1.9000)"
  ),
  parameters = rbind(
    defineParameter("climateScenario", "character", NA, NA, NA,
                    desc = paste("Name of the CMIP6 climate scenario including SSP, formatted as in `ClimateNA`,",
                                 "e.g. 'CanESM5_SSP370'. Used in figure filenames (multi mode).")),
    defineParameter("mode", "character", "single", NA, NA,
                    paste("'single': run within a simulation, saving `burnMap` and `burnSummary` at `end(sim)`.",
                          "'multi': summarize the saved outputs of several replicates in figures.")),
    defineParameter("simOutputPath", "character", outputPath(sim), NA, NA,
                    desc = "Directory holding the replicate output directories, and where figures are written (multi mode)."),
    defineParameter("studyAreaName", "character", NA, NA, NA,
                    desc = "Study area name; used in figure paths and filenames (multi mode)."),
    defineParameter("reps", "integer", 1L:10L, 1, NA,
                    desc = paste("Replicate numbers to summarize (multi mode). Files are read with `mclapply`;",
                                 "set `options(mc.cores = )` to run in parallel.")),
    defineParameter("escapeSizeHa", "numeric", 50, 0, NA,
                    desc = "Fire size (ha) counted as an escape in the hindcast (`nFires50`); as in the fit."),
    defineParameter("hindcastInterval", "numeric", 1, 0, NA,
                    desc = paste("Hindcast: time between the simulated rep-years. A job for year Y runs times",
                                 "Y, Y + interval, ...; rep = round((time - hindcastYear) / interval) + 1.",
                                 "Set it to the predict modules' interval (e.g. 0.001).")),
    defineParameter("hindcastYear", "integer", NA_integer_, NA, NA,
                    desc = paste("Hindcast validation: the observed year the simulated rep-years are compared with.",
                                 "`NA` (default): no hindcast. Single mode records each rep-year and, at `end(sim)`,",
                                 "writes `hindcast_reps.csv`, `hindcast_sizes.csv`, `hindcast_observed.csv`,",
                                 "`hindcast_observedSizes.csv`, `hindcast_observedPolys.gpkg`, `hindcast_burnMap.tif` and one",
                                 "`hindcast_burnID_rep<rep>.tif` per rep. Multi mode pools the replicate folders",
                                 "(parameter sets) in hindcast figures instead of the usual multi-mode figures.")),
    defineParameter("years", "integer", c(NA_integer_, NA_integer_), NA, NA,
                    desc = paste("Which two simulation years should be compared?",
                                 "Typically start and end years.",
                                 "Defaults to the simulation's own start and end times."))
  ),
  inputObjects = bindrows(
    expectsInput("burnMap", "SpatRaster",
                 desc = "Cumulative burn map from `fireSense_burn`. Required in single mode.",
                 sourceURL = NA),
    expectsInput("burnSummary", "data.table",
                 "Fire summary table from `fireSense_burn`. Required in single mode.",
                 sourceURL = NA),
    expectsInput("firePolys", "list", sourceURL = NA,
                 paste("Optional; multi mode. List of annual historical fire polygons.",
                       "If missing, the NFDB polygons are downloaded.")),
    expectsInput("ignitionFirePoints", "SpatVector", sourceURL = NA,
                 paste("Optional; multi mode. Historical fire ignition points.",
                       "If missing, the NFDB points are downloaded.")),
    expectsInput("ignitionsAndEscapes", "data.table", sourceURL = NA,
                 paste("Optional; hindcast (single mode). This year's ignitions (`pixelID`, `escaped`) from",
                       "`fireSense_ignitionPredict`.")),
    expectsInput("rasterToMatchELF", "SpatRaster", sourceURL = NA,
                 "Optional; hindcast (single mode). The ELF core; the simulation and the observations are clipped to it."),
    expectsInput("rstAnnualBurnID", "SpatRaster", sourceURL = NA,
                 "Optional; hindcast (single mode). This year's fire IDs from `fireSense_burn`."),
    expectsInput("outputsDF", "data.table",
                 desc = paste("Optional; multi mode. `outputs(sim)` of all replicates, row-bound. Its `file` column",
                              "locates the burn maps and summaries. If missing, `simOutputPath` is searched."),
                 sourceURL = NA),
    expectsInput("rasterToMatch", "SpatRaster",
                 "Template raster of the simulations. Required in multi mode.",
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

#' Event dispatcher
#'
#' `init`: in single mode, schedules `save_single` at `end(sim)` (and, if `hindcastYear` is set,
#' `hindcastRecord` every `hindcastInterval` and `hindcast` at `end(sim)`); in multi mode,
#' runs `InitMulti()` and makes the burn summary, cumulative burn and historic
#' fire figures. `save_single` writes `burnMap` (`.tif`) and `burnSummary` (`.csv`)
#' to `outputPath(sim)`.
#'
#' @param sim A `simList`.
#' @param eventTime Time of the event.
#' @param eventType `"init"`, `"save_single"`, `"hindcastRecord"` or `"hindcast"`.
#'
#' @return The `simList`, invisibly.
doEvent.fireSense_summary = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      if (P(sim)$mode == "single") {
        sim <- scheduleEvent(sim, end(sim), "fireSense_summary", "save_single", .last())
        if (!is.na(P(sim)$hindcastYear)) {
          sim <- scheduleEvent(sim, start(sim), "fireSense_summary", "hindcastRecord", .last())
          ## after the last record, which is at the same time
          sim <- scheduleEvent(sim, end(sim), "fireSense_summary", "hindcast", .last() + 1)
        }
      } else if (P(sim)$mode == "multi" && !is.na(P(sim)$hindcastYear)) {
        sim <- hindcastFigures(sim)
      } else if (P(sim)$mode == "multi") {
        sim <- InitMulti(sim)

        padYear <- paddedFloatToChar(time(sim), padL = ceiling(log10(end(sim) + 1)))

        f_burnSummary_plot <- fireSenseUtils::plotBurnSummary(
          climateScenario = P(sim)$climateScenario,
          studyAreaName = P(sim)$studyAreaName,
          outputDir = P(sim)$simOutputPath,
          Nreps = max(P(sim)$reps),
          years = P(sim)$years,
          pixelSize = unique(terra::res(sim$rasterToMatch)),
          simFiles = mod$simFiles
        )
        sim <- registerOutputs(f_burnSummary_plot, sim)

        f_cumulBurn_plot <- fireSenseUtils::plotCumulativeBurns(
          climateScenario = P(sim)$climateScenario,
          studyAreaName = P(sim)$studyAreaName,
          outputDir = P(sim)$simOutputPath,
          Nreps = max(P(sim)$reps),
          years = P(sim)$years,
          rasterToMatch = sim$rasterToMatch,
          simFiles = mod$simFiles
        )
        sim <- registerOutputs(f_cumulBurn_plot, sim)

        f_historic_plot <- fireSenseUtils::plotHistoricFires(
          climateScenario = as.character(P(sim)$climateScenario),
          studyAreaName = P(sim)$studyAreaName,
          outputDir = P(sim)$simOutputPath,
          pixelSize = unique(terra::res(sim$rasterToMatch)),
          firePolys = mod$firePolys,
          ignitionPoints = mod$ignitionFirePoints,
          simFiles = mod$simFiles
        )
        sim <- registerOutputs(f_historic_plot, sim)
      }
    },
    save_single = {
      padYear <- paddedFloatToChar(time(sim), padL = ceiling(log10(end(sim) + 1)))

      f_burnMap <- file.path(outputPath(sim), paste0("burnMap_year", padYear, ".tif"))
      terra::writeRaster(sim$burnMap, f_burnMap, datatype = "INT2U", overwrite = TRUE)
      sim <- registerOutputs(f_burnMap, sim)

      f_burnSummary <- file.path(outputPath(sim), "fireSense_burnSummary.csv")
      data.table::fwrite(sim$burnSummary, file = f_burnSummary)
      sim <- registerOutputs(f_burnSummary, sim)
    },
    hindcastRecord = {
      sim <- hindcastRecord(sim)
      if (time(sim) + P(sim)$hindcastInterval <= end(sim) + 1e-9)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$hindcastInterval, "fireSense_summary", "hindcastRecord", .last())
    },
    hindcast = {
      sim <- hindcastSave(sim)
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

#' The files the hindcast writes and reads
#'
#' One definition for the names used by `hindcastSave()` and `hindcastFigures()`.
hindcastFiles <- c(reps = "hindcast_reps.csv", sizes = "hindcast_sizes.csv", observed = "hindcast_observed.csv",
                   observedSizes = "hindcast_observedSizes.csv", observedPolys = "hindcast_observedPolys.gpkg",
                   burnMap = "hindcast_burnMap.tif")

#' Record one simulated rep-year (hindcast, single mode)
#'
#' Called each `hindcastInterval` after `fireSense_burn`. Counts this year's ignitions, escapes, area burned and
#' fires in the ELF core (`fireSenseUtils::hindcastSimulated()`) and writes the year's fire-ID raster.
#' Times need not be whole years: `rep = round((time - hindcastYear) / hindcastInterval) + 1`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly, with `mod$hindcastReps` and `mod$hindcastSizes` extended.
hindcastRecord <- function(sim) {
  needed <- c("rasterToMatchELF", "rstAnnualBurnID", "ignitionsAndEscapes", "ignitionFirePoints", "firePolys")
  missing <- needed[vapply(needed, function(o) is.null(sim[[o]]), logical(1))]
  if (length(missing))
    stop("hindcastYear is set, so fireSense_summary needs: ", paste(missing, collapse = ", "))
  repNo <- as.integer(round((time(sim) - P(sim)$hindcastYear) / P(sim)$hindcastInterval)) + 1L
  one <- fireSenseUtils::hindcastSimulated(sim$rstAnnualBurnID, sim$rasterToMatchELF,
                                           sim$ignitionsAndEscapes, P(sim)$escapeSizeHa)
  mod$hindcastReps <- data.table::rbindlist(list(
    mod$hindcastReps, cbind(data.table::data.table(rep = repNo, year = time(sim)), one$counts)))
  mod$hindcastSizes <- data.table::rbindlist(list(
    mod$hindcastSizes, cbind(data.table::data.table(rep = repNo)[rep(1L, NROW(one$sizes))], one$sizes)))
  f <- file.path(outputPath(sim), sprintf("hindcast_burnID_rep%02d.tif", repNo))
  terra::writeRaster(sim$rstAnnualBurnID, f, datatype = "INT4S", overwrite = TRUE)
  registerOutputs(f, sim)
}

#' Write the hindcast tables and the observed year (hindcast, single mode)
#'
#' At `end(sim)`: the per-rep rows and fire sizes recorded by `hindcastRecord()`, and the observed ignitions,
#' escapes, area burned (natural-cause and all-cause) and fire sizes of `hindcastYear` in the ELF core from
#' `fireSenseUtils::hindcastObserved()`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly.
hindcastSave <- function(sim) {
  obs <- fireSenseUtils::hindcastObserved(sim$rasterToMatchELF, sim$ignitionFirePoints, sim$firePolys,
                                          P(sim)$hindcastYear, escapeSizeHa = P(sim)$escapeSizeHa)
  tables <- list(reps = mod$hindcastReps, sizes = mod$hindcastSizes, observed = obs$counts,
                 observedSizes = obs$sizes)
  files <- file.path(outputPath(sim), hindcastFiles[names(tables)])
  Map(function(x, f) data.table::fwrite(x, f), tables, files)
  if (!is.null(obs$polys) && NROW(obs$polys)) {
    files <- c(files, file.path(outputPath(sim), hindcastFiles[["observedPolys"]]))
    terra::writeVector(obs$polys, files[[length(files)]], overwrite = TRUE)
  }
  ## named for the hindcast, not time(sim), which is a decimal year here
  files <- c(files, file.path(outputPath(sim), hindcastFiles[["burnMap"]]))
  terra::writeRaster(sim$burnMap, files[[length(files)]], datatype = "INT2U", overwrite = TRUE)
  registerOutputs(files, sim)
}

#' Hindcast figures, pooling the replicate folders (multi mode)
#'
#' Reads the files `hindcastSave()` wrote in each replicate folder (a parameter set each) and draws, through
#' `Plots()`, the distributions of the pooled rep-years against the observed year, the fire-size ECDF, and the
#' burn-frequency map with the observed polygons (`fireSenseUtils::plotHindcast*()`).
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly.
hindcastFigures <- function(sim) {
  dirs <- file.path(P(sim)$simOutputPath, dirnamesFromSet(sim$outputsDF$file, P(sim)$reps))
  dirs <- dirs[dir.exists(dirs)]
  if (!length(dirs)) stop("no replicate folders found in ", P(sim)$simOutputPath)
  readAll <- function(name) data.table::rbindlist(lapply(seq_along(dirs), function(i)
    cbind(set = i, data.table::fread(file.path(dirs[i], hindcastFiles[[name]])))))
  reps <- readAll("reps")
  sizes <- readAll("sizes")
  obs <- list(counts = data.table::fread(file.path(dirs[1], hindcastFiles[["observed"]])),
              sizes = data.table::fread(file.path(dirs[1], hindcastFiles[["observedSizes"]])))
  polyFile <- file.path(dirs[1], hindcastFiles[["observedPolys"]])
  polys <- if (file.exists(polyFile)) terra::vect(polyFile)
  ## the burn maps count the rep-years in which a pixel burned; sum them over the parameter sets
  burn <- Reduce(`+`, lapply(dirs, function(d) terra::rast(file.path(d, hindcastFiles[["burnMap"]]))))
  title <- paste(P(sim)$studyAreaName, P(sim)$hindcastYear)
  checkPath(figurePath(sim), create = TRUE)
  Plots(reps, fn = fireSenseUtils::plotHindcastDistributions, observed = obs, title = title,
        types = P(sim)$.plots, filename = "hindcast_distributions", path = figurePath(sim))
  Plots(sizes$ha, fn = fireSenseUtils::plotHindcastSizes, observed = obs, title = title,
        types = P(sim)$.plots, filename = "hindcast_sizes", path = figurePath(sim))
  Plots(burn, fn = fireSenseUtils::plotHindcastMap, nReps = NROW(reps), firePolys = polys, title = title,
        types = P(sim)$.plots, filename = "hindcast_map", path = figurePath(sim))
  invisible(sim)
}

#' Find the replicate output files and the historical fires (multi mode)
#'
#' Stops if expected burn maps or burn summaries are missing from `simOutputPath`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly, with `mod$simFiles` (`NULL` without `outputsDF`),
#'   `mod$firePolys` and `mod$ignitionFirePoints` set, and `P(sim)$years` resolved.
InitMulti <- function(sim) {
  ## check for necessary output files -----------------------------------------------
  ## NOTE: don't load simLists -- slow and unreliable
  mod$useOutputs <- NROW(sim$outputsDF) > 0
  mod$allReps <- dirnamesFromSet(sim$outputsDF$file, P(sim)$reps)
  ## assigned back: P(sim)$years is read downstream, not just for padding
  P(sim)$years <- resolveSimYears(P(sim)$years, sim)
  pad <- padYears(P(sim)$years)

  checkPath(file.path(P(sim)$simOutputPath, "figures", currentModule(sim)), create = TRUE)

  if (mod$useOutputs) {
    mod$bmbs <- grep(
      value = TRUE,
      sim$outputsDF$file,
      pattern = "burnMap|burnSummary"
    ) |>
      grep(paste0("(", paste0(mod$allReps, collapse = "|"), ")"), x = _, value = TRUE) |>
      grep("gri|png|txt|xml", x = _, value = TRUE, invert = TRUE)

    ## the per-rep outputs are not necessarily under `simOutputPath`/repNN (e.g. runs
    ## restored from another machine), so let the plotting functions use these paths
    mod$simFiles <- mod$bmbs
  } else {
    mod$bmbs <- fs::dir_ls(
      P(sim)$simOutputPath,
      regexp = "burnMap|burnSummary",
      recurse = 1,
      type = "file"
    ) |>
      grep(paste0("(", paste0(P(sim)$reps, collapse = "|"), ")"), x = _, value = TRUE) |>
      grep(paste0("_year(", paste0(P(sim)$years, collapse = "|"), ")"), x = _, value = TRUE)

    filesUserHas <- c(mod$bmbs)

    dirsExpected <- file.path(P(sim)$simOutputPath, mod$allReps)
    filesExpected <- as.character(sapply(dirsExpected, function(d) {
      c(
        file.path(d, sprintf("burnMap_year%04d.tif", P(sim)$years[2])),
        file.path(d, "fireSense_burnSummary.csv")
      )
    }))

    filesNeeded <- data.frame(file = filesExpected, exists = filesExpected %in% filesUserHas)

    if (!all(filesNeeded$exists)) {
      missing <- filesNeeded[filesNeeded$exists == FALSE, ]$file
      stop(
        sum(!filesNeeded$exists),
        " simulation files appear to be missing:\n",
        paste(missing, collapse = "\n")
      )
    }
  }

  ## get historical fire points and polys -------------------------------------------

  ## TODO: use an updated/working prepInputs version (fireSenseUtils::getFirePolygons?)
  if (exists("firePolys", envir(sim))) {
    mod$firePolys <- sim$firePolys |>
      tidyterra::bind_spat_rows() |>
      tidyterra::mutate(
        YEAR = as.integer(YEAR)
      )
  } else {
    mod$firePolys <- {
      dst <- inputPath(sim)
      nfdb_url <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip"
      nfdb_zip <- file.path(dst, basename(nfdb_url))

      if (!file.exists(nfdb_zip)) {
        download.file(nfdb_url, destfile = nfdb_zip)
      }

      all_nfdb_files <- fs::dir_ls(dst, regexp = "NFDB_poly_(1972to2020|2021to2024).*")

      if (length(all_nfdb_files) != 16) {
        archive::archive_extract(nfdb_zip, dst)
      }

      nfdb_shp <- fs::dir_ls(dst, regexp = "NFDB_poly_(1972to2020|2021to2024).*[.]shp$")

      purrr::map(.x = nfdb_shp, .f = function(x) {
        p <- terra::vect(x)

        ## NOTE: terra::makeValid takes so long;
        ## just drop the tiny number of invalid geometries
        p[terra::is.valid(p), ]
      }) |>
        tidyterra::bind_spat_rows() |>
        tidyterra::mutate(
          YEAR = as.integer(YEAR),
          MONTH = as.integer(MONTH),
          DAY = as.integer(DAY)
        ) |>
        terra::project(sim$rasterToMatch)
    }
  }

  # plotHistoricFires expects SIZE_HA
  if (!"SIZE_HA" %in% names(mod$firePolys)) {
    if ("ADJ_HA" %in% names(mod$firePolys)) {
      mod$firePolys <- mod$firePolys |>
        tidyterra::mutate(
          SIZE_HA = ADJ_HA
        )
    } else {
      ## NOTE: `mod$firePolys`, not `sim$firePolys`: by this point the polygons have
      ## been bound and typed above, whereas when they were supplied as an input
      ## `sim$firePolys` is still the *list* of annual SpatVectors, which
      ## tidyterra::mutate() cannot take.
      mod$firePolys <- mod$firePolys |>
        tidyterra::mutate(
          SIZE_HA = POLY_HA
        )
    }
  }

  ## NFDB points, harmonised and clipped to the template raster by fireregimetools (the URL this
  ## module used to hard-code, .../NFDB_point.zip, returns 404 now)
  mod$ignitionFirePoints <- if (exists("ignitionFirePoints", envir(sim))) {
    sim$ignitionFirePoints
  } else {
    fireregimetools::fetch_nfdb_points(sim$rasterToMatch, dest = inputPath(sim))
  }

  return(invisible(sim))
}

#' No default inputs
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly.
.inputObjects <- function(sim) {
  return(invisible(sim))
}
