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
  version = list(fireSense_summary = "1.0.2"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "fireSense_summary.Rmd"), ## same file
  loadOrder = list(after = c("fireSense")),
  reqdPkgs = list(
    "assertthat", "cowplot", "data.table", "fs", "ggplot2", "googledrive",
    "purrr", "qs2", "RColorBrewer", "terra", "tidyterra",
    "raster", "rasterVis", ## TODO: remove these once fireSenseUtils::plotCumulativeBurns switched to ggplot2/tidyterra
    "PredictiveEcology/fireSenseUtils@development (>= 0.1.2.9000)",
    "PredictiveEcology/SpaDES.core@development (>= 3.0.3.9003)",
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
    defineParameter("years", "integer", c(NA_integer_, NA_integer_), NA, NA,
                    desc = paste("Which two simulation years should be compared?",
                                 "Typically start and end years.",
                                 "Defaults to the simulation's own start and end times."))
  ),
  inputObjects = bindrows(
    expectsInput("burnMap", "SpatRaster",
                 desc = "Cumulative burn map from `fireSense`. Required in single mode.",
                 sourceURL = NA),
    expectsInput("burnSummary", "data.table",
                 "Fire summary table from `fireSense`. Required in single mode.",
                 sourceURL = NA),
    expectsInput("firePolys", "list", sourceURL = NA,
                 paste("Optional; multi mode. List of annual historical fire polygons.",
                       "If missing, the NFDB polygons are downloaded.")),
    expectsInput("ignitionFirePoints", "SpatVector", sourceURL = NA,
                 paste("Optional; multi mode. Historical fire ignition points.",
                       "If missing, the NFDB points are downloaded.")),
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
#' `init`: in single mode, schedules `save_single` at `end(sim)`; in multi mode,
#' runs `InitMulti()` and makes the burn summary, cumulative burn and historic
#' fire figures. `save_single` writes `burnMap` (`.tif`) and `burnSummary` (`.csv`)
#' to `outputPath(sim)`.
#'
#' @param sim A `simList`.
#' @param eventTime Time of the event.
#' @param eventType `"init"` or `"save_single"`.
#'
#' @return The `simList`, invisibly.
doEvent.fireSense_summary = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      if (P(sim)$mode == "single") {
        sim <- scheduleEvent(sim, end(sim), "fireSense_summary", "save_single", .last())
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
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
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

  ## TODO: use an updated/working prepInputs version (fireSenseUtils::getFirePoints_NFDB_V2?)
  if (exists("ignitionFirePoints", envir(sim))) {
    mod$ignitionFirePoints <- sim$ignitionFirePoints
  } else {
    mod$ignitionFirePoints <- {
      dst <- inputPath(sim)

      nfdb_url <- "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip"
      nfdb_zip <- file.path(dst, basename(nfdb_url))

      if (!file.exists(nfdb_zip)) {
        download.file(nfdb_url, destfile = nfdb_zip)
      }

      all_nfdb_files <- fs::dir_ls(dst, regexp = "NFDB_point_.*")

      if (length(all_nfdb_files) != 10) {
        archive::archive_extract(nfdb_zip, dst)
      }

      nfdb_shp <- fs::dir_ls(dst, regexp = "NFDB_point_.*[.]shp$")

      ## NOTE: using terra here because it's much faster than sf
      p <- terra::vect(nfdb_shp)

      ## NOTE: terra::makeValid takes so long;
      ## just drop the tiny number of invalid geometries
      p[terra::is.valid(p), ] |>
        tidyterra::mutate(
          YEAR = as.integer(YEAR),
          MONTH = as.integer(MONTH),
          DAY = as.integer(DAY)
        ) |>
        terra::project(sim$rasterToMatch)
    }
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
