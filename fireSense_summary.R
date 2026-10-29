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
  version = list(fireSense_summary = "1.1.0.9008"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "fireSense_summary.Rmd"), ## same file
  loadOrder = list(after = c("fireSense_burn")),
  reqdPkgs = list(
    "assertthat", "cowplot", "data.table", "fs", "ggplot2", "googledrive",
    "FOR-CAST/fireregimetools@main (>= 0.1.0.9001)", ## fetch_nfdb_points()
    "purrr", "qs2", "RColorBrewer", "reproducible", "terra", "tidyterra",
    "raster", "rasterVis", ## TODO: remove these once fireSenseUtils::plotCumulativeBurns switched to ggplot2/tidyterra
    "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9127)", ## hindcast*(); hindcastObserved()$points, flammableRTM; ELFmetricsFilename(); escapes from burned-area polygons (#187)
    "PredictiveEcology/SpaDES.core@development (>= 3.2.1.9038)", ## dirnamesFromSet(leafOnly = FALSE) returns the rep folder, resolveSimYears(), padYears()
    "PredictiveEcology/SpaDES.tools@development (>= 2.1.1.9000)"
  ),
  parameters = rbind(
    defineParameter(".neverCache", "character", c("init", "save_single", "hindcastRecord", "hindcast"), NA, NA,
                    paste("Events that are never cached, whatever `.useCache` says (read by SpaDES.core, 3.2.1.9037 or later):",
                          "every event reads the run's output files or writes files (maps, csv, figures), none of which is in the event's cache key, and a cache hit would write nothing.")),
    defineParameter("climateScenario", "character", NA, NA, NA,
                    desc = paste("Name of the CMIP6 climate scenario including SSP, formatted as in `ClimateNA`,",
                                 "e.g. 'CanESM5_SSP370'. Used in figure filenames (multi mode).")),
    defineParameter("mode", "character", "single", NA, NA,
                    paste("'single': run within a simulation, saving `burnMap` and `burnSummary` at `end(sim)`.",
                          "'multi': summarize the saved outputs of several replicates in figures.")),
    defineParameter("simOutputPath", "character", outputPath(sim), NA, NA,
                    desc = "Directory holding the replicate output directories, and where figures are written (multi mode)."),
    defineParameter(".runName", "character", NA_character_, NA, NA,
                    desc = paste("Name of this run, e.g. '13.1_NRV_370_1'; names its metrics file",
                                 "(`fireSenseUtils::ELFmetricsFilename()`). `NA`: the name of `outputPath(sim)`.")),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    desc = paste("Human-readable name for the study area; used in figure paths and filenames (multi mode).",
                                 "If `NA`, a hash of `rasterToMatch` will be used.")),
    defineParameter("reps", "integer", 1L:10L, 1, NA,
                    desc = paste("Replicate numbers to summarize (multi mode). Files are read with `mclapply`;",
                                 "set `options(mc.cores = )` to run in parallel.")),
    defineParameter("escapeSizeHa", "numeric", 50, 0, NA,
                    desc = "Fire size (ha) counted as an escape in the hindcast (`nFires50`); as in the fit."),
    defineParameter("fireCauses", "character", NULL, NA, NA,
                    desc = paste("Cause groups counted as the observed fire of the hindcast (`natural`, `unknown`, `human`,",
                                 "`other`; `fireSenseUtils::fireCauseCodes()`). `NULL` (default): NFDB L and N, NBAC Natural.",
                                 "Set it to the `fireCauses` of `fireSense_dataPrepFit` so the observed fire has the fit's causes.")),
    defineParameter("hindcastInterval", "numeric", 1, 0, NA,
                    desc = paste("Hindcast: time between the simulated rep-years. A job for year Y runs times",
                                 "Y, Y + interval, ...; rep = round((time - hindcastYear) / interval) + 1.",
                                 "Set it to the predict modules' interval (e.g. 0.001).")),
    defineParameter("hindcastYear", "integer", NA_integer_, NA, NA,
                    desc = paste("Hindcast validation: the observed year the simulated rep-years are compared with.",
                                 "`NA` (default): no hindcast. Single mode records each rep-year and, at `end(sim)`,",
                                 "writes `hindcast_reps.csv`, `hindcast_sizes.csv`, `hindcast_observed.csv`,",
                                 "`hindcast_observedSizes.csv`, `hindcast_observedPolys.gpkg`, `hindcast_observedPoints.gpkg`,",
                                 "`hindcast_burnMap.tif` and, per rep, `hindcast_burnID_rep<rep>.tif`,",
                                 "`hindcast_ignitionProb_rep<rep>.tif` and `hindcast_escapeProb_rep<rep>.tif`. Multi mode pools the replicate folders",
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
    expectsInput("fireSense_IgAndEscapeProbRas", "SpatRaster", sourceURL = NA,
                 paste("Optional; hindcast (single mode). This year's layers `ignitionProb` and `escapeProb` from",
                       "`fireSense_ignitionPredict`, written for every rep.")),
    expectsInput("rasterToMatchELF", "SpatRaster", sourceURL = NA,
                 "Optional; hindcast (single mode). The ELF core; the simulation and the observations are clipped to it."),
    expectsInput("flammableRTM", "SpatRaster", sourceURL = NA,
                 paste("Optional; hindcast (single mode). The pixels the simulation can burn (from `fireSense_burn`);",
                       "the observed area burned and fire sizes are counted on them.")),
    expectsInput("rstAnnualBurnID", "SpatRaster", sourceURL = NA,
                 "Optional; hindcast (single mode). This year's fire IDs from `fireSense_burn`."),
    expectsInput("fireSense_SpreadParamSet", "integer", sourceURL = NA,
                 paste("Optional; hindcast (single mode). The row of the fitted parameter sets this year's spread",
                       "prediction used, from `fireSense_spreadPredict`; written as the `paramSet` column of the",
                       "hindcast tables (`NA` if absent or, with several ELFs, not one number).")),
    expectsInput("outputsDF", "data.table",
                 desc = paste("Optional; multi mode. `outputs(sim)` of all replicates, row-bound. Its `file` column",
                              "locates the burn maps and summaries. If missing, `simOutputPath` is searched."),
                 sourceURL = NA),
    expectsInput(".ELFind", "character", sourceURL = NA,
                 "Optional; single mode. The ELF of this run, recorded in its metrics row."),
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
      ## `studyAreaName` was renamed `.studyAreaName`; still honour the old name
      oldStudyAreaName <- P(sim)$studyAreaName # nolint: param_used_undeclared
      if (!is.null(oldStudyAreaName)) {
        warning("fireSense_summary: the `studyAreaName` parameter is now `.studyAreaName`; ",
                "please rename it.", call. = FALSE)
        if (is.na(P(sim)$.studyAreaName))
          P(sim)$.studyAreaName <- oldStudyAreaName
      }
      ## the module has no studyArea, so an unnamed study area is named after its template raster
      if (is.na(P(sim)$.studyAreaName) && !is.null(sim$rasterToMatch))
        P(sim)$.studyAreaName <- reproducible::.robustDigest(sim$rasterToMatch, algo = "xxhash64")
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
          studyAreaName = P(sim)$.studyAreaName,
          outputDir = P(sim)$simOutputPath,
          Nreps = max(P(sim)$reps),
          years = P(sim)$years,
          pixelSize = unique(terra::res(sim$rasterToMatch)),
          simFiles = mod$simFiles
        )
        sim <- registerOutputs(f_burnSummary_plot, sim)

        f_cumulBurn_plot <- fireSenseUtils::plotCumulativeBurns(
          climateScenario = P(sim)$climateScenario,
          studyAreaName = P(sim)$.studyAreaName,
          outputDir = P(sim)$simOutputPath,
          Nreps = max(P(sim)$reps),
          years = P(sim)$years,
          rasterToMatch = sim$rasterToMatch,
          simFiles = mod$simFiles
        )
        sim <- registerOutputs(f_cumulBurn_plot, sim)

        f_historic_plot <- fireSenseUtils::plotHistoricFires(
          climateScenario = as.character(P(sim)$climateScenario),
          studyAreaName = P(sim)$.studyAreaName,
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
      ## fireSense_burn leaves burnSummary NULL in a run without fire; the file is still written (empty),
      ## because the multi-mode summary expects one per run
      data.table::fwrite(sim$burnSummary %||% data.table::data.table(), file = f_burnSummary)
      sim <- registerOutputs(f_burnSummary, sim)
      ## a hindcast writes its metrics in hindcastSave(), after its last rep-year
      if (is.na(P(sim)$hindcastYear))
        sim <- writeRunMetrics(sim)
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
                   observedPoints = "hindcast_observedPoints.gpkg", burnMap = "hindcast_burnMap.tif")

#' The per-rep maps the hindcast writes
#'
#' The layers of `fireSense_IgAndEscapeProbRas` that `hindcastRecord()` writes, one file per rep each.
hindcastRepMaps <- c("ignitionProb", "escapeProb")

#' Record one simulated rep-year (hindcast, single mode)
#'
#' Called each `hindcastInterval` after `fireSense_burn`. Counts this year's ignitions, escapes, area burned and
#' fires in the ELF core (`fireSenseUtils::hindcastSimulated()`) and writes the year's fire-ID raster and the
#' ignition and escape probability maps used. Both tables carry `paramSet`, the parameter set
#' `fireSense_spreadPredict` used for the rep (`sim$fireSense_SpreadParamSet`; `NA` if absent).
#' Times need not be whole years: `rep = round((time - hindcastYear) / hindcastInterval) + 1`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly, with `mod$hindcastReps` and `mod$hindcastSizes` extended.
hindcastRecord <- function(sim) {
  needed <- c("rasterToMatchELF", "flammableRTM", "rstAnnualBurnID", "ignitionsAndEscapes", "fireSense_IgAndEscapeProbRas",
              "ignitionFirePoints", "firePolys")
  missing <- needed[vapply(needed, function(o) is.null(sim[[o]]), logical(1))]
  if (length(missing))
    stop("hindcastYear is set, so fireSense_summary needs: ", paste(missing, collapse = ", "))
  repNo <- as.integer(round((time(sim) - P(sim)$hindcastYear) / P(sim)$hindcastInterval)) + 1L
  paramSet <- if (length(sim$fireSense_SpreadParamSet) == 1L) as.integer(sim$fireSense_SpreadParamSet) else NA_integer_
  one <- fireSenseUtils::hindcastSimulated(sim$rstAnnualBurnID, sim$rasterToMatchELF,
                                           sim$ignitionsAndEscapes, P(sim)$escapeSizeHa)
  mod$hindcastReps <- data.table::rbindlist(list(
    mod$hindcastReps, cbind(data.table::data.table(rep = repNo, paramSet = paramSet,
                                                   year = as.integer(floor(time(sim) + 1e-9)),
                                                   time = time(sim)), one$counts)))
  mod$hindcastSizes <- data.table::rbindlist(list(
    mod$hindcastSizes, cbind(data.table::data.table(rep = repNo, paramSet = paramSet)[rep(1L, NROW(one$sizes))], one$sizes)))
  f <- file.path(outputPath(sim), sprintf("hindcast_burnID_rep%02d.tif", repNo))
  terra::writeRaster(sim$rstAnnualBurnID, f, datatype = "INT4S", overwrite = TRUE)
  ## each rep draws its own climate, so the probability maps differ by rep
  maps <- file.path(outputPath(sim), sprintf("hindcast_%s_rep%02d.tif", hindcastRepMaps, repNo))
  Map(function(layer, file) terra::writeRaster(sim$fireSense_IgAndEscapeProbRas[[layer]], file, overwrite = TRUE),
      hindcastRepMaps, maps)
  registerOutputs(c(f, maps), sim)
}

#' Write the hindcast tables and the observed year (hindcast, single mode)
#'
#' At `end(sim)`: the per-rep rows and fire sizes recorded by `hindcastRecord()`, and the observed ignitions,
#' escapes, area burned (natural-cause and all-cause), fire sizes and natural ignition points of `hindcastYear`
#' in the ELF core (area and sizes on the burnable pixels, `flammableRTM`) from `fireSenseUtils::hindcastObserved()`.
#'
#' @param sim A `simList`.
#'
#' @return The `simList`, invisibly.
hindcastSave <- function(sim) {
  obs <- fireSenseUtils::hindcastObserved(sim$rasterToMatchELF, sim$ignitionFirePoints, sim$firePolys,
                                          P(sim)$hindcastYear, escapeSizeHa = P(sim)$escapeSizeHa,
                                          flammableRTM = sim$flammableRTM, fireCauses = P(sim)$fireCauses)
  tables <- list(reps = mod$hindcastReps, sizes = mod$hindcastSizes, observed = obs$counts,
                 observedSizes = obs$sizes)
  files <- file.path(outputPath(sim), hindcastFiles[names(tables)])
  Map(function(x, f) data.table::fwrite(x, f), tables, files)
  if (!is.null(obs$polys) && NROW(obs$polys)) {
    files <- c(files, file.path(outputPath(sim), hindcastFiles[["observedPolys"]]))
    terra::writeVector(obs$polys, files[[length(files)]], overwrite = TRUE)
  }
  if (!is.null(obs$points) && NROW(obs$points)) {
    files <- c(files, file.path(outputPath(sim), hindcastFiles[["observedPoints"]]))
    terra::writeVector(obs$points, files[[length(files)]], overwrite = TRUE)
  }
  ## named for the hindcast, not time(sim), which is a decimal year here
  files <- c(files, file.path(outputPath(sim), hindcastFiles[["burnMap"]]))
  terra::writeRaster(sim$burnMap, files[[length(files)]], datatype = "INT2U", overwrite = TRUE)
  sim <- registerOutputs(files, sim)
  writeRunMetrics(sim, observed = obs$counts)
}

#' Write this run's metrics row (single mode)
#'
#' One row, written at `end(sim)` to `fireSenseUtils::ELFmetricsFilename(.runName)` in `outputPath(sim)`, for the
#' national map: `fireSenseUtils::readELFmetrics()` collects the runs' rows and `fireSenseUtils::plotELFs(metrics =)`
#' maps them. The row names the run (`runName`), the ELF (`sim$.ELFind`) and the spread and ignition ledger files
#' whose fits the run used, so the reader can drop runs made with older fits; then the metrics. A hindcast gives
#' the mean of its rep-years (`sim_*`, from `hindcastRecord()`) and the observed year (`obs_*`); any other run its
#' fires and area burned (`burnSummary`, buffer included) per year `fireSense_burn` burned.
#'
#' @param sim A `simList`.
#' @param observed Hindcast only: `counts` of `fireSenseUtils::hindcastObserved()`.
#'
#' @return The `simList`, invisibly, with the file registered in `outputs(sim)`.
writeRunMetrics <- function(sim, observed = NULL) {
  runName <- P(sim)$.runName
  if (is.na(runName)) runName <- basename(outputPath(sim))
  ledgers <- fitLedgerFiles(sim)
  row <- data.table::data.table(runName = runName, ELF = if (is.null(sim$.ELFind)) NA_character_ else sim$.ELFind,
                                spreadFitFile = ledgers[["spread"]], ignitionFitFile = ledgers[["ignition"]])
  if (is.null(observed)) {
    burn <- params(sim)$fireSense_burn
    burnYears <- if (is.null(burn)) NA_integer_ else
      length(seq(burn$.runInitialTime, end(sim), by = burn$.runInterval))
    row[, `:=`(burnYears = burnYears, firesPerYear = NROW(sim$burnSummary) / burnYears,
               areaBurnedHaPerYear = sum(sim$burnSummary$areaBurnedHa) / burnYears)]
  } else {
    cols <- c("ignitions", "escapes", "areaBurnedHa", "nFires50")
    simulated <- mod$hindcastReps[, lapply(.SD, mean), .SDcols = cols]
    obsCols <- setdiff(names(observed), "year")
    row[, `:=`(hindcastYear = P(sim)$hindcastYear, reps = NROW(mod$hindcastReps))]
    row[, paste0("sim_", cols) := simulated]
    row[, paste0("obs_", obsCols) := observed[, obsCols, with = FALSE]]
  }
  f <- file.path(outputPath(sim), fireSenseUtils::ELFmetricsFilename(runName))
  data.table::fwrite(row, f)
  registerOutputs(f, sim)
}

#' The spread and ignition ledger files whose fits this run used
#'
#' The fit modules' `spreadFitFilename` and `ignitionFitFilename`; `"latest"` is the file a fit over `fireYears`
#' with `fitFileSuffix` is written to (`fireSenseUtils::spreadFitFilenameFor()`, `ignitionFitFilenameFor()`). This
#' module has none of these parameters, so they are read from the other modules, which check that they agree.
#'
#' @param sim A `simList`.
#'
#' @return A named character vector, `spread` and `ignition`; `NA` where no module has the parameter.
fitLedgerFiles <- function(sim) {
  fromOthers <- function(name) {
    v <- unique(Filter(Negate(is.null), lapply(params(sim), `[[`, name)))
    if (length(v)) v[[1]]
  }
  fireYears <- fromOthers("fireYears")
  suffix <- fromOthers("fitFileSuffix")
  if (is.null(suffix)) suffix <- ""
  resolve <- function(file, filenameFor) {
    if (is.null(file)) return(NA_character_)
    if (!identical(file, "latest")) return(file)
    if (is.null(fireYears)) NA_character_ else filenameFor(fireYears, suffix)
  }
  c(spread = resolve(fromOthers("spreadFitFilename"), fireSenseUtils::spreadFitFilenameFor),
    ignition = resolve(fromOthers("ignitionFitFilename"), fireSenseUtils::ignitionFitFilenameFor))
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
  title <- paste(P(sim)$.studyAreaName, P(sim)$hindcastYear)
  checkPath(figurePath(sim), create = TRUE)
  Plots(reps, fn = fireSenseUtils::plotHindcastDistributions, observed = obs, title = title,
        types = P(sim)$.plots, filename = "hindcast_distributions", path = figurePath(sim))
  Plots(sizes$ha, fn = fireSenseUtils::plotHindcastSizes, observed = obs, title = title,
        types = P(sim)$.plots, filename = "hindcast_sizes", path = figurePath(sim))
  Plots(burn, fn = fireSenseUtils::plotHindcastMap, nReps = NROW(reps), firePolys = polys, title = title,
        types = P(sim)$.plots, filename = "hindcast_map", path = figurePath(sim))
  invisible(sim)
}

## The National Fire Database polygon shapefiles, downloaded (once) and unzipped into `dst` by
## prepInputs(), so the access is cached and appears in the download ledger (urlLog(sim)).
## The zip holds two shapefiles (1972-2020 and 2021-2024), so `fun = NA`: prepInputs() returns
## the unzipped paths and each shapefile is read by the caller.
nfdbShapefiles <- function(dst) {
  nfdb_url <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip"
  files <- reproducible::prepInputs(url = nfdb_url, destinationPath = dst, fun = NA)
  grep("NFDB_poly_(1972to2020|2021to2024).*[.]shp$", files, value = TRUE)
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
  ## without outputsDF, the files on disk say how the rep folders are padded (rep1 or rep01)
  bmbPattern <- "burnMap|burnSummary"
  filesOnDisk <- if (mod$useOutputs) {
    sim$outputsDF$file
  } else {
    fs::dir_ls(
      P(sim)$simOutputPath,
      regexp = bmbPattern,
      recurse = 1,
      type = "file"
    )
  }
  mod$allReps <- dirnamesFromSet(filesOnDisk, P(sim)$reps)
  ## assigned back: P(sim)$years is read downstream, not just for padding
  P(sim)$years <- resolveSimYears(P(sim)$years, sim)
  pad <- padYears(P(sim)$years)

  checkPath(file.path(P(sim)$simOutputPath, "figures", currentModule(sim)), create = TRUE)

  if (mod$useOutputs) {
    mod$bmbs <- grep(
      value = TRUE,
      filesOnDisk,
      pattern = bmbPattern
    ) |>
      grep(paste0("(", paste0(mod$allReps, collapse = "|"), ")"), x = _, value = TRUE) |>
      grep("gri|png|txt|xml", x = _, value = TRUE, invert = TRUE)

    ## the per-rep outputs are not necessarily under `simOutputPath`/repNN (e.g. runs
    ## restored from another machine), so let the plotting functions use these paths
    mod$simFiles <- mod$bmbs
  } else {
    inReps <- grepl(paste0("(", paste0(P(sim)$reps, collapse = "|"), ")"), filesOnDisk)
    ## the year is in the burn map names only; the burn summary csv has none
    inYears <- !grepl("burnMap", filesOnDisk) |
      grepl(paste0("_year(", paste0(P(sim)$years, collapse = "|"), ")"), filesOnDisk)
    mod$bmbs <- filesOnDisk[inReps & inYears]

    if (length(mod$allReps) < length(P(sim)$reps)) {
      stop("replicate folders appear to be missing under ", P(sim)$simOutputPath,
           ": found ", length(mod$allReps), " of ", length(P(sim)$reps))
    }

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
      nfdb_shp <- nfdbShapefiles(inputPath(sim))

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
