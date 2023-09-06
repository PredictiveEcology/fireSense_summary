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
  version = list(fireSense_summary = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "fireSense_summary.Rmd"), ## same file
  reqdPkgs = list("assertthat", "cowplot", "data.table", "disk.frame", "fs",
                  "PredictiveEcology/fireSenseUtils@dev-stable (>= 0.0.5.9048)",
                  "ggplot2", "googledrive", "purrr", "raster", "rasterVis", "RColorBrewer",
                  "SpaDES.core (>= 1.0.10)", "SpaDES.tools", "qs"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("climateScenarios", "character", NA, NA, NA,
                    desc = paste("names of CIMP6 climate scenarios including SSP,",
                                 "formatted as in ClimateNA, using underscores as separator.",
                                 "E.g., 'CanESM5_SSP370'.")),
    defineParameter("simOutputPath", "character", outputPath(sim), NA, NA,
                    desc = "Directory specifying the location of the simulation outputs."),
    defineParameter("studyAreaNames", "character", NA, NA, NA,
                    desc = "names of study areas simulated."),
    defineParameter("reps", "integer", 1:10, 1, NA,
                    desc = paste("number of replicates/runs per study area and climate scenario.",
                                 "NOTE: `mclapply` is used internally, so you should set",
                                 "`options(mc.cores = nReps)` to run in parallel.")),
    defineParameter("upload", "logical", FALSE, NA, NA,
                    desc = "if TRUE, uses the `googledrive` package to upload figures."),
    defineParameter("years", "integer", c(2011, 2100), NA, NA,
                    desc = "Which two simulation years should be compared? Typically start and end years.")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("burnMap", "RasterLayer",
                 "loaded from disk (i.e., from files in `simOutputPath`).",
                 sourceURL = NA),
    expectsInput("burnSummary", "data.table",
                 "loaded from disk (i.e., from files in `simOutputPath`).",
                 sourceURL = NA),
    expectsInput("rasterToMatch", "RasterLayer", "DESCRIPTION NEEDED", sourceURL = NA),
    expectsInput("uploadTo", "character",
                 desc = paste("if `upload = TRUE`, a named list of Google Drive folder ids,",
                              "corresponding to `studyAreaNames`."),
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.fireSense_summary = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "fireSense_summary", "plot_burnSummary")
      sim <- scheduleEvent(sim, start(sim), "fireSense_summary", "plot_cumulBurn")
      sim <- scheduleEvent(sim, start(sim), "fireSense_summary", "plot_historic")

      if (isTRUE(P(sim)$upload)) {
        sim <- scheduleEvent(sim, end(sim), "fireSense_summary", "upload", .last())
      }
    },
    plot_burnSummary = {
      # ! ----- EDIT BELOW ----- ! #

      files2upload <- lapply(P(sim)$studyAreaNames, function(studyAreaName) {
        lapply(P(sim)$climateScenarios, function(climateScenario) {
          plotBurnSummary(
            studyAreaName = studyAreaName,
            climateScenario = climateScenario,
            outputDir = P(sim)$simOutputPath,
            Nreps = max(P(sim)$reps)
          )
        })
      })
      files2upload <- unlist(files2upload, recursive = TRUE)

      mod$files2upload <- c(mod$files2upload, files2upload)

      # ! ----- STOP EDITING ----- ! #
    },
    plot_cumulBurn = {
      # ! ----- EDIT BELOW ----- ! #

      files2upload <- lapply(P(sim)$studyAreaNames, function(studyAreaName) {
        ## get rasterToMatch for each studyArea
        tmp <- loadSimList(file.path(P(sim)$simOutputPath, studyAreaName,
                                     paste0("simOutPreamble_", studyAreaName, "_",
                                            gsub("SSP", "", P(sim)$climateScenarios[1]), ".qs")))
        sim$rasterToMatch <- tmp$rasterToMatchReporting
        rm(tmp)

        lapply(P(sim)$climateScenarios, function(climateScenario) {
          plotCumulativeBurns(
            studyAreaName = studyAreaName,
            climateScenario = climateScenario,
            outputDir = P(sim)$simOutputPath,
            Nreps = max(P(sim)$reps),
            rasterToMatch = sim$rasterToMatch
          )
        })
      })
      files2upload <- unlist(files2upload, recursive = TRUE)

      mod$files2upload <- c(mod$files2upload, files2upload)

      # ! ----- STOP EDITING ----- ! #
    },
    plot_historic = {
      # ! ----- EDIT BELOW ----- ! #

      files2upload <- lapply(P(sim)$studyAreaNames, function(studyAreaName) {
        lapply(P(sim)$climateScenarios, function(climateScenario) {
          plotHistoricFires(
            climateScenario = climateScenario,
            studyAreaName = studyAreaName,
            outputDir = P(sim)$simOutputPath,
            firePolys = mod$firePolys,
            ignitionPoints = mod$ignitionFirePoints
          )
        })
      })
      files2upload <- unlist(files2upload, recursive = TRUE)

      mod$files2upload <- c(mod$files2upload, files2upload)

      # ! ----- STOP EDITING ----- ! #
    },
    upload = {
      # ! ----- EDIT BELOW ----- ! #
      mod$files2upload <- set_names(mod$files2upload, basename(mod$files2upload))

      gid <- as_id(sim$uploadTo[[P(sim)$studyAreaName]])
      prevUploaded <- drive_ls(gid)
      toUpload <- mod$files2upload[!(basename(mod$files2upload) %in% prevUploaded$name)]
      uploaded <- map(toUpload, ~ drive_upload(.x, path = gid))
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  checkPath(file.path(P(sim)$simOutputPath, P(sim)$studyAreaNames, "figures"), create = TRUE)

  ## TODO: inventory all files to ensure correct dir structure? compare against expected files?
  #filesUserHas <- fs::dir_ls(P(sim)$simOutputPath, recurse = TRUE, type = "file", glob = "*.qs")

  filesUserExpects <- rbindlist(lapply(P(sim)$studyAreaNames, function(studyAreaName) {
    rbindlist(lapply(P(sim)$climateScenarios, function(climateScenario) {
      rbindlist(lapply(P(sim)$reps, function(rep) {
        runName <- sprintf("%s_%s", studyAreaName, climateScenario)
        f <- file.path(P(sim)$simOutputPath, runName, sprintf("rep%02d", as.integer(rep)),
                       paste0(runName, "_", sprintf("rep%02d", as.integer(rep)), ".qs"))

        data.table(file = f, exists = file.exists(f))
      }))
    }))
  }))

  if (!all(filesUserExpects$exists)) {
    missing <- filesUserExpects[exists == FALSE, ]$file
    stop("Some simulation files missing:\n", paste(missing, collapse = "\n"))
  }

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  ## same historic fire polygons for all sims, so it doesn't matter which one we load
  sim_fsDP <- loadSimList(file.path("outputs", P(sim)$studyAreaNames[[1]],
                                    paste0("fSsimDataPrep_", P(sim)$studyAreaNames[[1]], ".qs")))
  mod$firePolys <- sim_fsDP$firePolys
  mod$ignitionFirePoints <- sim_fsDP$ignitionFirePoints
  rm(sim_fsDP)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
