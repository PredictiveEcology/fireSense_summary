## `save_single` is the only event that runs in a plain simulation, and it is the
## one a project actually depends on: everything the multi-mode summary later reads
## is written here. These run the real event through spades(), not the function in
## isolation.

makeSingleSim <- function(times = list(start = 2011, end = 2013), ...) {
  out <- withr::local_tempdir(.local_envir = parent.frame())
  SpaDES.core::simInit(
    times = times,
    params = list(fireSense_summary = list(mode = "single")),
    modules = "fireSense_summary",
    objects = list(burnMap = toyBurnMap(), burnSummary = toyBurnSummary(), ...),
    paths = list(modulePath = modulePath, outputPath = out,
                 cachePath = file.path(out, "cache"), inputPath = file.path(out, "in"))
  )
}

test_that("single mode writes the burn map, summary and metrics row, named for end(sim)", {
  s <- makeSingleSim()
  out <- SpaDES.core::outputPath(s)
  sOut <- SpaDES.core::spades(s, debug = FALSE)

  ## 2013 with padL = ceiling(log10(2014)) = 4 -> "2013", hence burnMap_year2013.tif
  expect_setequal(
    setdiff(list.files(out), c("cache", "in")),
    c("burnMap_year2013.tif", "fireSense_burnSummary.csv", paste0("ELFmetrics_", basename(out), ".csv"))
  )

  ## the raster round-trips: 6 burned pixels of 16, written as INT2U
  r <- terra::rast(file.path(out, "burnMap_year2013.tif"))
  expect_equal(dim(r), c(4L, 4L, 1L))
  expect_equal(sum(terra::values(r)), 6)
  expect_identical(terra::crs(r, describe = TRUE)$code, "3978")

  ## and so does the summary table
  dt <- data.table::fread(file.path(out, "fireSense_burnSummary.csv"))
  expect_equal(dim(dt), c(3L, 3L))
  expect_identical(names(dt), c("year", "rep", "sizeHa"))
  expect_equal(dt$sizeHa, c(10, 20, 30))
})

test_that("a run without fire writes an empty burn summary", {
  ## fireSense_burn leaves burnSummary NULL when nothing burned; a one-year hindcast run
  ## without fire failed here (fwrite: is.list(x) is not TRUE)
  out <- withr::local_tempdir()
  s <- SpaDES.core::simInit(
    times = list(start = 2011, end = 2011),
    params = list(fireSense_summary = list(mode = "single")),
    modules = "fireSense_summary",
    objects = list(burnMap = toyBurnMap()),
    paths = list(modulePath = modulePath, outputPath = out,
                 cachePath = file.path(out, "cache"), inputPath = file.path(out, "in"))
  )
  SpaDES.core::spades(s, debug = FALSE)
  f <- file.path(out, "fireSense_burnSummary.csv")
  expect_true(file.exists(f))
  expect_identical(NROW(suppressWarnings(data.table::fread(f))), 0L)
})

test_that("all written files are registered in outputs(sim)", {
  ## registerOutputs() is what makes these files discoverable by a later multi-mode
  ## run via `outputsDF`; if it stops happening the summary silently sees nothing.
  s <- makeSingleSim()
  out <- SpaDES.core::outputPath(s)
  sOut <- SpaDES.core::spades(s, debug = FALSE)

  odf <- SpaDES.core::outputs(sOut)
  expect_equal(NROW(odf), 3L)
  expect_setequal(basename(odf$file),
                  c("burnMap_year2013.tif", "fireSense_burnSummary.csv", paste0("ELFmetrics_", basename(out), ".csv")))
  expect_equal(unique(odf$saveTime), 2013)
})

test_that("the burn map filename follows end(sim), not start(sim)", {
  ## The padded year comes from time(sim) at the scheduled event, which is end(sim).
  ## A 5-digit end year also exercises the ceiling(log10(end + 1)) padding rule.
  s <- makeSingleSim(times = list(start = 1, end = 7))
  out <- SpaDES.core::outputPath(s)
  SpaDES.core::spades(s, debug = FALSE)
  ## padL = ceiling(log10(8)) = 1, so no zero padding
  expect_true(file.exists(file.path(out, "burnMap_year7.tif")))
  expect_false(file.exists(file.path(out, "burnMap_year1.tif")))
})

test_that("an unknown event type warns", {
  ## `noEventWarning()` in SpaDES.core only *returns* the message string, so the
  ## module has to wrap it in warning(); bare, `switch()`'s default value was
  ## discarded and an unknown event type was silently a no-op.
  s <- makeSingleSim()
  s <- SpaDES.core::scheduleEvent(s, SpaDES.core::start(s), "fireSense_summary", "notAnEvent")
  expect_warning(
    SpaDES.core::spades(s, debug = FALSE),
    "Undefined event type: 'notAnEvent' in module 'fireSense_summary'"
  )
})

test_that("single-mode init schedules exactly one save_single at end(sim)", {
  ## The two modes must not overlap: a single-mode run schedules exactly one
  ## save_single at end(sim); a multi-mode run schedules nothing (it does its work
  ## inside init) and so must never write burn maps.
  s <- makeSingleSim()
  before <- SpaDES.core::events(s)
  after <- SpaDES.core::events(
    s$.mods$fireSense_summary$doEvent.fireSense_summary(s, SpaDES.core::start(s), "init")
  )
  added <- after[!after$eventType %in% before$eventType, ]
  expect_identical(NROW(added), 1L)
  expect_identical(added$eventType, "save_single")
  expect_equal(as.numeric(added$eventTime), 2013)
  expect_identical(added$moduleName, "fireSense_summary")

  ## and it is the LAST priority, so it runs after everything else scheduled at
  ## end(sim) -- the burn map it saves must be the final one.
  expect_equal(as.numeric(added$eventPriority), SpaDES.core::.last())
})

## The metrics row: one file per run, read by fireSenseUtils::readELFmetrics() for the national map.
## The fit modules are not in these simulations, so their parameters are set on the simList directly.
withFitModules <- function(s, ignitionFitFilename = "latest") {
  s@params$fireSense_spreadFit <- list(spreadFitFilename = "fireSenseParams_x.rds", fitFileSuffix = "_t")
  s@params$fireSense_ignitionFit <- list(ignitionFitFilename = ignitionFitFilename, fitFileSuffix = "_t")
  s@params$fireSense_ELFs <- list(fireYears = 1986:2024, fitFileSuffix = "_t")
  s@params$fireSense_burn <- list(.runInitialTime = 2011, .runInterval = 1)
  s
}

test_that("the metrics row names the run, the ELF and the fits, then fires and area burned per burn year", {
  burnSummary <- data.table::data.table(igLoc = 1:4, N = 1L, year = c(2011, 2011, 2012, 2013),
                                        areaBurnedHa = c(10, 20, 30, 60))
  s <- makeSingleSim(.ELFind = "4.10")
  s$burnSummary <- burnSummary   # fireSense_burn's columns, not the toy's
  SpaDES.core::P(s, module = "fireSense_summary", ".runName") <- "4.10_NRV_370_1"
  s <- withFitModules(s)
  SpaDES.core::spades(s, debug = FALSE)

  row <- data.table::fread(file.path(SpaDES.core::outputPath(s), "ELFmetrics_4.10_NRV_370_1.csv"),
                           colClasses = list(character = "ELF"))
  expect_identical(row$runName, "4.10_NRV_370_1")
  expect_identical(row$ELF, "4.10")
  expect_identical(row$spreadFitFile, "fireSenseParams_x.rds")                         # named: used as it is
  expect_identical(row$ignitionFitFile, fireSenseUtils::ignitionFitFilenameFor(1986:2024, "_t"))  # "latest": resolved
  expect_equal(row$burnYears, 3)                                                       # 2011, 2012, 2013
  expect_equal(row$firesPerYear, 4 / 3)
  expect_equal(row$areaBurnedHaPerYear, 40)
})

test_that("without the fit modules the ledger files are NA, and the run is named after outputPath", {
  s <- makeSingleSim()
  SpaDES.core::spades(s, debug = FALSE)
  out <- SpaDES.core::outputPath(s)
  row <- data.table::fread(file.path(out, paste0("ELFmetrics_", basename(out), ".csv")))
  expect_identical(row$runName, basename(out))
  expect_true(is.na(row$spreadFitFile) && is.na(row$ignitionFitFile) && is.na(row$ELF))
})

test_that("a hindcast's metrics row has the mean of its rep-years and the observed year", {
  s <- withFitModules(makeSingleSim(.ELFind = "6.2"))
  SpaDES.core::P(s, module = "fireSense_summary", "hindcastYear") <- 2015L
  mod <- s@.xData$.modObjs$fireSense_summary
  mod$hindcastReps <- data.table::data.table(rep = 1:2, year = 2015L, time = c(2015, 2015.001),
                                             ignitions = c(4L, 6L), escapes = c(1L, 3L),
                                             areaBurnedHa = c(100, 300), nFires50 = c(1L, 1L))
  observed <- data.table::data.table(year = 2015L, ignitions = 7L, escapes = 2L, areaBurnedHa = 250,
                                     areaBurnedHaAllCause = 400, nFires50 = 2L, nFires50AllCause = 3L)
  s@current <- data.frame(eventTime = SpaDES.core::start(s), moduleName = "fireSense_summary",
                          eventType = "hindcast", eventPriority = 1)
  s <- s$.mods$fireSense_summary$writeRunMetrics(s, observed = observed)

  out <- SpaDES.core::outputPath(s)
  row <- data.table::fread(file.path(out, paste0("ELFmetrics_", basename(out), ".csv")))
  expect_equal(row$hindcastYear, 2015L)
  expect_equal(row$reps, 2L)
  expect_equal(c(row$sim_ignitions, row$sim_escapes, row$sim_areaBurnedHa, row$sim_nFires50), c(5, 2, 200, 1))
  expect_equal(c(row$obs_ignitions, row$obs_areaBurnedHaAllCause, row$obs_nFires50AllCause), c(7, 400, 3))
  expect_false("burnYears" %in% names(row))
})
