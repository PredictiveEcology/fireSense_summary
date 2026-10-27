## Hindcast: a job for year Y runs 10 rep-years as DECIMAL times Y, Y + 0.001, ... (so every rep floors to Y).
## These run the real events through spades() on a 10 x 10 toy grid whose left five columns are the ELF core.

toyHindcastCore <- function() {
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10000, ymin = 0, ymax = 10000, crs = "EPSG:3978")
  terra::setValues(r, ifelse(terra::colFromCell(r, seq_len(terra::ncell(r))) <= 5, 2, NA))
}

## the coarse (5 x 5, 2 km) grid of the ignition fit, not the burn grid
toyIgAndEscapeProb <- function() {
  r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 10000, ymin = 0, ymax = 10000, crs = "EPSG:3978")
  ig <- terra::setValues(r, seq_len(25) / 100)
  es <- terra::setValues(r, seq_len(25) / 1000)
  stats::setNames(c(ig, es), c("ignitionProb", "escapeProb"))
}

toyHindcastObjects <- function() {
  core <- toyHindcastCore()
  id <- terra::setValues(terra::rast(core), NA_integer_)
  id[c(1, 2, 3)] <- 1L                                     # 300 ha in the core
  id[c(6, 7)] <- 2L                                        # outside the core
  pts <- terra::vect(cbind(c(1000, 2000, 8000), rep(5500, 3)), crs = "EPSG:3978")
  pts$YEAR <- 2000L; pts$CAUSE <- c("N", "L", "N"); pts$SIZE_HA <- c(10, 60, 80)
  poly <- terra::vect("POLYGON ((500 500, 1500 500, 1500 1500, 500 1500, 500 500))", crs = "EPSG:3978")
  poly$YEAR <- 2000L; poly$FIRECAUS <- "Natural"
  list(rasterToMatchELF = core, rstAnnualBurnID = id,
       ignitionsAndEscapes = data.table::data.table(pixelID = c(1L, 2L, 6L), escaped = c(TRUE, FALSE, TRUE)),
       fireSense_IgAndEscapeProbRas = toyIgAndEscapeProb(),
       burnMap = terra::setValues(terra::rast(core), 1L), burnSummary = toyBurnSummary(),
       ignitionFirePoints = pts, firePolys = list(year2000 = poly))
}

makeHindcastSim <- function(hindcastYear = 2000L) {
  out <- withr::local_tempdir(.local_envir = parent.frame())
  SpaDES.core::simInit(
    times = list(start = 2000, end = 2000.0095),
    params = list(fireSense_summary = list(mode = "single", hindcastYear = hindcastYear, hindcastInterval = 0.001,
                                           escapeSizeHa = 150)),
    modules = "fireSense_summary", objects = toyHindcastObjects(),
    paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"),
                 inputPath = file.path(out, "in")))
}

test_that("hindcast single mode records ten decimal-year reps and the observed year", {
  s <- makeHindcastSim()
  out <- SpaDES.core::outputPath(s)
  SpaDES.core::spades(s, debug = FALSE)

  reps <- data.table::fread(file.path(out, "hindcast_reps.csv"))
  expect_identical(reps$rep, 1:10)
  expect_type(reps$year, "integer")
  expect_true(all(reps$year == 2000L))                     # the hindcast year, not the rep time
  expect_identical(reps$paramSet, rep(NA, 10))                 # no fireSense_SpreadParamSet: NA, not an error
  expect_equal(reps$time, 2000 + (0:9) / 1000)             # the rep time is kept for readers that used it
  expect_true(all(reps$ignitions == 2L))                   # pixels 1 and 2 are in the core, 6 is not
  expect_true(all(reps$escapes == 1L))
  expect_true(all(reps$areaBurnedHa == 300))
  expect_true(all(reps$nFires50 == 1L))                    # escapeSizeHa = 150

  sizes <- data.table::fread(file.path(out, "hindcast_sizes.csv"))
  expect_identical(sizes$rep, 1:10)
  expect_identical(sizes$paramSet, rep(NA, 10))
  expect_true(all(sizes$ha == 300))

  obs <- data.table::fread(file.path(out, "hindcast_observed.csv"))
  expect_identical(obs$year, 2000L)
  expect_identical(obs$ignitions, 2L)
  expect_identical(obs$escapes, 0L)                        # the 60 ha point is below escapeSizeHa = 150
  expect_equal(obs$areaBurnedHa, 100, tolerance = 0.03)
  expect_true(file.exists(file.path(out, "hindcast_observedSizes.csv")))
  expect_true(file.exists(file.path(out, "hindcast_observedPolys.gpkg")))
  expect_true(file.exists(file.path(out, "hindcast_burnMap.tif")))
  expect_setequal(grep("hindcast_burnID_rep", list.files(out), value = TRUE),
                  sprintf("hindcast_burnID_rep%02d.tif", 1:10))
})

test_that("hindcast tables carry the parameter set fireSense_spreadPredict used", {
  s <- makeHindcastSim()
  out <- SpaDES.core::outputPath(s)
  s$fireSense_SpreadParamSet <- c(A = 3L)                  # named by ELF when spreadPredict has several
  SpaDES.core::spades(s, debug = FALSE)
  reps <- data.table::fread(file.path(out, "hindcast_reps.csv"))
  sizes <- data.table::fread(file.path(out, "hindcast_sizes.csv"))
  expect_identical(reps$paramSet, rep(3L, 10))
  expect_identical(sizes$paramSet, rep(3L, 10))
  expect_identical(names(reps)[1:2], c("rep", "paramSet"))
})

test_that("hindcast writes each rep's ignition and escape probability maps on the ignition grid", {
  s <- makeHindcastSim()
  out <- SpaDES.core::outputPath(s)
  SpaDES.core::spades(s, debug = FALSE)
  for (f in c("ignitionProb", "escapeProb")) {
    files <- sprintf("hindcast_%s_rep%02d.tif", f, 1:10)
    expect_setequal(grep(paste0("hindcast_", f, "_rep"), list.files(out), value = TRUE), files)
    r <- terra::rast(file.path(out, files[[3]]))
    expected <- toyIgAndEscapeProb()[[f]]
    expect_equal(terra::values(r, mat = FALSE), terra::values(expected, mat = FALSE))
    expect_true(terra::compareGeom(r, expected, stopOnError = FALSE))
    expect_identical(terra::crs(r), terra::crs(expected))
  }
})

test_that("hindcast writes the year's observed natural ignition points in the core", {
  s <- makeHindcastSim()
  out <- SpaDES.core::outputPath(s)
  SpaDES.core::spades(s, debug = FALSE)
  pts <- terra::vect(file.path(out, "hindcast_observedPoints.gpkg"))
  expect_equal(NROW(pts), 2)                          # the two natural points in the core, as ignitions
  expect_true(all(pts$YEAR == 2000L & pts$CAUSE %in% c("L", "N")))
  expect_identical(terra::crs(pts), terra::crs(toyHindcastCore()))
})

test_that("hindcast events are scheduled only when hindcastYear is set", {
  s <- makeHindcastSim(hindcastYear = NA_integer_)
  s <- s$.mods$fireSense_summary$doEvent.fireSense_summary(s, SpaDES.core::start(s), "init")
  expect_false(any(grepl("hindcast", SpaDES.core::events(s)$eventType)))

  h <- makeHindcastSim()
  h <- h$.mods$fireSense_summary$doEvent.fireSense_summary(h, SpaDES.core::start(h), "init")
  ev <- SpaDES.core::events(h)
  expect_setequal(ev$eventType[grepl("hindcast", ev$eventType)], c("hindcastRecord", "hindcast"))
  expect_gt(ev$eventPriority[ev$eventType == "hindcast"], ev$eventPriority[ev$eventType == "hindcastRecord"])
})

test_that("hindcast stops at the first rep-year when an input is missing", {
  s <- makeHindcastSim()
  s$firePolys <- NULL
  expect_error(SpaDES.core::spades(s, debug = FALSE), "firePolys")
})

test_that("hindcast stops when the ignition probability maps are missing", {
  s <- makeHindcastSim()
  s$fireSense_IgAndEscapeProbRas <- NULL
  expect_error(SpaDES.core::spades(s, debug = FALSE), "fireSense_IgAndEscapeProbRas")
})

test_that("hindcast stops when ignitionsAndEscapes is missing, rather than counting NA ignitions", {
  s <- makeHindcastSim()
  s$ignitionsAndEscapes <- NULL
  expect_error(SpaDES.core::spades(s, debug = FALSE), "ignitionsAndEscapes")
})

test_that("hindcast multi mode pools the parameter-set folders into three figures", {
  skip_if_not_installed("ggplot2")
  root <- withr::local_tempdir()
  for (k in 1:2) {
    d <- file.path(root, paste0("rep", k))
    s <- makeHindcastSim()
    SpaDES.core::spades(s, debug = FALSE)
    dir.create(d)
    file.copy(list.files(SpaDES.core::outputPath(s), "hindcast_", full.names = TRUE), d)
  }
  m <- SpaDES.core::simInit(
    times = list(start = 2000, end = 2000.0095),
    params = list(fireSense_summary = list(mode = "multi", hindcastYear = 2000L, reps = 1:2,
                                           simOutputPath = root, .studyAreaName = "toy", .plots = "png")),
    modules = "fireSense_summary", objects = list(),
    paths = list(modulePath = modulePath, outputPath = root, cachePath = file.path(root, "cache"),
                 inputPath = file.path(root, "in")))
  SpaDES.core::spades(m, debug = FALSE)
  figs <- list.files(root, "^hindcast_(distributions|sizes|map)[.]png$", recursive = TRUE)
  expect_equal(length(figs), 3L)
})
