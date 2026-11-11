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
  id[c(6, 7, 8, 9)] <- 2L                                     # starts in the buffer
  id[c(15, 16)] <- 3L                                      # starts in the core (15), burns into the buffer (16)
  pts <- terra::vect(cbind(c(1000, 2000, 8000), rep(5500, 3)), crs = "EPSG:3978")
  pts$YEAR <- 2000L; pts$CAUSE <- c("N", "L", "N"); pts$SIZE_HA <- c(10, 60, 80)
  ## the observed fires, as the simulated ones: B starts in the core (centroid x 3500) and burns into the buffer
  ## (5 pixels, 500 ha); A starts and burns in the buffer (3 pixels, 300 ha)
  poly <- rbind(
    terra::vect("POLYGON ((1200 7200, 5800 7200, 5800 7800, 1200 7800, 1200 7200))", crs = "EPSG:3978"),
    terra::vect("POLYGON ((6200 2200, 8800 2200, 8800 2800, 6200 2800, 6200 2200))", crs = "EPSG:3978"))
  poly$YEAR <- 2000L; poly$FIRECAUS <- "Natural"; poly$POLY_HA <- c(500, 300)
  list(rasterToMatchELF = core, rstAnnualBurnID = id,
       ignitionsAndEscapes = data.table::data.table(pixelID = c(1L, 2L, 6L, 15L),
                                                       escaped = c(TRUE, FALSE, TRUE, FALSE)),
       fireSense_IgAndEscapeProbRas = toyIgAndEscapeProb(),
       burnMap = terra::setValues(terra::rast(core), 1L), burnSummary = toyBurnSummary(),
       ignitionFirePoints = pts, firePolys = list(year2000 = poly),
       flammableRTM = terra::setValues(terra::rast(core), 1L))      # core and buffer
}

makeHindcastSim <- function(hindcastYear = 2000L) {
  out <- withr::local_tempdir(.local_envir = parent.frame())
  SpaDES.core::simInit(
    times = list(start = 2000, end = 2000.0095),
    params = list(fireSense_summary = list(mode = "single", hindcastYear = hindcastYear, hindcastInterval = 0.001,
                                           escapeSizeHa = 250)),
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
  expect_equal(reps$time, 2000 + (0:9) / 1000)             # the rep time is kept for readers that used it
  expect_true(all(reps$ignitions == 3L))                   # pixels 1, 2 and 15 are in the core; 6 is a buffer start
  expect_true(all(reps$escapes == 1L))
  expect_true(all(reps$areaBurnedHa == 500))               # fire 1 (300) and fire 3 (200, into the buffer); not fire 2
  expect_true(all(reps$nFires50 == 1L))                    # escapeSizeHa = 250

  sizes <- data.table::fread(file.path(out, "hindcast_sizes.csv"))
  expect_identical(sizes$rep, rep(1:10, each = 2))
  expect_setequal(sizes$ha, c(300, 200))                   # whole extents; the buffer-start fire is absent

  obs <- data.table::fread(file.path(out, "hindcast_observed.csv"))
  expect_identical(obs$year, 2000L)
  expect_identical(obs$ignitions, 3L)                      # two core NFDB points, and the start of the unmatched escape
  expect_identical(obs$escapes, 1L)
  expect_equal(obs$areaBurnedHa, 500)                      # fire B at its whole extent; buffer-start fire A is not counted
  expect_identical(obs$nFires50, 1L)
  expect_true(file.exists(file.path(out, "hindcast_observedSizes.csv")))
  expect_true(file.exists(file.path(out, "hindcast_observedPolys.gpkg")))
  expect_true(file.exists(file.path(out, "hindcast_burnMap.tif")))
  expect_setequal(grep("hindcast_burnID_rep", list.files(out), value = TRUE),
                  sprintf("hindcast_burnID_rep%02d.tif", 1:10))
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
  expect_gte(NROW(pts), 2)                         # the natural points in the core, as ignitions; none in the buffer
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
                                           simOutputPath = root, studyAreaName = "toy", .plots = "png")),
    modules = "fireSense_summary", objects = list(),
    paths = list(modulePath = modulePath, outputPath = root, cachePath = file.path(root, "cache"),
                 inputPath = file.path(root, "in")))
  SpaDES.core::spades(m, debug = FALSE)
  figs <- list.files(root, "^hindcast_(distributions|sizes|map)[.]png$", recursive = TRUE)
  expect_equal(length(figs), 3L)
})

test_that("a buffer-start fire is in neither the observed nor the simulated area; a core-start fire counts whole", {
  s <- makeHindcastSim()
  out <- SpaDES.core::outputPath(s)
  SpaDES.core::spades(s, debug = FALSE)
  reps <- data.table::fread(file.path(out, "hindcast_reps.csv"))
  sizes <- data.table::fread(file.path(out, "hindcast_sizes.csv"))
  expect_false(400 %in% sizes$ha)                           # fire 2 (400 ha) started in the buffer
  expect_true(all(reps$areaBurnedHa == sum(sizes$ha[sizes$rep == 1L])))
  expect_true(200 %in% sizes$ha)                            # fire 3 starts in the core: 100 ha + 100 ha buffer
  obsSizes <- data.table::fread(file.path(out, "hindcast_observedSizes.csv"))
  expect_equal(obsSizes$ha[obsSizes$cause == "natural"], 500)  # B whole; A (300 ha, buffer start) absent
})

test_that("escapeSizeHa and fireCauses must agree with the other modules", {
  other <- withr::local_tempdir()
  suppressMessages(SpaDES.core::newModule("otherEscape", other, open = FALSE))
  f <- file.path(other, "otherEscape", "otherEscape.R")
  x <- readLines(f)
  at <- grep("^  parameters = bindrows", x)
  stopifnot(length(at) == 1L)
  x <- append(x, '    defineParameter("escapeSizeHa", "numeric", 99, 0, NA, "x"),', at)
  writeLines(x, f)
  s <- makeHindcastSim()
  expect_error(
    SpaDES.core::simInit(
      times = list(start = 2000, end = 2000.0095),
      params = list(fireSense_summary = list(mode = "single", escapeSizeHa = 50), otherEscape = list(escapeSizeHa = 99)),
      modules = c("fireSense_summary", "otherEscape"), objects = toyHindcastObjects(),
      paths = list(modulePath = c(modulePath, other), outputPath = withr::local_tempdir())),
    "escapeSizeHa")
})

test_that("hindcast stops when flammableRTM, the core and buffer burnable pixels, is missing", {
  s <- makeHindcastSim()
  s$flammableRTM <- NULL
  expect_error(SpaDES.core::spades(s, debug = FALSE), "flammableRTM")
})
