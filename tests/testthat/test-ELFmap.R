## mode = 'ELFmap' draws only the map of the ELFs, with the reported ones filled green.

test_that("ELFmap mode draws the ELF map with the reported ELFs filled, and nothing else", {
  out <- withr::local_tempdir()
  got <- NULL
  testthat::local_mocked_bindings(
    plotELFs = function(...) { got <<- list(...); graphics::plot(1) },
    .package = "fireSenseUtils"
  )
  SpaDES.core::simInitAndSpades(
    times = list(start = 0, end = 1),
    params = list(fireSense_summary = list(mode = "ELFmap", reportedELFs = c("6.4", "9.3"),
                                           simOutputPath = out, .plots = "png")),
    modules = "fireSense_summary",
    objects = list(ELFs = list(rasWhole = list(), rasCentered = list())),
    paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"),
                 inputPath = file.path(out, "in"))
  )
  expect_identical(got$which, c("6.4", "9.3"))
  expect_identical(got$fill, "green")
  expect_identical(nrow(got$plan), 0L) ## the ELFs are already merged
  expect_identical(basename(list.files(out, "\\.png$", recursive = TRUE)), "ELFmap.png")
})

test_that("ELFmap mode without ELFs stops, naming them", {
  out <- withr::local_tempdir()
  expect_error(SpaDES.core::simInitAndSpades(
    times = list(start = 0, end = 1),
    params = list(fireSense_summary = list(mode = "ELFmap", reportedELFs = "6.4", simOutputPath = out)),
    modules = "fireSense_summary",
    paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"),
                 inputPath = file.path(out, "in"))
  ), "needs `ELFs`")
})

## nrvComparisonPath: each reported ELF coloured by the percentage of its NRV tests inside the natural range,
## read from NRV_summary's `nrvInside.csv` (columns poly, group, nTests, nInside, pctInside; one row per group).
## `nTests`/`nInside` are of the whole ELF; `stand` says how many of them are stand-level tests.

test_that("nrvInsideTable reads the counts from nrvInside.csv, wherever below the ELF's folder it is", {
  nrv <- withr::local_tempdir()
  writeNrvFixture(nrv, "6.4", nTests = 7L, nInside = 2L, stand = c(4L, 1L))
  writeNrvFixture(nrv, "9.3", nTests = 3L, nInside = 1L, depth = "a/b/csv", stand = c(3L, 1L))
  ## no lm_/st_nrvComparison.csv anywhere: the count is not recomputed from them
  got <- moduleFun("nrvInsideTable")(c("6.4", "9.3"), nrv)
  expect_identical(got$ELF, c("6.4", "9.3"))
  expect_identical(got$nTests, c(7L, 3L))
  expect_identical(got$nInside, c(2L, 1L))
  expect_equal(got$pctInside, c(100 * 2 / 7, 100 / 3))
  expect_equal(got$pctInsideStand, c(25, 100 / 3)) ## the stand group's rows only
})

test_that("nrvInsideTable gives no stand percentage for an ELF whose file has no stand group", {
  nrv <- withr::local_tempdir()
  writeNrvFixture(nrv, "6.4", nTests = 4L, nInside = 3L, stand = c(0L, 0L))
  got <- moduleFun("nrvInsideTable")("6.4", nrv)
  expect_equal(got$pctInside, 75)
  expect_identical(got$pctInsideStand, NA_real_) ## NA, not NaN (0 / 0)
})

## the stand-level comparison NRV_summary writes: one row per metric, with its position and whether it is a test
writeStFixture <- function(path, elf, metric, label = metric, position, tested = TRUE, depth = "postprocess/csv/st/ELF") {
  d <- file.path(path, elf, depth)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(poly = elf, metric = metric, label = label, position = position, tested = tested),
                   file.path(d, "st_nrvComparison.csv"), row.names = FALSE)
}

test_that("nrvStandPositions codes the position of each tested stand-level metric, below -1, inside 0, above 1", {
  nrv <- withr::local_tempdir()
  writeStFixture(nrv, "6.4", c("age_Old", "lead_A", "lead_Zero"), c("Old forest", "Pine", "Zero"),
                 c("below", "inside", "above"), tested = c(TRUE, TRUE, FALSE))
  writeStFixture(nrv, "9.3", "age_Old", "Old forest", "above")
  got <- moduleFun("nrvStandPositions")(c("6.4", "9.3"), nrv)
  expect_identical(got$ELF, c("6.4", "6.4", "9.3"))
  expect_identical(got$metric, c("age_Old", "lead_A", "age_Old")) ## lead_Zero is not a test
  expect_identical(got$label, c("Old forest", "Pine", "Old forest"))
  expect_equal(got$code, c(-1, 0, 1))
})

test_that("nrvStandPositions warns about an ELF without st_nrvComparison.csv and leaves it out", {
  nrv <- withr::local_tempdir()
  writeStFixture(nrv, "6.4", "age_Old", position = "inside")
  dir.create(file.path(nrv, "9.3"))
  expect_warning(got <- moduleFun("nrvStandPositions")(c("6.4", "9.3"), nrv), "9.3")
  expect_identical(got$ELF, "6.4")
})

test_that("nrvInsideTable warns about an ELF without nrvInside.csv and leaves it out", {
  nrv <- withr::local_tempdir()
  writeNrvFixture(nrv, "6.4", nTests = 2L, nInside = 2L)
  dir.create(file.path(nrv, "9.3"))
  expect_warning(got <- moduleFun("nrvInsideTable")(c("6.4", "9.3"), nrv), "9.3")
  expect_identical(got$ELF, "6.4")
  expect_equal(got$pctInside, 100)
})

test_that("ELFmap mode with nrvComparisonPath draws the all-tests map and the stand-only map, one scale", {
  out <- withr::local_tempdir(); nrv <- withr::local_tempdir()
  writeNrvFixture(nrv, "6.4", nTests = 3L, nInside = 2L, stand = c(2L, 1L))
  writeStFixture(nrv, "6.4", c("age_Old", "lead_A"), c("Old forest", "Pine"), c("below", "above"))
  calls <- list()
  testthat::local_mocked_bindings(
    plotELFs = function(...) { calls[[length(calls) + 1L]] <<- list(...); graphics::plot(1) },
    .package = "fireSenseUtils"
  )
  SpaDES.core::simInitAndSpades(
    times = list(start = 0, end = 1),
    params = list(fireSense_summary = list(mode = "ELFmap", reportedELFs = "6.4", nrvComparisonPath = nrv,
                                           simOutputPath = out, .plots = "png")),
    modules = "fireSense_summary",
    objects = list(ELFs = list(rasWhole = list(), rasCentered = list())),
    paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"),
                 inputPath = file.path(out, "in"))
  )
  ## Plots() may call the function more than once per figure; one call per metric is enough
  st <- calls[vapply(calls, function(x) identical(x$metric, "code"), NA)]
  calls <- calls[!vapply(calls, function(x) identical(x$metric, "code"), NA)]
  calls <- calls[!duplicated(vapply(calls, function(x) x$metric, ""))]
  expect_length(calls, 2L)
  expect_identical(vapply(calls, function(x) x$metric, ""), c("pctInside", "pctInsideStand"))
  expect_identical(vapply(calls, function(x) x$metricTitle, ""),
                   c("% of tests\ninside the NRV", "% of stand tests\ninside the NRV"))
  expect_identical(calls[[1]]$metricCol, calls[[2]]$metricCol)
  expect_equal(calls[[1]]$metricRange, c(0, 100))
  expect_equal(calls[[2]]$metricRange, c(0, 100))
  ## one map per stand-level metric: the position as a code, three colours, the metric's label as title
  st <- st[!duplicated(vapply(st, function(x) x$metricTitle, ""))]
  key <- "\n-1 below \u00b7 0 inside \u00b7 1 above" ## the legend has no labels of its own
  expect_identical(vapply(st, function(x) x$metricTitle, ""), paste0(c("Old forest", "Pine"), key))
  expect_equal(vapply(st, function(x) x$metrics$code, 0), c(-1, 1))
  expect_length(st[[1]]$metricCol, 3L)
  expect_equal(st[[1]]$metricRange, c(-1, 1))
  expect_true(is.na(st[[1]]$fill)) ## an ELF where the metric is not a test is not filled
  expect_identical(calls[[1]]$which, "6.4")
  expect_equal(calls[[1]]$metrics$pctInside, 200 / 3)
  expect_equal(calls[[1]]$metrics$pctInsideStand, 50)
  expect_setequal(basename(list.files(out, "\\.png$", recursive = TRUE)), c("ELFmap.png", "ELFmap_stand.png", "ELFmap_st_age_Old.png", "ELFmap_st_lead_A.png"))
  tab <- data.table::fread(file.path(out, "ELFmap_insideNRV.csv"))
  expect_identical(tab$nTests, 3L)
  expect_equal(tab$pctInsideStand, 50)
})

## ELFs with burn maps but no nrvInside.csv (reports made before NRV_summary#32): the burn-probability maps
## do not need it.

test_that("nrvInsideTable and nrvStandPositions keep their columns when no ELF has the file", {
  nrv <- withr::local_tempdir()
  dir.create(file.path(nrv, "9.3"))
  suppressWarnings(inside <- moduleFun("nrvInsideTable")("9.3", nrv))
  expect_identical(nrow(inside), 0L)
  expect_identical(names(inside), c("ELF", "nTests", "nInside", "pctInside", "pctInsideStand"))
  suppressWarnings(pos <- moduleFun("nrvStandPositions")("9.3", nrv))
  expect_identical(nrow(pos), 0L)
  expect_identical(names(pos), c("ELF", "metric", "label", "code"))
})

test_that("ELFmap mode with nrvComparisonPath but no nrvInside.csv still makes the burn-probability maps", {
  out <- withr::local_tempdir(); nrv <- withr::local_tempdir()
  dir.create(file.path(nrv, "6.4"))
  writeBurnMap(out, "6.4", "meanAnnualCumulBurnMap")
  expect_warning(seen <- runELFmap(out, params = list(reportedELFs = "6.4", nrvComparisonPath = nrv)),
                 "no ELF has an nrvInside.csv")
  expect_identical(names(seen$files), "NRVburnProbability")
  expect_false(file.exists(file.path(out, "ELFmap_insideNRV.csv")))
})

test_that("ELFmap mode with nrvInside.csv for only some ELFs maps those and still makes the burn-probability maps", {
  out <- withr::local_tempdir(); nrv <- withr::local_tempdir()
  writeNrvFixture(nrv, "6.4", nTests = 3L, nInside = 2L)
  dir.create(file.path(nrv, "9.3"))
  writeBurnMap(out, "6.4", "meanAnnualCumulBurnMap"); writeBurnMap(out, "9.3", "meanAnnualCumulBurnMap")
  suppressWarnings(seen <- runELFmap(out, params = list(nrvComparisonPath = nrv)))
  expect_identical(names(seen$files$NRVburnProbability), c("6.4", "9.3"))
  expect_identical(data.table::fread(file.path(out, "ELFmap_insideNRV.csv"), colClasses = list(character = "ELF"))$ELF, "6.4")
})
