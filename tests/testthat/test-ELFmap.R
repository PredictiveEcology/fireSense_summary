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

writeNrvFixture <- function(path, elf, nTests, nInside, depth = "postprocess/csv", stand = c(nTests, nInside)) {
  d <- file.path(path, elf, depth)
  dir.create(d, recursive = TRUE)
  g <- data.frame(poly = elf, group = c("landscape", "stand"), nTests = c(nTests - stand[1], stand[1]),
                  nInside = c(nInside - stand[2], stand[2]))
  g <- g[g$nTests > 0, ]
  g$pctInside <- 100 * g$nInside / g$nTests
  utils::write.csv(g, file.path(d, "nrvInside.csv"), row.names = FALSE)
}

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
  expect_true(is.na(got$pctInsideStand))
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
  calls <- calls[!duplicated(vapply(calls, function(x) x$metric, ""))]
  expect_length(calls, 2L)
  expect_identical(vapply(calls, function(x) x$metric, ""), c("pctInside", "pctInsideStand"))
  expect_identical(vapply(calls, function(x) x$metricTitle, ""),
                   c("% of tests\ninside the NRV", "% of stand tests\ninside the NRV"))
  expect_identical(calls[[1]]$metricCol, calls[[2]]$metricCol)
  expect_equal(calls[[1]]$metricRange, c(0, 100))
  expect_equal(calls[[2]]$metricRange, c(0, 100))
  expect_identical(calls[[1]]$which, "6.4")
  expect_equal(calls[[1]]$metrics$pctInside, 200 / 3)
  expect_equal(calls[[1]]$metrics$pctInsideStand, 50)
  expect_setequal(basename(list.files(out, "\\.png$", recursive = TRUE)), c("ELFmap.png", "ELFmap_stand.png"))
  tab <- data.table::fread(file.path(out, "ELFmap_insideNRV.csv"))
  expect_identical(tab$nTests, 3L)
  expect_equal(tab$pctInsideStand, 50)
})
