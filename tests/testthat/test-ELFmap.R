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

## nrvComparisonPath: each reported ELF coloured by the percentage of its NRV tests inside the natural range.

writeNrvFixture <- function(path, elf, lm, st, depth = "postprocess/csv") {
  nrvCols <- function(position, metric, current = 1, hi = 2)
    data.frame(poly = elf, metric = metric, lo = 0, hi = hi, mid = 1, current = current, position = position)
  d <- file.path(path, elf, depth)
  dir.create(file.path(d, "lm"), recursive = TRUE); dir.create(file.path(d, "st"), recursive = TRUE)
  utils::write.csv(nrvCols(lm, paste0("lm", seq_along(lm))), file.path(d, "lm", "lm_nrvComparison.csv"), row.names = FALSE)
  utils::write.csv(do.call(rbind, Map(nrvCols, st$position, st$metric, st$current, st$hi)),
                   file.path(d, "st", "st_nrvComparison.csv"), row.names = FALSE)
}

nrvInsideFun <- function() {
  out <- withr::local_tempdir(.local_envir = parent.frame())
  SpaDES.core::simInit(times = list(start = 0, end = 1),
                       params = list(fireSense_summary = list(mode = "ELFmap", simOutputPath = out)),
                       modules = "fireSense_summary",
                       paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"),
                                    inputPath = file.path(out, "in")))$.mods$fireSense_summary$nrvInsideTable
}

test_that("nrvInsideTable counts the tests inside the NRV, dropping lead_ types that lead nowhere", {
  nrv <- withr::local_tempdir()
  ## lm: 2 inside of 3; st: 1 inside of 2 kept, plus a lead_ row with current == 0 and hi == 0 (dropped),
  ## and a lead_ row with current == 0 but hi > 0 (kept, it led somewhere in the NRV years)
  writeNrvFixture(nrv, "6.4", lm = c("inside", "inside", "above"),
                  st = data.frame(position = c("inside", "below", "below", "below"),
                                  metric = c("age_Old", "age_Young", "lead_Abie_bal", "lead_Pice_mar"),
                                  current = c(1, 1, 0, 0), hi = c(2, 2, 0, 3)))
  writeNrvFixture(nrv, "9.3", lm = c("below", "below"), st = data.frame(position = "inside", metric = "age_Old",
                                                                         current = 1, hi = 2), depth = "a/b/csv")
  got <- nrvInsideFun()(c("6.4", "9.3"), nrv)
  expect_identical(got$ELF, c("6.4", "9.3"))
  expect_identical(got$nTests, c(6L, 3L))
  expect_identical(got$nInside, c(3L, 1L))
  expect_equal(got$pctInside, c(50, 100 / 3))
})

test_that("nrvInsideTable warns about an ELF missing a table and leaves it out", {
  nrv <- withr::local_tempdir()
  writeNrvFixture(nrv, "6.4", lm = "inside", st = data.frame(position = "inside", metric = "age_Old", current = 1, hi = 2))
  dir.create(file.path(nrv, "9.3"))
  expect_warning(got <- nrvInsideFun()(c("6.4", "9.3"), nrv), "9.3")
  expect_identical(got$ELF, "6.4")
  expect_equal(got$pctInside, 100)
})

test_that("ELFmap mode with nrvComparisonPath colours the ELFs by % inside and writes the table", {
  out <- withr::local_tempdir(); nrv <- withr::local_tempdir()
  writeNrvFixture(nrv, "6.4", lm = c("inside", "above"), st = data.frame(position = "inside", metric = "age_Old", current = 1, hi = 2))
  got <- NULL
  testthat::local_mocked_bindings(
    plotELFs = function(...) { got <<- list(...); graphics::plot(1) },
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
  expect_identical(got$which, "6.4")
  expect_identical(got$metric, "pctInside")
  expect_equal(got$metricRange, c(0, 100))
  expect_equal(got$metrics$pctInside, 200 / 3)
  tab <- data.table::fread(file.path(out, "ELFmap_insideNRV.csv"))
  expect_identical(tab$nTests, 3L)
})
