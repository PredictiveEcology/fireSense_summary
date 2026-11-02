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
