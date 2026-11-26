## Mode 'variancePartition': the real event through spades(), on a toy fit whose answer is known. Climate is one value per
## year and fuel differs only between pixels, so between years the signal is all climate and within years none of it is.

toyPartitionObjects <- function() {
  set.seed(3)
  nPix <- 50L
  years <- 2001:2005
  annual <- stats::setNames(lapply(seq_along(years), function(i)
    data.table::data.table(pixelID = seq_len(nPix), CMD = 10 * i)), years)
  nonAnnual <- list(`2000` = data.table::data.table(pixelID = seq_len(nPix),
                                                    dom_agb_Pice_mar = fireSenseUtils::logMinB(runif(nPix, 0, 8000)),
                                                    youngAge = as.integer(seq_len(nPix) %% 5 == 0)))
  row <- data.frame(polygonID = c("4.1", "4.2"))
  row$params <- rep(list(data.frame(maxAsymptote = 0.25, hillSlope1 = 1, inflectionPoint1 = 1, CMD = 3,
                                    dom_agb_Pice_mar = 2, youngAge = -1)), 2)
  row$covMinMax_spread <- rep(list(data.table::data.table(CMD = c(0, 100), dom_agb_Pice_mar = fireSenseUtils::fuelLinearRange,
                                                          youngAge = c(0, 1))), 2)
  list(studyAreaWithSpreadParams = row, fireSense_annualSpreadFitCovariates = annual,
       fireSense_nonAnnualSpreadFitCovariates = nonAnnual, .ELFind = "4.1")
}

test_that("mode 'variancePartition' writes the partition of the run's ELF and its figure", {
  out <- withr::local_tempdir()
  s <- SpaDES.core::simInit(
    times = list(start = 2005, end = 2005),
    params = list(fireSense_summary = list(mode = "variancePartition", .plots = "png")),
    modules = "fireSense_summary", objects = toyPartitionObjects(),
    paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"),
                 inputPath = file.path(out, "in")))
  s <- SpaDES.core::spades(s, debug = FALSE)
  f <- file.path(out, "variancePartition.csv")
  expect_true(file.exists(f))
  vp <- data.table::fread(f)
  expect_identical(unique(vp$ELF), 4.1)                   # the run's ELF only, not the other row
  expect_identical(unique(vp$method), "model")
  share <- function(signal, part) vp$share[vp$signal == signal & vp$part == part]
  expect_equal(share("between", "climate"), 1)
  expect_equal(share("within", "climate"), 0)
  expect_equal(share("within", "fuel") + share("within", "youngAge"), 1)
  expect_true(f %in% SpaDES.core::outputs(s)$file)
  expect_length(list.files(SpaDES.core::figurePath(s), "^variancePartition_4.1_model", recursive = TRUE), 1L)
})

test_that("with commonalityLedgers, the four fits of the ELF give the commonality partition too", {
  ## each fit's in-sample annual areas and fire-size error, as fireSense_spreadFit stores them in its ledger row
  obs <- c(10, 200, 50, 3000, 20, 800, 5, 100)
  fit <- function(sim, fireRMSE) {
    r <- toyPartitionObjects()$studyAreaWithSpreadParams
    r[[fireSenseUtils::spreadFitInSampleSizesTxt]] <- rep(list(list(
      sizes = data.table::data.table(fireRMSE = fireRMSE),
      byYear = data.table::data.table(yr = 2001:2008, obs = obs, simMedian = sim))), 2)
    r
  }
  ledgers <- list(fNone = fit(rep(100, 8), 1), fClim = fit(obs, 0.8), fFuel = fit(rep(100, 8), 0.5),
                  fBoth = fit(obs, 0.4))
  testthat::local_mocked_bindings(readSpreadFitLedger = function(spreadFitFilename, ...) ledgers[[spreadFitFilename]],
                                  .package = "fireSenseUtils")
  out <- withr::local_tempdir()
  objs <- toyPartitionObjects()
  objs$studyArea <- sf::st_as_sf(terra::vect("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", crs = "EPSG:3978"))
  s <- SpaDES.core::simInit(
    times = list(start = 2005, end = 2005),
    params = list(fireSense_summary = list(mode = "variancePartition", .plots = NA,
                                           commonalityLedgers = c(none = "fNone", climate = "fClim", fuel = "fFuel",
                                                                  climateFuel = "fBoth"))),
    modules = "fireSense_summary", objects = objs,
    paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"),
                 inputPath = file.path(out, "in")))
  SpaDES.core::spades(s, debug = FALSE)
  vp <- data.table::fread(file.path(out, "variancePartition.csv"))
  cp <- vp[vp$method == "commonality", ]
  share <- function(signal, part) cp$share[cp$signal == signal & cp$part == part]
  ## between years climate explains it all and fuel nothing: all unique climate
  expect_equal(share("between", "climate"), 1, tolerance = 1e-9)
  expect_equal(share("between", "fuel"), 0)
  expect_equal(share("within", "unexplained"), 0.4^2)
})

test_that("mode 'variancePartition' stops, naming them, without the fit's covariates", {
  out <- withr::local_tempdir()
  objs <- toyPartitionObjects()[c("studyAreaWithSpreadParams", ".ELFind")]
  s <- SpaDES.core::simInit(
    times = list(start = 2005, end = 2005),
    params = list(fireSense_summary = list(mode = "variancePartition", .plots = NA)),
    modules = "fireSense_summary", objects = objs,
    paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"),
                 inputPath = file.path(out, "in")))
  expect_error(SpaDES.core::spades(s, debug = FALSE), "fireSense_annualSpreadFitCovariates")
})

test_that("ledgerRowsOf() picks the rows of the ELFs, and stops on an ELF without exactly one", {
  out <- withr::local_tempdir()
  ledgerRowsOf <- SpaDES.core::simInit(
    times = list(start = 1, end = 1), modules = moduleName,
    paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"),
                 inputPath = file.path(out, "in")))$.mods[[moduleName]]$ledgerRowsOf
  rows <- data.frame(polygonID = c("4.1", "4.2"), x = 1:2)
  expect_identical(ledgerRowsOf(rows, "4.2")$x, 2L)
  expect_identical(nrow(ledgerRowsOf(rows, NULL)), 2L)
  expect_error(ledgerRowsOf(rows, "9.9"), "9.9 has 0")
})
