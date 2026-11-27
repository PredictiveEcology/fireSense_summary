## mode = 'ELFmap' also writes the national burn-probability maps from the reported ELFs' mean annual burn maps.

test_that("ELFmap mode mosaics the simulated and the observed mean annual burn maps of the reported ELFs", {
  out <- withr::local_tempdir()
  sim <- c(`6.4` = writeBurnMap(out, "6.4", "meanAnnualCumulBurnMap"), `9.3` = writeBurnMap(out, "9.3", "meanAnnualCumulBurnMap"))
  hist <- c(`6.4` = writeBurnMap(out, "6.4", "meanAnnualCumulBurnMapHistoric"), `9.3` = writeBurnMap(out, "9.3", "meanAnnualCumulBurnMapHistoric"))
  seen <- runELFmap(out)
  expect_identical(names(seen$files), c("NRVburnProbability", "NRVburnProbabilityHistoric"))
  expect_identical(seen$files$NRVburnProbability, sim)
  expect_identical(seen$files$NRVburnProbabilityHistoric, hist)
  expect_identical(seen$filename, c(NRVburnProbability = file.path(out, "NRVburnProbability.tif"),
                                    NRVburnProbabilityHistoric = file.path(out, "NRVburnProbabilityHistoric.tif")))
  expect_named(seen$ELFs$rasWhole, "6.4")
})

test_that("an ELF without the maps is warned about and left out; with none at all nothing is mosaicked", {
  out <- withr::local_tempdir()
  writeBurnMap(out, "6.4", "meanAnnualCumulBurnMap")
  expect_warning(seen <- runELFmap(out), "ELF 9.3: no mean annual burn map")
  expect_identical(names(seen$files$NRVburnProbability), "6.4")
  expect_identical(names(seen$files), "NRVburnProbability")     # no observed map of any ELF: no observed product
  out2 <- withr::local_tempdir()
  expect_warning(seen2 <- runELFmap(out2), "ELF 6.4, 9.3: no mean annual burn map")
  expect_null(seen2$files)
})

test_that("burnProbabilityMap = FALSE writes only the ELF map", {
  out <- withr::local_tempdir()
  writeBurnMap(out, "6.4", "meanAnnualCumulBurnMap")
  seen <- runELFmap(out, params = list(burnProbabilityMap = FALSE))
  expect_null(seen$files)
  expect_identical(SpaDES.core::params(SpaDES.core::simInit(modules = "fireSense_summary", times = list(start = 0, end = 1),
    paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"),
                 inputPath = file.path(out, "in")), params = list(fireSense_summary = list(mode = "ELFmap", reportedELFs = "6.4"))))$fireSense_summary$burnProbabilityMap, TRUE)
})
