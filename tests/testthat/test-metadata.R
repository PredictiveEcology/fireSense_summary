## The module's metadata is its public contract: a project using this module binds
## to these object names and classes. Renaming or retyping one breaks every caller,
## which is exactly the class of change the raster -> terra migration makes, so it is
## worth asserting here rather than discovering downstream.
##
## When a change is deliberate, update this file in the same commit and bump the
## module version to match: removed, renamed or retyped is a MAJOR bump.

test_that("module metadata parses", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_type(md, "list")
  expect_identical(md$name, moduleName)
})

test_that("inputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  inputs <- stats::setNames(md$inputObjects$objectClass, md$inputObjects$objectName)
  expect_identical(
    inputs[order(names(inputs))],
    c(burnMap            = "SpatRaster",
      burnSummary        = "data.table",
      firePolys          = "list",
      ignitionFirePoints = "SpatVector",
      outputsDF          = "data.table",
      rasterToMatch      = "SpatRaster")
  )
})

test_that("no outputs are declared", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_true(all(is.na(md$outputObjects$objectName)))
})

test_that("parameters are the expected names", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_identical(
    sort(md$parameters$paramName),
    sort(c("climateScenario", "mode", "reps", "simOutputPath", "studyAreaName",
           "years"))
  )
})

test_that("parameter classes and defaults are as declared", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  p <- md$parameters[order(md$parameters$paramName), ]
  expect_identical(
    stats::setNames(unlist(p$paramClass), p$paramName),
    c(climateScenario = "character", mode = "character", reps = "integer",
      simOutputPath = "character", studyAreaName = "character", years = "integer")
  )
  dflt <- stats::setNames(p$default, p$paramName)
  expect_identical(dflt$mode, "single")
  expect_identical(dflt$reps, 1L:10L)
  ## all-NA on purpose: resolveSimYears() fills it from the sim clock, so the module
  ## cannot silently summarise a year range the simulation never ran.
  expect_identical(dflt$years, c(NA_integer_, NA_integer_))
  expect_true(is.na(dflt$climateScenario))
  expect_true(is.na(dflt$studyAreaName))
})

test_that("the module declares the functions the tests reach for", {
  ## `InitMulti` and `doEvent` are what the behaviour tests call; if either is
  ## renamed those tests would silently stop covering anything.
  mods <- SpaDES.core::simInit(
    times = list(start = 1, end = 1),
    modules = moduleName,
    paths = list(modulePath = modulePath, outputPath = testPaths$outputPath,
                 cachePath = testPaths$cachePath, inputPath = testPaths$inputPath)
  )$.mods[[moduleName]]
  expect_true(all(c("InitMulti", "doEvent.fireSense_summary", ".inputObjects") %in%
                    ls(mods, all.names = TRUE)))
})

test_that(".inputObjects returns the simList unchanged", {
  ## It is empty by design -- both optional inputs are resolved in InitMulti() --
  ## so the contract is that it is a no-op, not that it does nothing at all.
  s <- SpaDES.core::simInit(
    times = list(start = 1, end = 1),
    modules = moduleName,
    paths = list(modulePath = modulePath, outputPath = testPaths$outputPath,
                 cachePath = testPaths$cachePath, inputPath = testPaths$inputPath)
  )
  before <- sort(ls(s@.xData, all.names = TRUE))
  s2 <- s$.mods[[moduleName]]$.inputObjects(s)
  expect_s4_class(s2, "simList")
  expect_identical(sort(ls(s2@.xData, all.names = TRUE)), before)
})
