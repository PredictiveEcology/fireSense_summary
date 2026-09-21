## `InitMulti()` is the module's only non-trivial function. It cannot be exercised
## end-to-end in CI -- `doEvent`'s multi branch calls three `fireSenseUtils` plotting
## functions that want real replicate outputs, and its download branches need the
## network -- but everything before those calls is pure bookkeeping over its inputs
## and is tested here on toy objects.

makeMultiSim <- function(firePolys = toyFirePolys(), outputsDF = NULL, reps = 1L,
                         years = c(NA_integer_, NA_integer_),
                         times = list(start = 2011, end = 2013),
                         simOutputPath = NULL, objects = list()) {
  out <- withr::local_tempdir(.local_envir = parent.frame())
  if (is.null(simOutputPath)) simOutputPath <- out
  if (is.null(outputsDF)) {
    outputsDF <- data.table::data.table(
      file = c(
        file.path(simOutputPath, "rep1", "burnMap_year2013.tif"),
        file.path(simOutputPath, "rep1", "fireSense_burnSummary.csv"),
        file.path(simOutputPath, "rep2", "burnMap_year2013.tif"),
        ## both must be filtered out: the first by the extension filter (it does
        ## match "burnMap"), the second because rep2 is outside the default `reps`
        file.path(simOutputPath, "rep1", "burnMap_year2013.png")
      ),
      saveTime = 2013
    )
  }
  objs <- c(list(rasterToMatch = toyRTM(), outputsDF = outputsDF,
                 ignitionFirePoints = toyIgnitionPoints()), objects)
  if (!is.null(firePolys)) objs$firePolys <- firePolys
  SpaDES.core::simInit(
    times = times,
    params = list(fireSense_summary = list(
      mode = "multi", reps = reps, simOutputPath = simOutputPath,
      studyAreaName = "toySA", years = years
    )),
    modules = "fireSense_summary",
    objects = objs,
    paths = list(modulePath = modulePath, outputPath = out,
                 cachePath = file.path(out, "cache"), inputPath = file.path(out, "in"))
  )
}

test_that("with outputsDF, simFiles are the matching rep files and nothing else", {
  s <- runInitMulti(makeMultiSim(reps = 1L))
  m <- modOf(s)

  expect_true(m$useOutputs)
  ## rep2 is dropped (not in `reps`), and the .png is dropped by the extension filter
  expect_identical(basename(m$simFiles),
                   c("burnMap_year2013.tif", "fireSense_burnSummary.csv"))
  expect_identical(m$allReps, "rep1")
  ## simFiles is exactly bmbs -- the paths handed to the plotting functions
  expect_identical(m$simFiles, m$bmbs)
})

test_that("reps selects which replicate directories are used", {
  s <- runInitMulti(makeMultiSim(reps = 1L:2L))
  m <- modOf(s)
  expect_identical(m$allReps, c("rep1", "rep2"))
  expect_equal(length(m$simFiles), 3L) ## two rep1 files + one rep2 file
  expect_true(any(grepl("/rep2/", m$simFiles)))

  ## NOT asserted: what happens when `reps` names a replicate that produced nothing.
  ## SpaDES.core::dirnamesFromSet() returns the bare prefix "rep" for an empty
  ## match, and the `(rep)` alternation InitMulti() then builds matches every rep
  ## directory, so asking for rep3 alone yields all three rep1/rep2 files. That is
  ## upstream behaviour in SpaDES.core, not this module, and pinning it here would
  ## enshrine it.
})

test_that("an all-NA years parameter resolves to the simulation's own start and end", {
  ## The point of the NA default: a 1991-2020 run that does not set `years` must not
  ## silently look for a hardcoded range.
  s <- runInitMulti(makeMultiSim(times = list(start = 1991, end = 2020)))
  expect_equal(unname(SpaDES.core::P(s, module = "fireSense_summary")$years), c(1991, 2020))
})

test_that("an explicit years parameter is left alone", {
  s <- runInitMulti(makeMultiSim(years = c(2011L, 2015L)))
  expect_equal(SpaDES.core::P(s, module = "fireSense_summary")$years, c(2011L, 2015L))
})

test_that("the figures directory is created under simOutputPath", {
  s0 <- makeMultiSim()
  figDir <- file.path(SpaDES.core::P(s0, module = "fireSense_summary")$simOutputPath,
                      "figures", "fireSense_summary")
  expect_false(dir.exists(figDir))
  runInitMulti(s0)
  expect_true(dir.exists(figDir))
})

test_that("supplied firePolys are bound into one SpatVector with an integer YEAR", {
  s <- runInitMulti(makeMultiSim())
  fp <- modOf(s)$firePolys

  expect_s4_class(fp, "SpatVector")
  expect_equal(nrow(fp), 2L)          ## the two annual polygons, row-bound
  expect_equal(fp$YEAR, c(1990L, 1991L))  ## coerced from character
  expect_type(fp$YEAR, "integer")
})

test_that("SIZE_HA is taken from ADJ_HA when present", {
  s <- runInitMulti(makeMultiSim(firePolys = toyFirePolys("ADJ_HA")))
  fp <- modOf(s)$firePolys
  expect_true("SIZE_HA" %in% names(fp))
  expect_equal(fp$SIZE_HA, c(111, 222))
})

test_that("SIZE_HA falls back to POLY_HA, from the bound polygons", {
  ## Regression test. This branch used to read `sim$firePolys`, which is still the
  ## *list* of annual SpatVectors the input arrived as, so tidyterra::mutate() failed
  ## with "no applicable method for 'mutate' applied to an object of class list".
  ## It now reads `mod$firePolys`, matching the ADJ_HA branch above.
  s <- runInitMulti(makeMultiSim(firePolys = toyFirePolys("POLY_HA")))
  fp <- modOf(s)$firePolys
  expect_s4_class(fp, "SpatVector")
  expect_equal(fp$SIZE_HA, c(111, 222))
  expect_equal(fp$SIZE_HA, fp$POLY_HA)
})

test_that("an existing SIZE_HA column is preserved, not recomputed", {
  fp <- toyFirePolys("ADJ_HA")
  fp[[1]]$SIZE_HA <- 1
  fp[[2]]$SIZE_HA <- 2
  s <- runInitMulti(makeMultiSim(firePolys = fp))
  expect_equal(modOf(s)$firePolys$SIZE_HA, c(1, 2))
})

test_that("supplied ignitionFirePoints are passed through untouched", {
  ## The alternative branch downloads the NFDB, so supplying them must short-circuit
  ## it; if it stopped doing so CI would hang or fail on the network.
  pts <- toyIgnitionPoints()
  s <- runInitMulti(makeMultiSim())
  got <- modOf(s)$ignitionFirePoints
  expect_s4_class(got, "SpatVector")
  expect_equal(nrow(got), 2L)
  expect_equal(got$YEAR, pts$YEAR)
  expect_equal(terra::crds(got), terra::crds(pts))
})

test_that("without outputsDF the simOutputPath is globbed, and missing files stop", {
  ## The `else` branch: no outputsDF, nothing on disk, so every expected file is
  ## reported missing and the error names them.
  out <- withr::local_tempdir()
  s <- makeMultiSim(outputsDF = data.table::data.table(), reps = 1L:2L,
                    years = c(2011L, 2013L), simOutputPath = out)
  expect_error(
    runInitMulti(s),
    "4 simulation files appear to be missing"
  )
  err <- tryCatch(runInitMulti(s), error = function(e) conditionMessage(e))
  ## the %04d-padded name the else branch expects, per replicate
  expect_match(err, "rep1/burnMap_year2013\\.tif", fixed = FALSE)
  expect_match(err, "rep2/fireSense_burnSummary\\.csv", fixed = FALSE)
})

## NOT TESTED, deliberately: the "files all present" success path of the glob
## branch. It appears to be unreachable. The `_year(...)` grep on line ~183 drops
## every path without `_year` in its name, which includes
## `fireSense_burnSummary.csv`, yet `filesExpected` below requires exactly that
## file per replicate -- so `filesNeeded$exists` is FALSE for it even when the file
## is there on disk, and InitMulti() stops. Confirmed by creating the four expected
## files on disk and calling InitMulti(): it still reports the two .csv files
## missing. This is pre-existing on `development` (same two greps, same
## `filesExpected`), is not touched by this PR, and is not asserted here because
## writing a test around it would enshrine it as correct behaviour.
