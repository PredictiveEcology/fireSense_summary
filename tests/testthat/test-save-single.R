## `save_single` is the only event that runs in a plain simulation, and it is the
## one a project actually depends on: everything the multi-mode summary later reads
## is written here. These run the real event through spades(), not the function in
## isolation.

makeSingleSim <- function(times = list(start = 2011, end = 2013), ...) {
  out <- withr::local_tempdir(.local_envir = parent.frame())
  SpaDES.core::simInit(
    times = times,
    params = list(fireSense_summary = list(mode = "single")),
    modules = "fireSense_summary",
    objects = list(burnMap = toyBurnMap(), burnSummary = toyBurnSummary(), ...),
    paths = list(modulePath = modulePath, outputPath = out,
                 cachePath = file.path(out, "cache"), inputPath = file.path(out, "in"))
  )
}

test_that("single mode writes the burn map and summary, named for end(sim)", {
  s <- makeSingleSim()
  out <- SpaDES.core::outputPath(s)
  sOut <- SpaDES.core::spades(s, debug = FALSE)

  ## 2013 with padL = ceiling(log10(2014)) = 4 -> "2013", hence burnMap_year2013.tif
  expect_setequal(
    setdiff(list.files(out), c("cache", "in")),
    c("burnMap_year2013.tif", "fireSense_burnSummary.csv")
  )

  ## the raster round-trips: 6 burned pixels of 16, written as INT2U
  r <- terra::rast(file.path(out, "burnMap_year2013.tif"))
  expect_equal(dim(r), c(4L, 4L, 1L))
  expect_equal(sum(terra::values(r)), 6)
  expect_identical(terra::crs(r, describe = TRUE)$code, "3978")

  ## and so does the summary table
  dt <- data.table::fread(file.path(out, "fireSense_burnSummary.csv"))
  expect_equal(dim(dt), c(3L, 3L))
  expect_identical(names(dt), c("year", "rep", "sizeHa"))
  expect_equal(dt$sizeHa, c(10, 20, 30))
})

test_that("both written files are registered in outputs(sim)", {
  ## registerOutputs() is what makes these files discoverable by a later multi-mode
  ## run via `outputsDF`; if it stops happening the summary silently sees nothing.
  s <- makeSingleSim()
  out <- SpaDES.core::outputPath(s)
  sOut <- SpaDES.core::spades(s, debug = FALSE)

  odf <- SpaDES.core::outputs(sOut)
  expect_equal(NROW(odf), 2L)
  expect_setequal(basename(odf$file),
                  c("burnMap_year2013.tif", "fireSense_burnSummary.csv"))
  expect_equal(unique(odf$saveTime), 2013)
})

test_that("the burn map filename follows end(sim), not start(sim)", {
  ## The padded year comes from time(sim) at the scheduled event, which is end(sim).
  ## A 5-digit end year also exercises the ceiling(log10(end + 1)) padding rule.
  s <- makeSingleSim(times = list(start = 1, end = 7))
  out <- SpaDES.core::outputPath(s)
  SpaDES.core::spades(s, debug = FALSE)
  ## padL = ceiling(log10(8)) = 1, so no zero padding
  expect_true(file.exists(file.path(out, "burnMap_year7.tif")))
  expect_false(file.exists(file.path(out, "burnMap_year1.tif")))
})

## NOT TESTED: the unknown-event-type branch. `noEventWarning()` in SpaDES.core
## only *returns* a message string, and `switch()`'s default value is discarded by
## `doEvent`, so an unknown event type is silently a no-op. Asserting that would
## enshrine it; it is upstream behaviour, not this module's.

test_that("single-mode init schedules exactly one save_single at end(sim)", {
  ## The two modes must not overlap: a single-mode run schedules exactly one
  ## save_single at end(sim); a multi-mode run schedules nothing (it does its work
  ## inside init) and so must never write burn maps.
  s <- makeSingleSim()
  before <- SpaDES.core::events(s)
  after <- SpaDES.core::events(
    s$.mods$fireSense_summary$doEvent.fireSense_summary(s, SpaDES.core::start(s), "init")
  )
  added <- after[!after$eventType %in% before$eventType, ]
  expect_identical(NROW(added), 1L)
  expect_identical(added$eventType, "save_single")
  expect_equal(as.numeric(added$eventTime), 2013)
  expect_identical(added$moduleName, "fireSense_summary")

  ## and it is the LAST priority, so it runs after everything else scheduled at
  ## end(sim) -- the burn map it saves must be the final one.
  expect_equal(as.numeric(added$eventPriority), SpaDES.core::.last())
})
