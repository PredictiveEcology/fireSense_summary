## `studyAreaName` and `years` default from the simulation, so a project need not
## repeat its `.studyAreaName` global or its start and end times here.

initOnly <- function(params) {
  sim <- SpaDES.core::simInit(times = list(start = 2025, end = 2044), modules = moduleName,
                              params = params, paths = testPaths)
  SpaDES.core::spades(sim, events = list(fireSense_summary = "init"), debug = FALSE)
}

test_that("studyAreaName defaults to the .studyAreaName global", {
  sim <- initOnly(list(.globals = list(.studyAreaName = "4.2.1")))
  expect_identical(SpaDES.core::P(sim, module = moduleName)$studyAreaName, "4.2.1")
})

test_that("a supplied studyAreaName is kept", {
  sim <- initOnly(list(.globals = list(.studyAreaName = "4.2.1"),
                       fireSense_summary = list(studyAreaName = "Mackenzie")))
  expect_identical(SpaDES.core::P(sim, module = moduleName)$studyAreaName, "Mackenzie")
})

test_that("years default to the simulation's start and end", {
  sim <- initOnly(list())
  years <- SpaDES.core::resolveSimYears(SpaDES.core::P(sim, module = moduleName)$years, sim)
  expect_equal(unname(years), c(2025, 2044))
})
