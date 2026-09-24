## `.studyAreaName` comes from `.globals`, like every other module's, and `years` default to the
## simulation's start and end, so a project need not repeat either here.

initOnly <- function(params) {
  sim <- SpaDES.core::simInit(times = list(start = 2025, end = 2044), modules = moduleName,
                              params = params, paths = testPaths)
  SpaDES.core::spades(sim, events = list(fireSense_summary = "init"), debug = FALSE)
}

test_that(".studyAreaName is taken from .globals", {
  sim <- initOnly(list(.globals = list(.studyAreaName = "4.2.1")))
  expect_identical(SpaDES.core::P(sim, module = moduleName)$.studyAreaName, "4.2.1")
})

test_that("years default to the simulation's start and end", {
  sim <- initOnly(list())
  years <- SpaDES.core::resolveSimYears(SpaDES.core::P(sim, module = moduleName)$years, sim)
  expect_equal(unname(years), c(2025, 2044))
})
