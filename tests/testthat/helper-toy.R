## Toy fixtures for the behaviour tests.
##
## Everything here is built in memory from `terra` / `data.table`, which are in the
## module's `reqdPkgs`, so these run under the CI workflow (which installs `reqdPkgs`
## and nothing else) rather than skipping.

## A 4x4 template raster on a projected CRS, 100 m pixels.
toyRTM <- function() {
  terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 400, ymin = 0, ymax = 400,
              crs = "EPSG:3978", vals = 1)
}

## A cumulative burn map with exactly 6 burned (value 1) pixels of the 16.
toyBurnMap <- function() {
  terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 400, ymin = 0, ymax = 400,
              crs = "EPSG:3978", vals = c(rep(1L, 6), rep(0L, 10)))
}

toyBurnSummary <- function() {
  data.table::data.table(
    year    = c(1L, 1L, 2L),
    rep     = c("rep1", "rep1", "rep1"),
    sizeHa  = c(10, 20, 30)
  )
}

## Two annual fire polygons carrying `areaCol` as their area column, as a *list* --
## which is the class the `firePolys` input is declared to have.
toyFirePolys <- function(areaCol = "ADJ_HA") {
  p1 <- terra::vect("POLYGON ((0 0, 100 0, 100 100, 0 100, 0 0))", crs = "EPSG:3978")
  p1$YEAR <- "1990"      ## character on purpose: InitMulti() is meant to coerce it
  p1[[areaCol]] <- 111
  p2 <- terra::vect("POLYGON ((200 200, 300 200, 300 300, 200 300, 200 200))",
                    crs = "EPSG:3978")
  p2$YEAR <- "1991"
  p2[[areaCol]] <- 222
  list(p1, p2)
}

toyIgnitionPoints <- function() {
  p <- terra::vect(cbind(c(50, 250), c(50, 250)), crs = "EPSG:3978")
  p$YEAR <- c(1990L, 1991L)
  p
}

## `InitMulti()` reads `mod$...`, which SpaDES.core resolves through an active
## binding that needs the module's own function environment and a `sim` it can find
## on the call stack. Reaching the function through `sim$.mods` and taking `sim` as
## the argument name satisfies both, and works whether or not the module has been
## converted to a package.
runInitMulti <- function(sim) {
  sim@current <- data.frame(eventTime = start(sim), moduleName = "fireSense_summary",
                            eventType = "init", eventPriority = 1)
  sim$.mods$fireSense_summary$InitMulti(sim)
}

## The `mod` environment of this module, after a call.
modOf <- function(sim) sim@.xData$.modObjs$fireSense_summary
