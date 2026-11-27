## Template `tests/testthat/setup.R` for a SpaDES module.
##
## The module is converted to a package rendition before the tests run (see the
## testthat-module workflow), so the module's own functions are already in a real
## namespace: a test can call `myHelper(x)` directly rather than reaching into
## `sim@.xData$.mods$<module>$myHelper`.
##
## This file only sets options and makes a scratch directory. It deliberately does
## NOT install packages or download anything: dependency resolution happens once,
## before the tests, from the module's own `reqdPkgs` metadata.

withr::local_options(
  list(
    reproducible.useMemoise = TRUE,
    reproducible.verbose    = -2,
    Require.verbose         = -2,
    spades.moduleCodeChecks = FALSE,
    spades.moduleDocument   = FALSE,
    spades.useRequire       = FALSE
  ),
  .local_envir = testthat::teardown_env()
)

## Where the module is, from testthat's working directory.
##
## testthat runs with the working directory set to `tests/testthat`, so the module
## directory is two levels up and the `modulePath` that SpaDES.core wants -- the
## directory *containing* modules -- is three. Getting this wrong is silent: the
## module is simply not found, and every test that needs it skips or errors on
## something unrelated.
moduleRoot <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)
moduleName <- basename(moduleRoot)
modulePath <- dirname(moduleRoot)

## A scratch tree removed when the suite finishes. Write here, never beside the
## module: the tests run against a throwaway copy, but the habit matters when they
## are run by hand.
testPaths <- local({
  root <- withr::local_tempdir(.local_envir = testthat::teardown_env())
  paths <- list(
    cachePath  = file.path(root, "cache"),
    inputPath  = file.path(root, "inputs"),
    modulePath = modulePath,
    outputPath = file.path(root, "outputs")
  )
  for (p in paths[c("cachePath", "inputPath", "outputPath")]) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
  paths
})

## A function of the module, as the module sees it (the module is not attached in the package rendition).
moduleFun <- function(name) {
  out <- withr::local_tempdir(.local_envir = parent.frame())
  SpaDES.core::simInit(times = list(start = 0, end = 1),
                       params = list(fireSense_summary = list(mode = "ELFmap", simOutputPath = out)),
                       modules = "fireSense_summary",
                       paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"),
                                    inputPath = file.path(out, "in")))$.mods$fireSense_summary[[name]]
}

## Fixtures shared by test-ELFmap.R and test-burnProbabilityMap.R (here, not a helper file: runELFmap() needs modulePath)

writeNrvFixture <- function(path, elf, nTests, nInside, depth = "postprocess/csv", stand = c(nTests, nInside)) {
  d <- file.path(path, elf, depth)
  dir.create(d, recursive = TRUE)
  g <- data.frame(poly = elf, group = c("landscape", "stand"), nTests = c(nTests - stand[1], stand[1]),
                  nInside = c(nInside - stand[2], stand[2]))
  g <- g[g$nTests > 0, ]
  g$pctInside <- 100 * g$nInside / g$nTests
  utils::write.csv(g, file.path(d, "nrvInside.csv"), row.names = FALSE)
}


## <out>/<ELF>/mainSim/<stem>.tif, as an NRV report leaves them
writeBurnMap <- function(out, elf, stem) {
  d <- file.path(out, elf, "mainSim"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(d, paste0(stem, ".tif"))
  terra::writeRaster(terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:3978", vals = 0.01), f)
  f
}

runELFmap <- function(out, params = list(), env = parent.frame()) {
  seen <- new.env()
  testthat::local_mocked_bindings(
    plotELFs = function(...) graphics::plot(1),
    mosaicELFs = function(files, ELFs, filename, ...) { seen$files <- files; seen$ELFs <- ELFs; seen$filename <- filename; filename },
    .package = "fireSenseUtils", .env = env)
  ELFs <- list(rasWhole = list(`6.4` = 1), rasCentered = list())
  SpaDES.core::simInitAndSpades(
    times = list(start = 0, end = 1),
    params = list(fireSense_summary = c(list(mode = "ELFmap", reportedELFs = c("6.4", "9.3"), simOutputPath = out, .plots = "png"), params)),
    modules = "fireSense_summary", objects = list(ELFs = ELFs),
    paths = list(modulePath = modulePath, outputPath = out, cachePath = file.path(out, "cache"), inputPath = file.path(out, "in")))
  seen
}

