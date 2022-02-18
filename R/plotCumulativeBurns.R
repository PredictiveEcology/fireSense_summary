#' Plot cumulative burn maps
#'
#' @param Nreps TODO
#' @param studyAreaName TODO
#' @param climateScenario TODO
#' @param outputDir TODO
#' @param rasterToMatch TODO
#'
#' @return list of filepaths corresponding to the images and/or objects written to disk
#'
#' @export
#' @importFrom raster calc crop mask maxValue raster stack
#' @importFrom rasterVis levelplot rasterTheme
#' @importFrom RColorBrewer brewer.pal
plotCumulativeBurns <- function(Nreps, studyAreaName, climateScenario, outputDir, rasterToMatch) {
  burnMapAllReps <- lapply(1:Nreps, function(rep) {
    runName <- sprintf("%s_%s_run%02d", studyAreaName, climateScenario, rep)
    resultsDir <- file.path(outputDir, runName)

    burnMap <- raster(file.path(resultsDir, "burnMap_2100_year2100.tif"))
  })

  cumulBurnMap <- calc(stack(burnMapAllReps), fun = sum) / Nreps
  cumulBurnMap <- mask(crop(cumulBurnMap, rasterToMatch), rasterToMatch)

  myPal <- RColorBrewer::brewer.pal("Reds", n = Nreps + 1) ## include 0 ## TODO: max 9 cols!
  myTheme <- rasterVis::rasterTheme(region = myPal)

  fburnMap <- file.path(outputDir, studyAreaName, "figures",
                        paste0("cumulBurnMap_", studyAreaName, "_", climateScenario, ".png"))
  png(filename = fburnMap, height = 800, width = 800)
  rasterVis::levelplot(cumulBurnMap, margin = list(FUN = "mean"), ## median?
                       main = paste0("Cumulative burn map 2011-2100 under ", climateScenario),
                       colorkey = list(
                         at = seq(0, maxValue(cumulBurnMap), length.out = Nreps + 1),
                         space = "bottom",
                         axis.line = list(col = "black"),
                         width = 0.75
                       ),
                       par.settings = myTheme)
  dev.off()

  return(fburnMap)
}
