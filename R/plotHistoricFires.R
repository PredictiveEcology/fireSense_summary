#' Plot historic ignitions, escapes, and area burned
#'
#' @param climateScenario
#' @param studyAreaName
#' @param firePolys
#' @param outputDir
#'
#' @return
#' @export
#'
#' @examples
plotHistoricFires <- function(climateScenario, studyAreaName, firePolys, outputDir) {
  gcm <- strsplit(climateScenario, "_")[[1]][1]
  ssp <- strsplit(climateScenario, "_")[[1]][2]
  runName <- sprintf("%s_%s_run01", studyAreaName, climateScenario) ## doesn't matter which run, all same
  run <- as.integer(strsplit(runName, "run")[[1]][2])
  sim <- loadSimList(file.path(outputDir, runName, paste0(runName, ".qs")))
  burnSummary <- sim$burnSummary
  rm(sim)

  historicalBurns <- do.call(what = rbind, args = firePolys)
  historicalBurns <- as.data.table(historicalBurns@data)

  ## restrict to escapes only, but sum poly_ha for burns
  res <- ifelse(grepl("ROF", studyAreaName), 125, 250)
  historicalBurns <- historicalBurns[SIZE_HA > res, .(sumBurn = sum(as.numeric(POLY_HA)), nFires = .N), .(YEAR)]
  setnames(historicalBurns, "YEAR", "year")
  historicalBurns[, stat := 'observed']
  projectedEscapes <- burnSummary[areaBurnedHa > res, .(nFires = .N), .(year)]
  projectedBurns <- burnSummary[, .(sumBurn = sum(areaBurnedHa)), .(year)]
  projectedBurns <- projectedBurns[projectedEscapes, on = c("year")]
  projectedBurns[, stat := "projected"]
  dat <- rbind(projectedBurns, historicalBurns)

  trueHistoricalIgs <- as.data.table(ignitionFirePoints) %>%
    .[, .N, .(YEAR)] %>%
    setnames(., "YEAR", "year") %>%
    .[, stat := "observed"] %>%
    .[, year := as.numeric(year)]
  projectedIgs <- burnSummary[, .N, .(year)] %>%
    .[, stat := "projected"]
  dat2 <- rbind(trueHistoricalIgs, projectedIgs)

  gIgnitions <- ggplot(data = dat2, aes(x = year, y = N, col = stat)) +
    geom_point() +
    # geom_smooth() +
    ylim(0, max(dat2$N) * 1.2) +
    labs(y = "number of ignitions",
         title = studyAreaName,
         subtitle = paste(gcm, ssp))

  gEscapes <- ggplot(data = dat, aes(x = year, y = nFires, col = stat)) +
    geom_point() +
    # geom_smooth() +
    ylim(0, max(dat$nFires) * 1.2) +
    labs(y = "number of escaped fires",
         title = studyAreaName,
         subtitle = paste(gcm, ssp))

  gBurns <- ggplot(data = dat, aes(x = year, y = sumBurn, col = stat)) +
    geom_point() +
    # geom_smooth() +
    ylim(0, max(dat$sumBurn) * 1.1) +
    labs(y = "annual area burned (ha)",
         title = paste(studyAreaName, "rep", run),
         subtitle = paste(gcm, ssp))


  figDir <- file.path(outputDir, runName, "figures")
  figs <- list(
    ignition = file.path(figDir, paste0("simulated_Ignitions", studyAreaName, ".png")),
    escape = file.path(figDir, paste0("simulated_Escapes", studyAreaName, ".png")),
    spread = file.path(figDir, paste0("simulated_burnArea", studyAreaName, ".png"))
  )
  ggsave(plot = gIgnitions, filename = figs$ignition)
  ggsave(plot = gEscapes, filename = figs$escape)
  ggsave(plot = gBurns, filename = figs$spread)

  return(figs)
}
