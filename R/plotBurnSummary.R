#' Title
#'
#' @param Nreps TODO
#' @param studyAreaName TODO
#' @param climateScenario TODO
#' @param outputDir TODO
#'
#' @return
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom cowplot draw_label ggdraw plot_grid
#' @importFrom gglpot2 aes element_blank element_text facet_grid geom_point ggplot ggsave
#' @importform ggplot2 labeller labs stat_smooth theme unit ylab
#' @importFrom qs qload
#' @importFrom stats coefficients lm pf
plotBurnSummary <- function(Nreps, studyAreaName, climateScenario, outputDir) {
  burnSummaryAllReps <- rbindlist(lapply(1:Nreps, function(rep) {
    runName <- sprintf("%s_%s_run%02d", studyAreaName, climateScenario, rep)
    resultsDir <- file.path(outputDir, runName)

    burnDT <- qs::qload(file.path(resultsDir, "burnSummary_year2100.qs"))
    burnSummary <- data.table(year = burnDT[["year"]],
                              N = burnDT[["N"]],
                              areaBurnedHa = burnDT[["areaBurnedHa"]],
                              rep = as.integer(rep))
    burnSummary ## TODO: this is the BUFFERED studyArea, not the REPORTING one!!!!
  }))

  # totAreaBurned <- burnSummaryAllReps[, lapply(.SD, sum), by = c("year", "rep"), .SDcols = "areaBurnedHa"]
  # totAreaBurend <- totAreaBurned[, lapply(.SD, mean), by = "year", .SDcols = "areaBurnedHa"]

  burnSummaryAllReps[, sumAB := sum(areaBurnedHa), by = c("year", "rep")]
  areaBurned <- unique(burnSummaryAllReps[, c("year", "rep", "sumAB")])

  tend <- lm(sumAB ~ year, data = areaBurned)
  coeff <- coefficients(tend)
  Fstats <- summary(tend)$fstatistic
  names(Fstats) <- NULL
  pValueA <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.01, " \n(significant)", " \n(non-significant)")

  areaBurned[, var := "area_burned"]
  areaBurned[, val := sumAB]

  # numberFires <- burnSummaryAllReps[, lapply(.SD, length), by = c("year", "rep"), .SDcols = "N"]
  # numberFires <- numberFires[, lapply(.SD, mean), by = "year", .SDcols = "N"]

  burnSummaryAllReps[, Nfires := length(N), by = c("year", "rep")]
  nFires <- unique(burnSummaryAllReps[, c("year", "rep", "Nfires")])

  tendF <- lm(Nfires ~ year, data = nFires)
  coeffF <- coefficients(tendF)
  Fstats <- summary(tendF)$fstatistic
  names(Fstats) <- NULL
  pValueF <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.01, " \n(significant)", " \n(non-significant)")
  nFires[, var := "number_fires"]
  nFires[, val := Nfires]

  # meanFireSize <- burnSummaryAllReps[, lapply(.SD, mean), by = c("year", "rep"), .SDcols = "areaBurnedHa"]
  # meanFireSize <- meanFireSize[, lapply(.SD, mean), by = "year", .SDcols = "areaBurnedHa"]

  burnSummaryAllReps[areaBurnedHa > 6.25, fireSize := mean(areaBurnedHa, na.rm = TRUE),
                     by = c("year", "rep")]
  fireSize <- na.omit(unique(burnSummaryAllReps[, c("year", "rep", "fireSize")]))

  tendS <- lm(fireSize ~ year, data = fireSize)
  coeffS <- coefficients(tendS)
  Fstats <- summary(tendS)$fstatistic
  names(Fstats) <- NULL
  pValueS <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.01, " \n(significant)", " \n(non-significant)")

  fireSize[, var := "fire_size"]
  fireSize[, val := fireSize]

  ### plotting

  coefXA <- round(coeff[2], 1)
  coefYA <- round(coeff[1], 1)
  coefXF <- round(coeffF[2], 1)
  coefYF <- round(coeffF[1], 1)
  coefXS <- round(coeffS[2], 1)
  coefYS <- round(coeffS[1], 1)

  replacementNames <- c(
    paste0("Area burned:\n",
           "y = ", ifelse(coefXA < 10000, coefXA, formatC(coefXA, format = "e", digits = 2)),
           "x + ", ifelse(coefYA < 10000, coefYA, formatC(coefYA, format = "e", digits = 2)), pValueA),
    paste0("No fires:\n",
           "y = ", ifelse(coefXF < 10000, coefXF, formatC(coefXF, format = "e", digits = 2)),
           "x + ", ifelse(coefYF < 10000, coefYF, formatC(coefYF, format = "e", digits = 2)), pValueF),
    paste0("Mean fire size:\n",
           "y = ", ifelse(coefXS < 10000, coefXS, formatC(coefXS, format = "e", digits = 2)),
           "x + ", ifelse(coefYS < 10000, coefYS, formatC(coefYS, format = "e", digits = 2)), pValueS)
  )
  names(replacementNames) <- c("area_burned", "number_fires", "fire_size")

  dt <- rbind(areaBurned, nFires, fireSize, use.names = FALSE)
  # Now remove original variable. It uses the first item's nameL sumAB
  dt[, sumAB := NULL]

  p1 <- ggplot(data = dt[var == "area_burned",], aes(x = year, y = val)) +
    geom_point(colour = "grey70") +
    stat_smooth(method = "lm", color = "darkred", fill = "red") +
    facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 9, face = "bold"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm")) +
    labs(y = "total area burned (ha)")
  p2 <- ggplot(data = dt[var == "number_fires",], aes(x = year, y = val, colour = "blue")) +
    geom_point(colour = "grey70") +
    stat_smooth(method = "lm", fill = "blue", color = "darkblue") +
    facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 9, face = "bold"),
          plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    ylab(label = "no. of fires")
  p3 <- ggplot(data = dt[var == "fire_size",], aes(x = year, y = val)) +
    geom_point(colour = "grey70") +
    stat_smooth(method = "lm", color = "orange", fill = "orange") +
    facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 9, face = "bold"),
          plot.margin = unit(c(-0.01, 0.2, 0.2, 0.2), "cm")) +
    labs(y = "mean fire size (ha)")

  title <- ggdraw() +
    draw_label(paste("Fires in the", studyAreaName, "study area under", climateScenario))

  p <- plot_grid(p1, p2, p3, align = "h", nrow = 3, labels = "AUTO")

  fgg <- file.path(outputDir, studyAreaName, "figures",
                   paste0("burnSummary_", studyAreaName, "_", climateScenario, ".png"))
  gg <- plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1))
  ggsave(gg, filename = fgg, height = 8, width = 11)
}
