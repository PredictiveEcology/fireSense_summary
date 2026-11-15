## runs the init event by hand, as SpaDES does: with the module as the current one
runInit <- function(s) {
  s@current <- data.table::data.table(moduleName = "fireSense_summary")
  s$.mods$fireSense_summary$doEvent.fireSense_summary(s, SpaDES.core::start(s), "init")
}
