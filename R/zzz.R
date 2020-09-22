.onAttach <- function(...) {
  packageStartupMessage(paste("\nWelcome to {sparrpowR} version ", utils::packageDescription("sparrpowR")$Version, "\n> help(\"sparrpowR\") # for documentation\n> citation(\"sparrpowR\") # for how to cite\n", sep = ""), appendLF = TRUE)
}
