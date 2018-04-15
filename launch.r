#' Function to launch ascr shniy application.
#' Looks for the ascr app folder in home directory
#' No arguments are required.

launch.ui <- function() {
  appDir <- list.files("/home", "ascr_shiny", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)[1]
  if (appDir == "") {
    stop("Looks like the ascr_shiny application is not available. Please download from https://github.com/cmjt/ascr_shiny" , call. = FALSE)
  }
  pkgs <- c("shiny","rmarkdown", "shinyjs", "shinycssloaders","shinythemes","animation","devtools")
  options(warn = -1)
  for (i in pkgs){
      if (!require(i, quietly = TRUE, character.only = TRUE)){
          install.packages(i)
      }
  }
  if(!require("ascr",quietly = TRUE, character.only = TRUE)){
      devtools::install_github("https://github.com/b-steve/ascr")
  }
  runApp(appDir, display.mode = "normal",launch.browser = TRUE)
}


