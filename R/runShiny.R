#' @title run KDHEtools Shiny Example
#'
#' @description Launches Shiny app for KDHEtools.
#'
#' @details The Shiny app based on the R package KDHEtools is included in the R package.
#' This function launches that app.
#'
#' The Shiny app is online at:
#' https://tetratech-wtr-wne.shinyapps.io/KDHEtools/
#'
#' @examples
#' \dontrun{
#' # Run Function
#' runShiny()
#' }
#
#' @export
runShiny <- function(){##FUNCTION.START
  #
  appDir <- system.file("shiny-examples", "KDHEtools", package = "KDHEtools")
  #
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `KDHEtools`.", call. = FALSE)
  }
  #
  shiny::runApp(appDir, display.mode = "normal")
  #
}##FUNCTION.END
