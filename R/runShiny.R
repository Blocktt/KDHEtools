#' @title run WDEQtools Shiny Example
#'
#' @description Launches Shiny app for WDEQtools.
#'
#' @details The Shiny app based on the R package WDEQtools is included in the R package.
#' This function launches that app.
#'
#' The Shiny app is online at:
#' https://tetratech-wtr-wne.shinyapps.io/WDEQtools/
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
  appDir <- system.file("shiny-examples", "WDEQtools", package = "WDEQtools")
  #
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `WDEQtools`.", call. = FALSE)
  }
  #
  shiny::runApp(appDir, display.mode = "normal")
  #
}##FUNCTION.END
