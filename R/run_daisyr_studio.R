#' Launch the daisyr Shiny studio
#'
#' Opens the graphical workflow for setting up Daisy projects, editing
#' parameter registries, running simulations, and running calibration,
#' sensitivity, and design analyses. Requires a working [daisyr] installation
#' and, for simulation, a Daisy executable (or a command template).
#'
#' @param ... Passed to [shiny::runApp()] (for example `launch.browser = TRUE`,
#'   `port = 3838`).
#'
#' @return The value of [shiny::runApp()], invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' daisyr.studio::run_daisyr_studio()
#' }
run_daisyr_studio <- function(...) {
  shiny::runApp(daisyr_studio_app(), ...)
}

#' Build the daisyr studio Shiny application object
#'
#' Use this when you want the app object without immediately running it
#' (for example `shiny::runApp(daisyr_studio_app(), port = 3838)`).
#'
#' @return A `shiny.appobj`.
#' @export
daisyr_studio_app <- function() {
  options(shiny.maxRequestSize = 80 * 1024^2)
  shiny::shinyApp(ui = daisyr_studio_ui(), server = daisyr_studio_server)
}
