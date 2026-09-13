mod_simulate_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "simulate-page",
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        fill = FALSE,
        bslib::card_header("Run Daisy"),
        shiny::p(class = "step-hint",
          "Runs the setup file from the Project page as-is. Tick ",
          shiny::strong("Use parameter defaults"), " to render the template with current parameter values first. Open ",
          shiny::strong("Plots"), " afterwards to read and chart ",
          shiny::tags$code(".dlf"), " output."
        ),
        shiny::uiOutput(ns("param_config_controls")),
        shiny::actionButton(ns("run"), "Run setup file", class = "btn-primary")
      ),
      bslib::card(
        fill = FALSE,
        bslib::card_header("Last run"),
        shiny::verbatimTextOutput(ns("run_summary"))
      )
    ),
    bslib::card(
      class = "simulate-log-card",
      fill = FALSE,
      bslib::card_header("Daisy log"),
      shiny::div(
        class = "d-flex justify-content-end mb-2",
        shiny::actionButton(ns("reload_log"), "Reload daisy.log", class = "btn-sm btn-outline-secondary")
      ),
      shiny::verbatimTextOutput(ns("run_log"))
    )
  )
}

mod_simulate_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    has_param_config <- function() {
      !is.null(project$config) && isTRUE(project$config_ok)
    }

    output$param_config_controls <- shiny::renderUI({
      if (!has_param_config()) return(NULL)
      shiny::checkboxInput(session$ns("use_defaults"), "Use parameter defaults", value = FALSE)
    })

    shiny::observe({
      tryCatch({
        use_tmpl <- has_param_config() && isTRUE(input$use_defaults)
        shiny::updateActionButton(
          session, "run",
          label = if (use_tmpl) "Render and run" else "Run setup file"
        )
      }, error = function(e) NULL)
    })

    shiny::observeEvent(input$run, {
      tryCatch({
        need_project(project)
        if (!nzchar(project$run_file)) stop("Set the setup file to run on the Project page.")
        run_path <- if (file.exists(project$run_file)) project$run_file
        else file.path(project$project_dir, project$run_file)
        if (!file.exists(run_path) && !file.exists(file.path(project$project_dir, project$run_file)))
          stop("Setup file not found: ", project$run_file)

        use_tmpl <- has_param_config() && isTRUE(input$use_defaults)
        with_nav_lock(session, project, function() {
          tryCatch({
            values <- NULL
            shiny::withProgress(message = "Running Daisy", value = 0.2, {
              if (use_tmpl) {
                values <- stats::setNames(
                  project$config$parameters$default,
                  project$config$parameters$name
                )
                shiny::incProgress(0.4, detail = "Executable")
              }
              run_project_daisy(project, values = values, show_log = TRUE)
            })
            if (nzchar(project$daisy_log_path %||% "")) {
              log_append(project, "Daisy finished with status ", project$last_status,
                         "; log ", project$daisy_log_path)
            } else if (is.null(values)) {
              log_append(project, "Daisy finished with status ", project$last_status,
                         " (setup file ", project$run_file, "; no daisy.log found)")
            } else {
              log_append(project, "Daisy finished with status ", project$last_status,
                         " (", paste(sprintf("%s=%s", names(values), signif(values, 4)), collapse = ", "), ")")
            }
            if (!identical(project$last_status, 0L))
              shiny::showNotification("Daisy returned a non-zero status. Check the log.", type = "warning")
            else
              shiny::showNotification("Run finished. Use the Plots page to view output.", type = "message")
          }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
        })
      }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    output$run_summary <- shiny::renderText({
      if (is.null(project$last_status))
        return("No Daisy run in this session yet.")
      vals <- project$last_run_values
      val_txt <- if (is.null(vals)) "\n(setup file as-is; template not rendered)"
      else paste0("\n", paste(sprintf("  %s = %s", names(vals), signif(vals, 5)), collapse = "\n"))
      paste0(
        "status = ", project$last_status,
        "\nrun file = ", project$run_file %||% "",
        val_txt
      )
    })

    shiny::observeEvent(input$reload_log, {
      project$daisy_log_path <- find_daisy_log(project$project_dir %||% "")
      project$daisy_log_stamp <- as.numeric(Sys.time())
    })

    output$run_log <- shiny::renderText({
      project$daisy_log_stamp
      path <- project$daisy_log_path %||% ""
      if (!nzchar(path)) {
        return(paste(
          "No daisy.log loaded yet.",
          "Run Daisy, or click Reload daisy.log if Output/daisy.log already exists."
        ))
      }
      read_daisy_log_text(path)
    })
  })
}
