daisyr_studio_server <- function(input, output, session) {
  output$nav_menu <- shiny::renderUI({
    current <- input$nav_pick %||% "project"
    nav_link <- function(value, label) {
      shiny::tags$a(
        href = "#",
        class = paste("app-nav-link", if (identical(current, value)) "active"),
        onclick = sprintf(
          "event.preventDefault(); Shiny.setInputValue('nav_pick', '%s', {priority: 'event'});",
          value
        ),
        label
      )
    }
    shiny::div(
      class = "nav-sidebar",
      shiny::div(class = "nav-kicker", "Needed first"),
      nav_link("project", "Project"),
      shiny::div(class = "nav-kicker", "Then any of these"),
      nav_link("setup", "Setup"),
      nav_link("parameters", "Parameters"),
      nav_link("simulate", "Simulate"),
      nav_link("plots", "Plots"),
      nav_link("observe", "Objective function"),
      nav_link("sensitivity", "Sensitivity"),
      nav_link("calibrate", "Calibrate"),
      nav_link("design", "Design")
    )
  })

  project <- shiny::reactiveValues(
    daisy_exe = default_daisy_exe(),
    daisy_cmd = "",
    daisy_launch = "exe",
    project_dir = "",
    template_dir = "",
    run_file = "",
    config_path = "",
    config = NULL,
    config_yaml = "",
    config_ok = FALSE,
    config_msg = "No parameters loaded",
    config_tick = 0L,
    last_sim = NULL,
    last_sim_path = NULL,
    last_status = NULL,
    last_run_values = NULL,
    daisy_log_path = "",
    daisy_log_stamp = 0,
    run_busy = FALSE,
    last_score = NULL,
    log = character(),
    objectives = list(),
    obs_sets = NULL,
    active_of = NULL,
    objective = NULL,
    maximize = FALSE,
    cal_result = NULL,
    cal_check_score = NULL,
    sa_obj = NULL,
    sa_indices = NULL,
    calibrate_names = NULL,
    param_design = NULL,
    design_run = NULL,
    scored_design = NULL,
    metamodel = NULL,
    session_reset = 0L,
    session_load = 0L
  )

  shiny::observeEvent(input$nav_pick, {
    if (isTRUE(project$run_busy)) return()
    selected <- as.character(input$nav_pick %||% "project")
    tryCatch(
      shiny::updateTabsetPanel(session, "main_page", selected = selected),
      error = function(e) NULL
    )
  }, ignoreNULL = TRUE)

  mod_project_server("project", project)
  mod_setup_server("setup", project)
  mod_parameters_server("parameters", project)
  mod_simulate_server("simulate", project)
  mod_plots_server("plots", project)
  mod_observe_server("observe", project)
  mod_calibrate_server("calibrate", project)
  mod_sensitivity_server("sensitivity", project)
  mod_design_server("design", project)

  shiny::observeEvent(input$session_init, {
    if (isTRUE(project$run_busy)) {
      shiny::showNotification("Wait until the current Daisy run finishes.", type = "warning")
      return()
    }
    reset_project_session(project)
    tryCatch(
      shiny::updateTabsetPanel(session, "main_page", selected = "project"),
      error = function(e) NULL
    )
    session$sendCustomMessage("daisy-nav", "project")
    shiny::showNotification("Session initialised.", type = "message")
  })

  refresh_session_choices <- function(selected = NULL) {
    ch <- tryCatch(list_studio_sessions(project), error = function(e) character())
    if (!length(ch)) {
      shiny::updateSelectInput(session, "session_pick", choices = character())
      return(invisible(NULL))
    }
    sel <- selected %||% shiny::isolate(input$session_pick)
    if (is.null(sel) || !sel %in% ch) sel <- ch[[1]]
    shiny::updateSelectInput(session, "session_pick", choices = ch, selected = sel)
    invisible(NULL)
  }

  shiny::observe({
    project$project_dir
    refresh_session_choices()
  })

  shiny::observeEvent(input$session_save, {
    if (isTRUE(project$run_busy)) {
      shiny::showNotification("Wait until the current Daisy run finishes.", type = "warning")
      return()
    }
    tryCatch({
      stem <- safe_session_stem(input$session_name)
      path <- file.path(studio_session_home_dir(), paste0(stem, ".rds"))
      write_studio_session(path, snapshot_project_state(project), label = stem)
      shiny::updateTextInput(session, "session_name", value = stem)
      refresh_session_choices(selected = normalizePath(path, winslash = "/", mustWork = FALSE))
      log_append(project, "Saved session ", path)
      shiny::showNotification(paste("Saved session", stem), type = "message")
    }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
  })

  shiny::observeEvent(input$session_reload, {
    if (isTRUE(project$run_busy)) {
      shiny::showNotification("Wait until the current Daisy run finishes.", type = "warning")
      return()
    }
    tryCatch({
      pick <- as.character(input$session_pick %||% "")
      if (!nzchar(pick)) stop("Choose a saved session to load.")
      path <- resolve_studio_session_file(pick, project)
      st <- read_studio_session(path)
      apply_project_state(project, st)
      refresh_session_choices(selected = path)
      log_append(project, "Loaded session ", path)
      shiny::showNotification(paste("Loaded session", basename(path)), type = "message")
    }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
  })

  output$session_status <- shiny::renderUI({
    exe_ok <- daisy_launcher_ok(project)
    dir_ok <- nzchar(project$project_dir) && dir.exists(project$project_dir)
    daisy_label <- if (identical(project$daisy_launch %||% "exe", "cmd")) {
      "Command template"
    } else {
      basename(project$daisy_exe %||% "")
    }
    block <- function(kicker, ...) {
      shiny::div(class = "session-block", shiny::div(class = "session-kicker", kicker), ...)
    }
    shiny::div(
      class = "session-status",
      block(
        "Daisy",
        shiny::div(
          class = "session-row",
          shiny::span(class = "session-value", daisy_label),
          status_pill(exe_ok, "Ready", "Missing")
        )
      ),
      block(
        "Project",
        shiny::div(class = "session-value", project$project_dir %||% ""),
        status_pill(dir_ok, "Ready", "Set a folder")
      ),
      block(
        "Template",
        shiny::div(class = "session-value", if (nzchar(project$template_dir %||% "")) project$template_dir else "\u2014"),
        if (!nzchar(project$template_dir %||% "")) {
          status_pill(FALSE, "Set", "Not set")
        } else {
          status_pill(dir.exists(project$template_dir), "Ready", "Missing")
        }
      ),
      block(
        "Run file",
        shiny::div(class = "session-value", project$run_file %||% "\u2014")
      ),
      block(
        "Parameters",
        status_pill(project$config_ok, "Valid", "Not valid"),
        shiny::div(class = "session-value", project$config_msg)
      ),
      block(
        "Fit subset",
        shiny::div(
          class = "session-value",
          {
            cf <- project$calibrate_names
            if (is.null(cf) || !length(cf)) "All parameters"
            else paste(cf, collapse = ", ")
          }
        )
      ),
      block(
        "Objective",
        shiny::div(class = "session-value", project$active_of %||% "\u2014"),
        status_pill(!is.null(project$objective), "Saved", "Not set")
      ),
      block(
        "Last Daisy status",
        shiny::div(
          class = "session-value",
          if (is.null(project$last_status)) "\u2014" else as.character(project$last_status)
        )
      )
    )
  })
}
