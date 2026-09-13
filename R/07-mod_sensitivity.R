mod_sensitivity_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Sensitivity test"),
      shiny::p(class = "step-hint",
        "Choose a test, then Daisy is run for every row of that design. Response is the saved objective score, or the mean of one output column."
      ),
      shiny::radioButtons(
        ns("method"),
        "Which test to run",
        choiceNames = list(
          shiny::tagList(shiny::strong("Morris"), shiny::span(" \u2014 elementary effects (screening)", class = "step-hint")),
          shiny::tagList(shiny::strong("Sobol"), shiny::span(" \u2014 first-order and total indices", class = "step-hint"))
        ),
        choiceValues = list("morris", "sobol"),
        selected = "morris"
      ),
      shiny::conditionalPanel(
        condition = "input.method == 'morris'",
        ns = ns,
        shiny::numericInput(ns("r"), "Trajectories (r)", value = 4, min = 2, step = 1),
        shiny::numericInput(ns("levels"), "Levels", value = 4, min = 2, step = 1)
      ),
      shiny::conditionalPanel(
        condition = "input.method == 'sobol'",
        ns = ns,
        shiny::numericInput(ns("N"), "Base sample size (N)", value = 8, min = 2, step = 1),
        shiny::p(class = "step-hint",
          "Sobol-Jansen needs about N \u00d7 (k + 2) Daisy runs for k parameters. Keep N small for a trial."
        )
      ),
      shiny::numericInput(ns("seed"), "Seed", value = 1, min = 1, step = 1),
      shiny::selectInput(
        ns("plot_type"),
        "Plot",
        choices = c("\u03bc* vs \u03c3 (Campolongo)" = "effects", "Ranked \u03bc*" = "bar"),
        selected = "effects"
      ),
      shiny::radioButtons(ns("response"), "Response",
                          c("Saved objective score" = "objective",
                            "Mean of a simulated column" = "column")),
      shiny::conditionalPanel(
        condition = "input.response == 'objective'",
        ns = ns,
        shiny::selectInput(ns("of_name"), "Objective", choices = character())
      ),
      shiny::conditionalPanel(
        condition = "input.response == 'column'",
        ns = ns,
        shiny::textInput(ns("resp_col"), "Simulated column", value = "tocalibrate")
      ),
      shiny::actionButton(ns("go"), "Run Morris analysis", class = "btn-primary")
    ),
    shiny::tagList(
      bslib::card(
        bslib::card_header("Indices"),
        shiny::p(class = "step-hint",
          "Select rows to calibrate (a PLF curve is kept as a unit). Suggested names have \u03bc* or ST at least 10% of the maximum. Then send them to Calibrate."
        ),
        shiny::div(
          class = "d-flex gap-2 flex-wrap mb-2",
          shiny::actionButton(ns("select_suggested"), "Select suggested", class = "btn-sm btn-outline-secondary"),
          shiny::actionButton(ns("use_for_cal"), "Use selected for calibration", class = "btn-sm btn-primary")
        ),
        DT::DTOutput(ns("table"))
      ),
      bslib::card(
        bslib::card_header("Plot"),
        shiny::plotOutput(ns("plot"), height = "360px")
      )
    )
  )
}

mod_sensitivity_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::updateActionButton(
        session, "go",
        label = if (identical(input$method, "sobol")) "Run Sobol analysis" else "Run Morris analysis"
      )
    })

    shiny::observe({
      if (identical(input$method, "sobol")) {
        shiny::updateSelectInput(
          session, "plot_type",
          choices = c(
            "First-order vs total (bar)" = "bar",
            "Ranked total index" = "ranked"
          ),
          selected = "bar"
        )
      } else {
        shiny::updateSelectInput(
          session, "plot_type",
          choices = c(
            "\u03bc* vs \u03c3 (Campolongo)" = "effects",
            "Ranked \u03bc*" = "bar"
          ),
          selected = "effects"
        )
      }
    })

    shiny::observe({
      nms <- names(project$objectives %||% list())
      sel <- input$of_name %||% project$active_of
      if (!length(nms)) {
        shiny::updateSelectInput(session, "of_name", choices = character())
      } else {
        if (is.null(sel) || !sel %in% nms) sel <- project$active_of %||% nms[[1]]
        shiny::updateSelectInput(session, "of_name", choices = nms, selected = sel)
      }
    })

    shiny::observeEvent(input$go, {
      tryCatch({
        need_project(project)
        need_param_config(project)
        if (!nzchar(project$run_file)) stop("Set the setup file to run on the Project page.")
        files <- list_dlf_files(project$project_dir)
        if (!length(files) && is.null(project$last_sim_path))
          stop("Run one simulation first so the output .dlf path is known.")
        out_path <- project$last_sim_path %||% files[[1]]
        if (length(files))
          output_files <- named_output_files(files, project, relative = FALSE)
        else
          output_files <- named_output_files(out_path, project, relative = FALSE)
        method <- input$method
        design <- if (identical(method, "morris")) {
          sa_design(
            project$config, method = "morris",
            r = as.integer(input$r),
            design = list(type = "oat", levels = as.integer(input$levels),
                          grid.jump = max(1L, as.integer(input$levels) %/% 2L)),
            seed = as.integer(input$seed)
          )
        } else {
          sa_design(
            project$config, method = "sobol",
            N = as.integer(input$N),
            seed = as.integer(input$seed)
          )
        }
        n_runs <- nrow(as.matrix(design$X))
        log_append(project, "SA design ", method, " with ", n_runs, " Daisy runs.")
        shiny::showNotification(paste("Running", n_runs, "Daisy evaluations."), type = "message")

        resp_mode <- input$response
        resp_col <- trimws(input$resp_col)
        out_name <- names(output_files)[[1]]

        if (identical(resp_mode, "objective")) {
          of <- pick_saved_objective(project, input$of_name)
          obj <- of$spec
          output_reader <- read_dlf
          response_fun <- function(outputs) {
            evaluate_objective(obj, sim_file = out_path, sim_outputs = outputs)$summary
          }
        } else {
          output_reader <- read_dlf
          response_fun <- function(outputs) {
            dt <- data.table::as.data.table(outputs[[out_name]])
            if (!resp_col %in% names(dt))
              stop("Column '", resp_col, "' not in simulation output.")
            mean(dt[[resp_col]], na.rm = TRUE)
          }
        }

        sa_run <- NULL
        with_nav_lock(session, project, function() {
          tryCatch({
            sa_run <- shiny::withProgress(message = paste("Sensitivity:", n_runs, "runs"), value = 0, {
              run_sa_design(
                design, project$config,
                run_file = project$run_file,
                daisy_exe = project$daisy_exe,
                cmd = daisy_cmd_of(project),
                output_files = output_files,
                response_fun = response_fun,
                working_dir = project$project_dir,
                template_dir = need_template_dir(project),
                output_dir = project$project_dir,
                show_log = FALSE,
                output_reader = output_reader,
                progress = function(i, n) shiny_daisy_progress(i, n, "Daisy run")
              )
            })
            sa <- complete_sa_analysis(design, sa_run$responses)
            project$sa_obj <- sa
            project$sa_indices <- sa_index_table(sa)
            sug <- tryCatch(
              suggest_calibrate_names(project$sa_indices, project$config),
              error = function(e) character()
            )
            if (length(sug)) project$calibrate_names <- sug
            log_append(project, "Sensitivity analysis finished.")
            shiny::showNotification("Sensitivity analysis finished. Review the table, then use selected parameters for calibration.", type = "message")
          }, error = function(e) {
            log_append(project, "SA failed: ", err_text(e))
            shiny::showNotification(err_text(e), type = "error")
          })
        })
      }, error = function(e) {
        log_append(project, "SA failed: ", err_text(e))
        shiny::showNotification(err_text(e), type = "error")
      })
    })

    output$table <- DT::renderDT({
      idx <- project$sa_indices
      shiny::req(idx)
      df <- as.data.frame(idx)
      sug <- character()
      if (!is.null(project$config)) {
        sug <- tryCatch(
          suggest_calibrate_names(df, project$config),
          error = function(e) character()
        )
      }
      selected <- which(as.character(df$name) %in% sug)
      DT::datatable(
        df, rownames = FALSE,
        selection = list(mode = "multiple", selected = selected),
        options = list(dom = "t", scrollX = TRUE)
      )
    })

    shiny::observeEvent(input$select_suggested, {
      idx <- project$sa_indices
      if (is.null(idx) || !NROW(idx) || is.null(project$config)) return()
      sug <- suggest_calibrate_names(idx, project$config)
      DT::selectRows(DT::dataTableProxy("table"), which(as.character(idx$name) %in% sug))
    })

    shiny::observeEvent(input$use_for_cal, {
      tryCatch({
        need_param_config(project)
        idx <- project$sa_indices
        if (is.null(idx) || !NROW(idx)) stop("Run a sensitivity analysis first.")
        rows <- as.integer(input$table_rows_selected)
        if (!length(rows)) stop("Select at least one row in the indices table.")
        nms <- as.character(idx$name[rows])
        project$calibrate_names <- expand_calibrate_names(project$config, nms)
        log_append(
          project, "Calibration names: ",
          paste(project$calibrate_names, collapse = ", ")
        )
        shiny::showNotification(
          paste("Calibrate will fit", length(project$calibrate_names), "parameter(s)."),
          type = "message"
        )
      }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    output$plot <- shiny::renderPlot({
      sa <- project$sa_obj
      shiny::req(sa)
      type <- input$plot_type %||% if (inherits(sa, "morris")) "effects" else "bar"
      if (inherits(sa, "morris")) {
        plot_sa(sa, type = if (type %in% c("effects", "bar")) type else "effects")
      } else {
        plot_sa(sa, type = if (type %in% c("bar", "ranked")) type else "bar")
      }
    })
  })
}
