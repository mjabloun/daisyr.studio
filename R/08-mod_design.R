mod_design_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Space-filling design"),
      shiny::p(class = "step-hint",
        "Draw a training design over parameter bounds, run Daisy for every row, optionally score with the saved objective, and fit a Kriging metamodel."
      ),
      shiny::numericInput(ns("n"), "Design points (n)", value = 10, min = 2, step = 1),
      shiny::selectInput(ns("method"), "Method", c("lhs", "sobol", "dice_lhs")),
      shiny::numericInput(ns("seed"), "Seed", value = 1, min = 1, step = 1),
      shiny::textInput(ns("keep_dir"), "Archive directory (inside project)", value = "metamodel_runs"),
      shiny::div(
        class = "d-flex gap-2 flex-wrap",
        shiny::actionButton(ns("draw"), "Draw design", class = "btn-outline-primary"),
        shiny::actionButton(ns("run"), "Run Daisy on design", class = "btn-primary")
      ),
      shiny::hr(),
      shiny::selectInput(ns("of_name"), "Objective", choices = character()),
      shiny::actionButton(ns("score"), "Score with objective", class = "btn-outline-secondary"),
      shiny::actionButton(ns("fit"), "Fit Kriging metamodel", class = "btn-outline-secondary")
    ),
    shiny::tagList(
      bslib::card(
        bslib::card_header("Design / scores"),
        shiny::div(
          class = "plots-table-digits",
          shiny::numericInput(
            ns("table_digits"), "Set digits",
            value = 3, min = 0, max = 10, step = 1, width = "7rem"
          )
        ),
        DT::DTOutput(ns("table"))
      ),
      bslib::card(
        bslib::card_header("Metamodel"),
        shiny::verbatimTextOutput(ns("meta"), placeholder = TRUE)
      )
    )
  )
}

mod_design_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
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

    shiny::observeEvent(input$draw, {
      tryCatch({
        need_param_config(project)
        des <- generate_param_design(
          project$config,
          n = as.integer(input$n),
          method = input$method,
          seed = as.integer(input$seed)
        )
        project$param_design <- des
        log_append(project, "Drew ", nrow(des), " design points (", input$method, ").")
        shiny::showNotification("Design drawn.", type = "message")
      }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    shiny::observeEvent(input$run, {
      tryCatch({
        need_project(project)
        need_param_config(project)
        if (is.null(project$param_design)) stop("Draw a design first.")
        files <- list_dlf_files(project$project_dir)
        out_path <- project$last_sim_path %||% if (length(files)) files[[1]] else ""
        if (!nzchar(out_path)) stop("Run one simulation first so the output .dlf path is known.")
        output_files <- named_output_files(if (length(files)) files else out_path, project, relative = TRUE)
        keep <- file.path(project$project_dir, trimws(input$keep_dir))
        n <- nrow(project$param_design)
        log_append(project, "Running param design (", n, " Daisy jobs).")
        with_nav_lock(session, project, function() {
          tryCatch({
            run <- shiny::withProgress(message = paste("Design runs:", n), value = 0, {
              run_param_design(
                project$param_design, project$config,
                run_file = project$run_file,
                daisy_exe = project$daisy_exe,
                cmd = daisy_cmd_of(project),
                output_files = output_files,
                working_dir = project$project_dir,
                template_dir = need_template_dir(project),
                output_dir = project$project_dir,
                keep_files = TRUE,
                keep_dir = keep,
                read = TRUE,
                output_reader = read_dlf,
                progress = function(i, n) shiny_daisy_progress(i, n, "Daisy run")
              )
            })
            project$design_run <- run
            log_append(project, "Design runs archived in ", keep)
            shiny::showNotification("Design runs finished.", type = "message")
          }, error = function(e) {
            log_append(project, "Design run failed: ", err_text(e))
            shiny::showNotification(err_text(e), type = "error")
          })
        })
      }, error = function(e) {
        log_append(project, "Design run failed: ", err_text(e))
        shiny::showNotification(err_text(e), type = "error")
      })
    })

    shiny::observeEvent(input$score, {
      tryCatch({
        of <- pick_saved_objective(project, input$of_name)
        if (is.null(project$design_run)) stop("Run the design through Daisy first.")
        scored <- score_param_design(
          project$design_run, of$spec,
          maximize = isTRUE(of$maximize)
        )
        project$scored_design <- scored
        log_append(project, "Scored design with '", of$name %||% input$of_name, "' (", nrow(scored), " rows).")
        shiny::showNotification("Design scored.", type = "message")
      }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    shiny::observeEvent(input$fit, {
      tryCatch({
        need_param_config(project)
        if (is.null(project$scored_design)) stop("Score the design first.")
        mm <- fit_metamodel(project$scored_design, project$config)
        project$metamodel <- mm
        log_append(project, "Fitted Kriging metamodel.")
        shiny::showNotification("Metamodel fitted.", type = "message")
      }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    output$table <- DT::renderDT({
      df <- project$scored_design
      if (is.null(df) && !is.null(project$design_run)) df <- project$design_run$design
      if (is.null(df)) df <- project$param_design
      shiny::req(df)
      digits <- as.integer(input$table_digits %||% 3L)
      if (!is.finite(digits) || digits < 0L) digits <- 3L
      df <- as.data.frame(df)
      tbl <- DT::datatable(df, rownames = FALSE,
                    options = list(scrollX = TRUE, pageLength = 10))
      num_cols <- names(df)[vapply(df, function(x) {
        is.numeric(x) && !inherits(x, c("Date", "POSIXt", "difftime"))
      }, logical(1))]
      if (length(num_cols))
        tbl <- DT::formatRound(tbl, columns = num_cols, digits = digits)
      tbl
    })

    output$meta <- shiny::renderText({
      mm <- project$metamodel
      if (is.null(mm)) return("Fit a metamodel after scoring to see a summary here.")
      paste(utils::capture.output(print(mm)), collapse = "\n")
    })
  })
}
