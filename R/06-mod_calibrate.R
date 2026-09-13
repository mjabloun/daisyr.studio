mod_calibrate_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "calibrate-page",
    bslib::navset_tab(
      id = ns("cal_tabs"),
      bslib::nav_panel(
        title = "Fit",
        bslib::layout_columns(
          col_widths = c(4, 8),
          bslib::card(
            bslib::card_header("DEoptim calibration"),
            shiny::p(class = "step-hint",
              "Fits the selected parameters against the saved objective. Other parameters stay at their defaults. Each candidate is one Daisy run, so keep ",
              shiny::tags$code("itermax"), " and ", shiny::tags$code("NP"),
              " small until a trial finishes cleanly."
            ),
            shiny::numericInput(ns("itermax"), "Maximum iterations (itermax)", value = 15, min = 1, step = 1),
            shiny::numericInput(ns("NP"), "Population size (NP)", value = 10, min = 4, step = 2),
            shiny::selectInput(ns("of_name"), "Objective", choices = character()),
            shiny::selectizeInput(
              ns("param_names"),
              "Parameters to fit",
              choices = character(),
              multiple = TRUE,
              options = list(dropdownParent = "body", placeholder = "All parameters if empty")
            ),
            shiny::p(class = "step-hint",
              "Filled from Sensitivity when you use selected indices. A PLF curve is always fit as a unit. Leave empty to fit every parameter."
            ),
            shiny::actionButton(ns("go"), "Start calibration", class = "btn-primary")
          ),
          shiny::tagList(
            bslib::card(
              fill = FALSE,
              class = "calibrate-best-card",
              bslib::card_header("Best parameters"),
              DT::DTOutput(ns("best"), height = "auto"),
              shiny::verbatimTextOutput(ns("score"), placeholder = TRUE)
            ),
            bslib::card(
              fill = FALSE,
              class = "calibrate-progress-card",
              bslib::card_header("DEoptim progress"),
              shiny::p(class = "step-hint",
                "One line per generation (NP Daisy runs). The progress bar also updates after each evaluation."
              ),
              shiny::div(
                class = "daisy-run-progress",
                style = "display: none;",
                shiny::div(
                  class = "progress",
                  shiny::div(
                    id = ns("cal_bar"),
                    class = "progress-bar daisy-progress-fill",
                    role = "progressbar",
                    `aria-valuemin` = "0",
                    `aria-valuemax` = "100",
                    `aria-valuenow` = "0",
                    style = "width: 0%"
                  )
                ),
                shiny::span(id = ns("cal_bar_lab"), class = "step-hint daisy-run-progress-lab",
                            "No calibration running.")
              ),
              shiny::tags$pre(id = ns("de_trace"), class = "calibrate-de-trace", "No calibration running.")
            ),
            bslib::card(
              fill = FALSE,
              class = "calibrate-trace-card",
              bslib::card_header("Best-so-far score"),
              shiny::plotOutput(ns("traceplot"), height = "420px")
            )
          )
        )
      ),
      bslib::nav_panel(
        title = "Fit vs observations",
        bslib::layout_columns(
          col_widths = c(4, 8),
          bslib::card(
            fill = FALSE,
            bslib::card_header("Objective"),
            shiny::p(class = "step-hint",
              "Uses the saved objective's join, summary, mapped columns, and ",
              shiny::tags$code(".dlf"), ". Simulated line is the latest Daisy output (including the automatic best-parameter run)."
            ),
            shiny::selectInput(ns("of_view"), "Objective", choices = character()),
            shiny::selectInput(ns("of_pair"), "Series", choices = character()),
            shiny::verbatimTextOutput(ns("of_view_score"), placeholder = TRUE)
          ),
          bslib::card(
            fill = FALSE,
            bslib::card_header("Simulated vs observed"),
            shiny::plotOutput(ns("fit_obs_plot"), height = "460px")
          )
        )
      )
    )
  )
}

mod_calibrate_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      nms <- names(project$objectives %||% list())
      sel <- input$of_name %||% project$active_of
      if (!length(nms)) {
        shiny::updateSelectInput(session, "of_name", choices = character())
        shiny::updateSelectInput(session, "of_view", choices = character())
      } else {
        if (is.null(sel) || !sel %in% nms) sel <- project$active_of %||% nms[[1]]
        shiny::updateSelectInput(session, "of_name", choices = nms, selected = sel)
        view <- input$of_view
        if (is.null(view) || !view %in% nms) view <- sel
        shiny::updateSelectInput(session, "of_view", choices = nms, selected = view)
      }
    })

    shiny::observe({
      project$config_tick
      project$calibrate_names
      nms <- if (!is.null(project$config))
        param_config_names(project$config) else character()
      sel <- project$calibrate_names
      if (is.null(sel) || !length(sel)) sel <- nms
      sel <- intersect(as.character(sel), nms)
      shiny::updateSelectizeInput(session, "param_names", choices = nms, selected = sel)
    })

    shiny::observeEvent(input$go, {
      tryCatch({
        need_project(project)
        need_param_config(project)
        of <- pick_saved_objective(project, input$of_name)
        project$objective <- of$spec
        project$maximize <- isTRUE(of$maximize)
        if (!is.null(of$name) && nzchar(of$name)) project$active_of <- of$name
        if (!nzchar(project$run_file)) stop("Set the setup file to run on the Project page.")
        sim_file <- project$last_sim_path
        if (is.null(sim_file) || !file.exists(sim_file %||% "")) {
          files <- list_dlf_files(project$project_dir)
          if (!length(files)) stop("Run a simulation first so the objective knows which .dlf to read.")
          sim_file <- files[[1]]
        }
        nms_fit <- expand_calibrate_names(project$config, input$param_names)
        if (!length(nms_fit))
          stop("Select at least one parameter to fit (or run Sensitivity and send a subset).")
        project$calibrate_names <- nms_fit
        log_append(
          project, "Calibration started (", input$itermax, " iterations, NP=", input$NP,
          ", fitting ", length(nms_fit), " of ", length(param_config_names(project$config)),
          " names)."
        )
        shiny::showNotification("Calibration running \u2014 progress appears in DEoptim progress.", type = "message")
        session$sendCustomMessage("daisy-append-text", list(
          id = session$ns("de_trace"), reset = TRUE,
          line = paste0(
            "DEoptim  itermax=", as.integer(input$itermax),
            "  NP=", as.integer(input$NP),
            "  names=", paste(nms_fit, collapse = ",")
          )
        ))
        send_daisy_progress(session, session$ns("cal_bar"), session$ns("cal_bar_lab"),
                            0L, 1L, "eval")
        np <- as.integer(input$NP)
        itermax <- as.integer(input$itermax)
        with_nav_lock(session, project, function() {
          tryCatch({
            fit <- shiny::withProgress(message = "Calibrating (Daisy runs)", value = 0, {
          calibrate_daisy(
            config = project$config,
            run_file = project$run_file,
            daisy_exe = project$daisy_exe,
            cmd = daisy_cmd_of(project),
            objective = of$spec,
            sim_file = sim_file,
            working_dir = project$project_dir,
            template_dir = need_template_dir(project),
            output_dir = project$project_dir,
            show_log = FALSE,
            maximize = isTRUE(of$maximize),
            control = list(
              itermax = itermax,
              NP = np,
              trace = FALSE
            ),
            names = nms_fit,
            reporter = function(info) {
              shiny_daisy_progress(info$evals, info$n_total, "eval")
              send_daisy_progress(
                session, session$ns("cal_bar"), session$ns("cal_bar_lab"),
                info$evals, info$n_total, "eval",
                detail = sprintf(
                  "eval %d / %d   best = %s",
                  info$evals, info$n_total, signif(info$best_score, 5)
                )
              )
              if (!isTRUE(info$generation_end)) return()
              bp <- info$best_params
              if (length(bp) && length(nms_fit))
                bp <- bp[intersect(nms_fit, names(bp))]
              mem <- if (length(bp))
                paste(sprintf("%s=%s", names(bp), signif(as.numeric(bp), 4)), collapse = "  ")
              else ""
              session$sendCustomMessage("daisy-append-text", list(
                id = session$ns("de_trace"),
                reset = FALSE,
                line = sprintf(
                  "Iteration: %d  best_score: %s  %s",
                  info$iter, signif(info$best_score, 6), mem
                )
              ))
            }
          )
        })
        project$cal_result <- fit
        log_append(project, "Calibration finished. Best score = ", signif(fit$best_score, 5))
        hide_daisy_progress(session, session$ns("cal_bar"))
        tryCatch({
          session$sendCustomMessage("daisy-append-text", list(
            id = session$ns("de_trace"), reset = FALSE,
            line = "Running Daisy with best parameters..."
          ))
          shiny::withProgress(message = "Running best parameters", value = 0.4, {
            run_project_daisy(project, values = fit$best_params, show_log = TRUE)
          })
          log_append(project, "Best-parameter run finished with status ", project$last_status)
          chk <- tryCatch({
            path <- resolve_objective_sim_file(of$spec, project)
            evaluate_objective(of$spec, path)$summary
          }, error = function(e) NA_real_)
          project$cal_check_score <- chk
          if (!identical(project$last_status, 0L))
            shiny::showNotification("Calibration finished, but the best-parameter run returned a non-zero status.", type = "warning")
          else
            shiny::showNotification("Calibration finished. Open Fit vs observations to plot the best run.", type = "message")
        }, error = function(e) {
          log_append(project, "Best-parameter run failed: ", err_text(e))
          shiny::showNotification(
            paste("Calibration finished, but the best-parameter run failed:", err_text(e)),
            type = "error"
          )
        })
        hide_daisy_progress(session, session$ns("cal_bar"))
          }, error = function(e) {
            hide_daisy_progress(session, session$ns("cal_bar"))
            log_append(project, "Calibration failed: ", err_text(e))
            shiny::showNotification(err_text(e), type = "error")
          })
        })
      }, error = function(e) {
        hide_daisy_progress(session, session$ns("cal_bar"))
        log_append(project, "Calibration failed: ", err_text(e))
        shiny::showNotification(err_text(e), type = "error")
      })
    })

    output$best <- DT::renderDT({
      fit <- project$cal_result
      shiny::req(fit)
      fitted <- fit$fitted_names %||% names(fit$best_params)
      df <- data.frame(
        parameter = names(fit$best_params),
        value = as.numeric(fit$best_params),
        status = ifelse(names(fit$best_params) %in% fitted, "fitted", "frozen"),
        stringsAsFactors = FALSE
      )
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", paging = FALSE, scrollX = TRUE))
    })

    output$score <- shiny::renderText({
      fit <- project$cal_result
      if (is.null(fit)) return("No calibration yet.")
      lines <- paste0("best_score = ", signif(fit$best_score, 6),
             if (isTRUE(project$maximize)) " (metric maximised; stored as the optimiser saw it)" else "")
      nfit <- length(fit$fitted_names %||% names(fit$best_params))
      nall <- length(fit$best_params)
      if (nfit < nall)
        lines <- paste0(lines, "\nfitted ", nfit, " of ", nall, " parameters (rest frozen at default)")
      chk <- project$cal_check_score
      if (!is.null(chk) && length(chk) && is.finite(chk[[1]]))
        lines <- paste0(lines, "\nthis .dlf = ", signif(chk[[1]], 6),
                        "  (objective on the best-parameter run)")
      lines
    })

    output$traceplot <- shiny::renderPlot({
      fit <- project$cal_result
      shiny::req(fit)
      member <- fit$fit$member
      y <- member$bestvalit
      if (is.null(y)) y <- member$bestval.iter
      if (is.null(y) || !length(y)) {
        y <- fit$best_score
      } else if (isTRUE(project$maximize)) {
        y <- -as.numeric(y)
      }
      y <- as.numeric(y)
      shiny::req(length(y) > 0, all(is.finite(y)))
      graphics::plot(
        seq_along(y), y,
        type = if (length(y) == 1L) "p" else "l",
        pch = 16, lwd = 2, col = "#1b5e40",
        xlab = "Iteration", ylab = "Best score",
        main = NULL
      )
      graphics::grid()
    })

    fit_obs_err <- shiny::reactiveVal("")

    fit_obs_plots <- shiny::reactive({
      project$last_status
      project$last_sim_path
      project$objectives
      nm <- input$of_view %||% input$of_name
      if (!nzchar(nm %||% "") || !nm %in% names(project$objectives %||% list())) {
        fit_obs_err("")
        return(list())
      }
      tryCatch({
        plots <- objective_fit_series(project$objectives[[nm]]$spec, project)
        if (!length(plots))
          stop("No series to plot. Check the objective mapping, join columns, and .dlf.")
        fit_obs_err("")
        plots
      }, error = function(e) {
        fit_obs_err(err_text(e))
        list()
      })
    })

    shiny::observe({
      plots <- fit_obs_plots()
      nms <- names(plots)
      if (!length(nms)) {
        shiny::updateSelectInput(session, "of_pair", choices = character())
        return()
      }
      sel <- input$of_pair
      if (is.null(sel) || !sel %in% nms) sel <- nms[[1]]
      shiny::updateSelectInput(session, "of_pair", choices = nms, selected = sel)
    })

    output$of_view_score <- shiny::renderText({
      project$last_status
      project$last_sim_path
      nm <- input$of_view %||% ""
      if (!nzchar(nm) || !nm %in% names(project$objectives %||% list()))
        return("Save a named objective on the Objective function page.")
      of <- project$objectives[[nm]]
      path <- tryCatch(resolve_objective_sim_file(of$spec, project), error = function(e) "")
      if (!nzchar(path) || !file.exists(path))
        return("No .dlf found yet. Calibrate (or run Daisy) first.")
      res <- tryCatch(
        evaluate_objective(of$spec, path, return_per_pair = TRUE),
        error = function(e) e
      )
      if (inherits(res, "error")) return(err_text(res))
      mets <- tryCatch({
        comps <- objective_component_list(of$spec)
        unique(vapply(comps, function(comp) {
          m <- comp$metric
          if (is.null(m) || !length(m) || is.na(m[[1]])) "" else as.character(m[[1]])
        }, character(1)))
      }, error = function(e) character())
      mets <- mets[nzchar(mets)]
      paste0(
        if (length(mets)) paste0("metric = ", paste(mets, collapse = ", "), "\n") else "",
        "summary = ", signif(res$summary, 6),
        if (!is.null(project$cal_result))
          paste0("\nDEoptim best_score = ", signif(project$cal_result$best_score, 6)),
        if (nzchar(fit_obs_err())) paste0("\n\nPlot: ", fit_obs_err()) else ""
      )
    })

    output$fit_obs_plot <- shiny::renderPlot({
      plots <- fit_obs_plots()
      shiny::req(length(plots) > 0)
      key <- input$of_pair
      if (is.null(key) || !key %in% names(plots)) key <- names(plots)[[1]]
      print(plots[[key]])
    })
  })
}
