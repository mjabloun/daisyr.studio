mod_plots_ui <- function(id, show_obs = FALSE) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "plots-page",
    bslib::layout_columns(
      col_widths = c(4, 8),
      shiny::div(
        class = "plots-left-col",
        bslib::card(
          fill = FALSE,
          class = "plots-read-card",
          bslib::card_header("Read Daisy output"),
          shiny::p(class = "step-hint",
            "Choose a ", shiny::tags$code(".dlf"), " log from the project (usually under ",
            shiny::tags$code("Output/"), ") after a simulation."
          ),
          shiny::selectInput(ns("dlf"), "Output file (.dlf)", choices = character()),
          shiny::div(
            class = "d-flex gap-2 flex-wrap",
            shiny::actionButton(ns("reload_dlf"), "Refresh file list", class = "btn-outline-secondary"),
            shiny::actionButton(ns("read"), "Read selected file", class = "btn-primary")
          )
        ),
        if (isTRUE(show_obs)) bslib::card(
          fill = FALSE,
          class = "plots-obs-card",
          bslib::card_header("Observed values"),
          shiny::p(class = "step-hint",
            "Overlay a series loaded on Objective function (red points). X uses the plot X column if it exists in the observations."
          ),
          shiny::selectizeInput(
            ns("obs_set"), "Observation set",
            choices = c("(none)" = ""),
            options = list(dropdownParent = "body")
          ),
          shiny::selectizeInput(
            ns("obs_y"), "Observed column",
            choices = c("(none)" = ""),
            options = list(dropdownParent = "body")
          )
        ),
        bslib::card(
          fill = FALSE,
          class = "plots-filter-card",
          bslib::card_header("Filter"),
          shiny::p(class = "step-hint",
            "Keep rows that match one column condition. Leave the column empty to show all data."
          ),
          shiny::selectizeInput(
            ns("filter_col"), "Column",
            choices = c("(none)" = ""),
            options = list(dropdownParent = "body")
          ),
          shiny::div(
            class = "plots-filter-row",
            shiny::div(
              class = "plots-filter-op",
              shiny::selectizeInput(
                ns("filter_op"), "Operator",
                choices = c("<" = "<", ">" = ">", "=" = "=", "<=" = "<=", ">=" = ">=", "between" = "between"),
                selected = "=",
                options = list(dropdownParent = "body")
              )
            ),
            shiny::div(
              class = "plots-filter-val",
              shiny::conditionalPanel(
                condition = "input.filter_op != 'between'",
                ns = ns,
                shiny::textInput(ns("filter_value"), "Value", value = "", placeholder = "e.g. 0")
              ),
              shiny::conditionalPanel(
                condition = "input.filter_op == 'between'",
                ns = ns,
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  shiny::textInput(ns("filter_from"), "From", value = "", placeholder = "e.g. 0"),
                  shiny::textInput(ns("filter_to"), "To", value = "", placeholder = "e.g. 10")
                )
              )
            )
          ),
          shiny::div(
            class = "d-flex gap-2 align-items-center flex-wrap",
            shiny::actionButton(ns("filter_clear"), "Clear filter", class = "btn-outline-secondary"),
            shiny::span(class = "step-hint", shiny::textOutput(ns("filter_status"), inline = TRUE))
          )
        ),
        bslib::card(
          fill = FALSE,
          class = "plots-summary-card",
          bslib::card_header("Summary"),
          shiny::p(class = "step-hint",
            "Like dplyr ", shiny::tags$code("group_by"), " + ", shiny::tags$code("summarise"),
            ". Leave the function empty to skip."
          ),
          shiny::selectizeInput(
            ns("summary_groups"), "Group by",
            choices = character(),
            multiple = TRUE,
            options = list(dropdownParent = "body", placeholder = "e.g. year, crop")
          ),
          shiny::selectizeInput(
            ns("summary_fun"), "Function",
            choices = c(
              "(none)" = "",
              "mean" = "mean",
              "sum" = "sum",
              "min" = "min",
              "max" = "max",
              "median" = "median",
              "sd" = "sd",
              "n" = "n"
            ),
            selected = "",
            options = list(dropdownParent = "body")
          ),
          shiny::div(
            class = "d-flex gap-2 align-items-center flex-wrap",
            shiny::actionButton(ns("summary_clear"), "Clear summary", class = "btn-outline-secondary"),
            shiny::span(class = "step-hint", shiny::textOutput(ns("summary_status"), inline = TRUE))
          )
        )
      ),
      shiny::div(
        class = "plots-output-card",
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Plot",
            shiny::div(
              class = "plots-plot-pane",
              shiny::div(class = "plots-plot-wrap", shiny::plotOutput(ns("plot"), height = "420px")),
              shiny::div(
                class = "plots-mapping",
                bslib::layout_columns(
                  col_widths = c(4, 8),
                  shiny::selectizeInput(ns("xcol"), "X axis", choices = character(), options = list(dropdownParent = "body", placeholder = "e.g. Date")),
                  shiny::selectizeInput(ns("ycols"), "Y axis", choices = NULL, multiple = TRUE, options = list(dropdownParent = "body", placeholder = "e.g. stem_DM"))
                ),
                bslib::layout_columns(
                  col_widths = c(4, 4, 4),
                  shiny::selectizeInput(
                    ns("geom"), "Plot type",
                    choices = c("Line" = "line", "Scatter" = "scatter", "Bar" = "bar", "Boxplot" = "boxplot"),
                    selected = "line",
                    options = list(dropdownParent = "body")
                  ),
                  shiny::selectizeInput(ns("fillcol"), "Fill / colour", choices = c("(none)" = ""), options = list(dropdownParent = "body")),
                  shiny::selectizeInput(ns("facetcol"), "Facet", choices = c("(none)" = ""), options = list(dropdownParent = "body"))
                )
              )
            )
          ),
          bslib::nav_panel(
            title = "Table",
            shiny::div(
              class = "plots-table-pane",
              shiny::div(
                class = "plots-table-digits",
                shiny::numericInput(
                  ns("table_digits"), "Set digits",
                  value = 3, min = 0, max = 10, step = 1, width = "7rem"
                )
              ),
              DT::DTOutput(ns("table"))
            )
          )
        )
      )
    )
  )
}

mod_plots_server <- function(id, project, show_obs = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    current_dt <- shiny::reactiveVal(NULL)
    display_names <- shiny::reactiveVal(NULL)

    refresh_dlf <- function() {
      files <- list_dlf_files(project$project_dir)
      labs <- if (length(files)) vapply(files, function(p) rel_output_path(project, p), character(1)) else character()
      sel <- if (!is.null(project$last_sim_path) && project$last_sim_path %in% files)
        project$last_sim_path
      else if (length(files)) files[[1]]
      else NULL
      shiny::updateSelectInput(session, "dlf", choices = stats::setNames(files, labs), selected = sel)
    }

    shiny::observeEvent(input$reload_dlf, refresh_dlf())
    shiny::observe({
      project$project_dir
      project$last_status
      refresh_dlf()
    })

    shiny::observeEvent(project$session_reset, {
      current_dt(NULL)
      display_names(NULL)
      shiny::updateSelectInput(session, "dlf", choices = character())
      shiny::updateSelectInput(session, "xcol", choices = character())
      shiny::updateSelectizeInput(session, "ycols", choices = character(), selected = character())
      shiny::updateSelectizeInput(session, "geom", selected = "line")
      shiny::updateSelectInput(session, "fillcol", choices = c("(none)" = ""), selected = "")
      shiny::updateSelectInput(session, "facetcol", choices = c("(none)" = ""), selected = "")
      shiny::updateSelectizeInput(session, "filter_col", choices = c("(none)" = ""), selected = "")
      shiny::updateSelectizeInput(session, "filter_op", selected = "=")
      shiny::updateTextInput(session, "filter_value", value = "")
      shiny::updateTextInput(session, "filter_from", value = "")
      shiny::updateTextInput(session, "filter_to", value = "")
      shiny::updateSelectizeInput(session, "summary_groups", choices = character(), selected = character())
      shiny::updateSelectizeInput(session, "summary_fun", selected = "")
      shiny::updateNumericInput(session, "table_digits", value = 3)
      if (isTRUE(show_obs)) {
        shiny::updateSelectizeInput(session, "obs_set", choices = c("(none)" = ""), selected = "")
        shiny::updateSelectizeInput(session, "obs_y", choices = c("(none)" = ""), selected = "")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$read, {
      tryCatch({
        path <- input$dlf
        if (is.null(path) || !nzchar(path) || !file.exists(path))
          stop("Choose an existing .dlf file.")
        reader <- make_sim_reader(FALSE)
        dt <- reader(path)
        current_dt(dt)
        project$last_sim <- dt
        project$last_sim_path <- path
        cols <- names(dt)
        y_cols <- numeric_plot_cols(dt)
        x_default <- if ("Date" %in% cols) "Date" else cols[[1]]
        none <- c("(none)" = "")
        shiny::updateSelectInput(session, "xcol", choices = cols, selected = x_default)
        shiny::updateSelectizeInput(
          session, "ycols", choices = y_cols,
          selected = utils::head(y_cols, 1)
        )
        shiny::updateSelectInput(session, "fillcol", choices = c(none, cols), selected = "")
        shiny::updateSelectInput(session, "facetcol", choices = c(none, cols), selected = "")
        prev <- input$filter_col
        shiny::updateSelectizeInput(
          session, "filter_col",
          choices = c("(none)" = "", stats::setNames(cols, cols)),
          selected = if (!is.null(prev) && prev %in% cols) prev else ""
        )
        shiny::updateSelectizeInput(
          session, "summary_groups",
          choices = cols,
          selected = intersect(input$summary_groups %||% character(), cols)
        )
        log_append(project, "Read ", path, " (", nrow(dt), " rows).")
      }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    filtered_dt <- shiny::reactive({
      dt <- current_dt()
      shiny::req(dt)
      filter_dt_rows(
        dt,
        input$filter_col,
        input$filter_op,
        if (identical(input$filter_op, "between")) input$filter_from else input$filter_value,
        input$filter_to
      )
    })

    display_dt <- shiny::reactive({
      summarise_dt(
        filtered_dt(),
        input$summary_groups,
        input$summary_fun
      )
    })

    shiny::observe({
      dt <- display_dt()
      shiny::req(dt)
      nms <- names(dt)
      if (identical(nms, display_names())) return()
      display_names(nms)
      shiny::isolate({
        y_cols <- numeric_plot_cols(dt)
        none <- c("(none)" = "")
        groups <- intersect(input$summary_groups %||% character(), nms)
        x_default <- if (length(groups)) groups[[1]]
        else if ("Date" %in% nms) "Date"
        else nms[[1]]
        y_keep <- intersect(input$ycols %||% character(), y_cols)
        shiny::updateSelectInput(session, "xcol", choices = nms, selected = x_default)
        shiny::updateSelectizeInput(
          session, "ycols", choices = y_cols,
          selected = if (length(y_keep)) y_keep else utils::head(y_cols, 1)
        )
        shiny::updateSelectInput(session, "fillcol", choices = c(none, nms), selected = "")
        shiny::updateSelectInput(session, "facetcol", choices = c(none, nms), selected = "")
      })
    })

    shiny::observeEvent(input$filter_clear, {
      shiny::updateSelectizeInput(session, "filter_col", selected = "")
      shiny::updateSelectizeInput(session, "filter_op", selected = "=")
      shiny::updateTextInput(session, "filter_value", value = "")
      shiny::updateTextInput(session, "filter_from", value = "")
      shiny::updateTextInput(session, "filter_to", value = "")
    })

    shiny::observeEvent(input$summary_clear, {
      shiny::updateSelectizeInput(session, "summary_groups", selected = character())
      shiny::updateSelectizeInput(session, "summary_fun", selected = "")
    })

    if (isTRUE(show_obs)) {
      shiny::observe({
        sets <- obs_sets_with_data(project$obs_sets)
        tags <- vapply(sets, function(s) s$tag, character(1))
        ids <- vapply(sets, function(s) s$id, character(1))
        choices <- c("(none)" = "")
        if (length(ids)) choices <- c(choices, stats::setNames(ids, tags))
        prev <- shiny::isolate(input$obs_set)
        sel <- if (!is.null(prev) && prev %in% ids) prev else ""
        shiny::updateSelectizeInput(session, "obs_set", choices = choices, selected = sel)
      })

      shiny::observe({
        sid <- input$obs_set %||% ""
        sets <- project$obs_sets %||% list()
        none <- c("(none)" = "")
        if (!nzchar(sid) || !sid %in% names(sets) || is.null(sets[[sid]]$data)) {
          shiny::updateSelectizeInput(session, "obs_y", choices = none, selected = "")
          return()
        }
        dt <- sets[[sid]]$data
        cols <- names(dt)
        y_cols <- numeric_plot_cols(dt)
        pick <- if (length(y_cols)) y_cols else cols
        map <- sets[[sid]]$map
        sim_y <- shiny::isolate(input$ycols) %||% character()
        default <- ""
        if (!is.null(map) && nrow(map) && length(sim_y)) {
          hit <- match(sim_y[[1]], map$sim_col)
          if (!is.na(hit) && nzchar(map$obs_col[[hit]])) default <- map$obs_col[[hit]]
        }
        prev <- shiny::isolate(input$obs_y)
        sel <- if (!is.null(prev) && prev %in% pick) prev
        else if (nzchar(default) && default %in% pick) default
        else if (length(pick)) pick[[1]]
        else ""
        shiny::updateSelectizeInput(
          session, "obs_y",
          choices = c(none, stats::setNames(pick, pick)),
          selected = sel
        )
      })
    }

    output$filter_status <- shiny::renderText({
      raw <- current_dt()
      if (is.null(raw)) return("No data loaded.")
      shown <- filtered_dt()
      n_raw <- NROW(raw)
      n_shown <- NROW(shown)
      if (identical(n_shown, n_raw)) paste0(n_raw, " rows")
      else paste0(n_shown, " of ", n_raw, " rows")
    })

    output$summary_status <- shiny::renderText({
      dt <- display_dt()
      if (is.null(dt)) return("")
      fun <- input$summary_fun %||% ""
      if (!nzchar(fun)) return("No summary")
      paste0(NROW(dt), " grouped rows")
    })

    output$plot <- shiny::renderPlot({
      dt <- display_dt()
      shiny::req(dt)
      y <- input$ycols
      shiny::req(length(y) > 0)
      obs_dt <- NULL
      obs_y <- NULL
      if (isTRUE(show_obs)) {
        sid <- input$obs_set %||% ""
        sets <- project$obs_sets %||% list()
        if (nzchar(sid) && sid %in% names(sets)) {
          obs_dt <- sets[[sid]]$data
          obs_y <- input$obs_y %||% ""
        }
      }
      plot_series(
        dt, y,
        x_col = input$xcol,
        geom = input$geom %||% "line",
        fill_col = input$fillcol,
        facet_col = input$facetcol,
        title = basename(project$last_sim_path %||% ""),
        obs_dt = obs_dt,
        obs_x = input$xcol,
        obs_y = obs_y
      )
    })

    output$table <- DT::renderDT({
      dt <- display_dt()
      shiny::req(dt)
      digits <- as.integer(input$table_digits %||% 3L)
      if (!is.finite(digits) || digits < 0L) digits <- 3L
      df <- as.data.frame(dt)
      tbl <- DT::datatable(
        df, rownames = FALSE,
        options = list(
          scrollX = TRUE,
          scrollY = "28rem",
          pageLength = 50,
          deferRender = TRUE
        )
      )
      num_cols <- drop_calendar_names(names(df)[vapply(df, function(x) {
        is.numeric(x) && !inherits(x, c("Date", "POSIXt", "difftime"))
      }, logical(1))])
      if (length(num_cols))
        tbl <- DT::formatRound(tbl, columns = num_cols, digits = digits)
      tbl
    })
  })
}
