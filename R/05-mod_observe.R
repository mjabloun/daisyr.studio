mod_obs_set_ui <- function(id) {
  ns <- shiny::NS(id)
  section <- function(step, title, hint, ...) {
    shiny::div(
      class = "obs-section",
      shiny::div(
        class = "obs-section-head",
        shiny::span(class = "obs-section-step", step),
        shiny::span(class = "obs-section-title", title)
      ),
      if (!is.null(hint)) shiny::p(class = "step-hint obs-section-hint", hint),
      ...
    )
  }
  shiny::div(
    class = "obs-set-body",
    section(
      "1", "Name this dataset",
      "Used when you build a named objective (yield, SWC, biomass, \u2026).",
      shiny::textInput(ns("tag"), NULL, placeholder = "e.g. yield")
    ),
    section(
      "2", "Load observations",
      "Upload a table, or type a path inside the project folder.",
      shiny::div(
        class = "obs-inline-row",
        shiny::div(
          class = "obs-inline-grow",
          shiny::textInput(ns("obs_path"), "Path", width = "100%",
                           placeholder = "e.g. observed/yield.txt")
        ),
        shiny::div(
          class = "obs-inline-btn",
          shiny::actionButton(ns("load_path"), "Load from path", class = "btn-outline-primary")
        )
      ),
      shiny::fileInput(ns("upload"), "Or upload a file", accept = c(".csv", ".txt", ".tsv"))
    ),
    section(
      "3", "Preview loaded table",
      NULL,
      DT::DTOutput(ns("preview"))
    ),
    section(
      "4", "Match to a Daisy output",
      "Join this table to one simulation log, then choose the goodness-of-fit metric.",
      bslib::layout_columns(
        col_widths = c(5, 4, 3),
        shiny::selectInput(ns("sim_file"), "Simulated .dlf", choices = character()),
        shiny::selectizeInput(
          ns("join_cols"), "Join columns",
          choices = character(),
          multiple = TRUE,
          selected = "Date",
          options = list(
            dropdownParent = "body",
            placeholder = "Shared columns"
          )
        ),
        shiny::selectInput(ns("metric"), "Metric", c("RMSE", "KGE", "MAE"))
      ),
      shiny::p(class = "step-hint",
        "Optional: summarise the simulated table before the join, like Plots (",
        shiny::tags$code("group_by"), " + ", shiny::tags$code("summarise"),
        "). Leave the function empty to skip."
      ),
      bslib::layout_columns(
        col_widths = c(6, 4, 2),
        class = "obs-summary-row",
        shiny::selectizeInput(
          ns("summary_groups"), "Group by",
          choices = character(),
          multiple = TRUE,
          selected = character(),
          options = list(dropdownParent = "body", placeholder = "e.g. Date")
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
          class = "obs-summary-clear",
          shiny::actionButton(ns("summary_clear"), "Clear summary", class = "btn-sm btn-outline-secondary")
        )
      )
    ),
    section(
      "5", "Map observed columns to simulated columns",
      "Pick columns from the loaded table and the .dlf, then add a pair. Label and weight are optional.",
      bslib::layout_columns(
        col_widths = c(4, 4, 2, 2),
        shiny::selectizeInput(
          ns("obs_col_pick"), "Observed column",
          choices = character(),
          options = list(dropdownParent = "body", placeholder = "e.g. stem_DM")
        ),
        shiny::selectizeInput(
          ns("sim_col_pick"), "Simulated column",
          choices = character(),
          options = list(dropdownParent = "body", placeholder = "e.g. stem_DM")
        ),
        shiny::textInput(ns("pair_label"), "Label", placeholder = "e.g. grain yield"),
        shiny::numericInput(ns("pair_weight"), "Weight", value = 1, min = 0, step = 0.1)
      ),
      shiny::div(
        class = "d-flex gap-2 flex-wrap column-map-actions",
        shiny::actionButton(ns("add_row"), "Add pair", class = "btn-sm btn-primary"),
        shiny::actionButton(ns("remove_row"), "Remove pair", class = "btn-sm btn-outline-danger")
      ),
      shiny::div(class = "column-map-wrap", DT::DTOutput(ns("map"), height = "auto"))
    )
  )
}

mod_obs_set_server <- function(id, sets, set_id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    map_df <- shiny::reactiveVal(empty_obs_map())
    ready <- shiny::reactiveVal(FALSE)

    patch <- function(...) {
      all <- sets()
      if (!set_id %in% names(all)) return()
      dots <- list(...)
      for (nm in names(dots)) all[[set_id]][[nm]] <- dots[[nm]]
      sets(all)
    }

    shiny::observeEvent(TRUE, {
      s <- sets()[[set_id]]
      shiny::req(s)
      map_df(s$map)
      shiny::updateTextInput(session, "tag", value = s$tag)
      shiny::updateTextInput(session, "obs_path", value = s$path %||% "")
      shiny::updateSelectInput(session, "metric", selected = s$metric %||% "RMSE")
      shiny::updateSelectizeInput(session, "summary_fun", selected = s$summary_fun %||% "")
      ready(TRUE)
    }, once = TRUE)

    shiny::observeEvent(project$session_reset, {
      s <- sets()[[set_id]]
      if (is.null(s)) return()
      map_df(s$map)
      shiny::updateTextInput(session, "tag", value = s$tag)
      shiny::updateTextInput(session, "obs_path", value = s$path %||% "")
      shiny::updateSelectInput(session, "metric", selected = s$metric %||% "RMSE")
      shiny::updateSelectizeInput(session, "summary_fun", selected = s$summary_fun %||% "")
      shiny::updateSelectizeInput(session, "summary_groups", selected = s$summary_groups %||% character())
      shiny::updateSelectizeInput(session, "join_cols", selected = split_csv(s$join_cols %||% "Date"))
      shiny::updateSelectizeInput(session, "obs_col_pick", choices = character(), selected = character())
      shiny::updateSelectizeInput(session, "sim_col_pick", choices = character(), selected = character())
      shiny::updateTextInput(session, "pair_label", value = "")
      shiny::updateNumericInput(session, "pair_weight", value = 1)
    }, ignoreInit = TRUE, priority = 0)

    shiny::observeEvent(project$session_load, {
      s <- sets()[[set_id]]
      if (is.null(s)) return()
      map_df(s$map)
      shiny::updateTextInput(session, "tag", value = s$tag)
      shiny::updateTextInput(session, "obs_path", value = s$path %||% "")
      shiny::updateSelectInput(session, "metric", selected = s$metric %||% "RMSE")
      shiny::updateSelectizeInput(session, "summary_fun", selected = s$summary_fun %||% "")
      shiny::updateSelectizeInput(session, "summary_groups", selected = s$summary_groups %||% character())
      shiny::updateSelectizeInput(session, "join_cols", selected = split_csv(s$join_cols %||% "Date"))
    }, ignoreInit = TRUE, priority = 0)

    refresh_dlf <- function() {
      files <- list_dlf_files(project$project_dir)
      labs <- if (length(files)) vapply(files, function(p) rel_output_path(project, p), character(1)) else character()
      s <- sets()[[set_id]]
      sel <- s$sim_file %||% ""
      if (!nzchar(sel) || !sel %in% files)
        sel <- if (length(files)) files[[1]] else ""
      shiny::updateSelectInput(session, "sim_file",
                               choices = stats::setNames(files, labs),
                               selected = sel)
    }

    shiny::observe({
      project$project_dir
      project$last_status
      if (isTRUE(ready())) refresh_dlf()
    })

    ingest <- function(path) {
      dt <- data.table::fread(path)
      if ("Date" %in% names(dt) && !inherits(dt$Date, "Date")) {
        parsed <- as.Date(dt$Date)
        if (all(is.na(parsed))) parsed <- as.Date(dt$Date, format = "%Y/%m/%d")
        dt[, Date := parsed]
      }
      patch(data = dt, path = path)
      log_append(project, "Loaded observations '", sets()[[set_id]]$tag, "' from ", path,
                 " (", nrow(dt), " rows).")
      dt
    }

    shiny::observeEvent(input$upload, {
      shiny::req(input$upload)
      tryCatch(ingest(input$upload$datapath),
               error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    shiny::observeEvent(input$load_path, {
      tryCatch({
        rel <- trimws(input$obs_path)
        if (!nzchar(rel)) stop("Give a file path.")
        path <- if (file.exists(rel)) rel else file.path(project$project_dir, rel)
        ingest(path)
      }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    shiny::observeEvent(input$tag, {
      if (!isTRUE(ready())) return()
      patch(tag = trimws(input$tag))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$join_cols, {
      if (!isTRUE(ready())) return()
      jc <- as.character(input$join_cols %||% character())
      jc <- jc[nzchar(jc)]
      patch(join_cols = if (length(jc)) paste(jc, collapse = ", ") else "")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$metric, {
      if (!isTRUE(ready())) return()
      patch(metric = input$metric)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$summary_groups, {
      if (!isTRUE(ready())) return()
      patch(summary_groups = as.character(input$summary_groups %||% character()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$summary_fun, {
      if (!isTRUE(ready())) return()
      patch(summary_fun = as.character(input$summary_fun %||% ""))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$summary_clear, {
      shiny::updateSelectizeInput(session, "summary_groups", selected = character())
      shiny::updateSelectizeInput(session, "summary_fun", selected = "")
    })

    shiny::observeEvent(input$sim_file, {
      if (!isTRUE(ready())) return()
      patch(sim_file = input$sim_file %||% "")
    }, ignoreInit = TRUE)

    shiny::observeEvent(map_df(), {
      if (!isTRUE(ready())) return()
      patch(map = map_df())
    }, ignoreInit = TRUE)

    shiny::observe({
      if (!isTRUE(ready())) return()
      dt <- sets()[[set_id]]$data
      obs_nms <- if (!is.null(dt)) names(dt) else character()
      join <- split_csv(input$join_cols %||% "Date")
      obs_nms <- setdiff(obs_nms, join)
      shiny::updateSelectizeInput(
        session, "obs_col_pick",
        choices = obs_nms,
        selected = intersect(shiny::isolate(input$obs_col_pick) %||% character(), obs_nms),
        server = TRUE
      )
    })

    shiny::observe({
      if (!isTRUE(ready())) return()
      path <- input$sim_file %||% ""
      raw_nms <- character()
      if (nzchar(path) && file.exists(path)) {
        raw_nms <- tryCatch({
          names(add_date(read_dlf(path), basename(path)))
        }, error = function(e) character())
      }
      join <- split_csv(input$join_cols %||% "Date")
      sim_nms <- setdiff(drop_calendar_names(raw_nms), join)
      shiny::updateSelectizeInput(
        session, "sim_col_pick",
        choices = sim_nms,
        selected = intersect(shiny::isolate(input$sim_col_pick) %||% character(), sim_nms),
        server = TRUE
      )
      s <- sets()[[set_id]]
      prev_g <- shiny::isolate(input$summary_groups) %||% character()
      if (!length(prev_g)) prev_g <- s$summary_groups %||% character()
      sel_g <- intersect(prev_g, raw_nms)
      shiny::updateSelectizeInput(
        session, "summary_groups",
        choices = raw_nms,
        selected = sel_g
      )
      obs_dt <- sets()[[set_id]]$data
      obs_nms <- if (!is.null(obs_dt)) names(obs_dt) else character()
      shared <- intersect(obs_nms, raw_nms)
      prev_j <- split_csv(paste(shiny::isolate(input$join_cols) %||% character(), collapse = ","))
      if (!length(prev_j)) prev_j <- split_csv(s$join_cols %||% "")
      sel_j <- intersect(prev_j, shared)
      if (!length(sel_j) && "Date" %in% shared) sel_j <- "Date"
      shiny::updateSelectizeInput(
        session, "join_cols",
        choices = shared,
        selected = sel_j
      )
    })

    output$map <- DT::renderDT({
      DT::datatable(
        map_df(),
        editable = list(target = "cell", disable = list(columns = c(0, 1))),
        selection = "single",
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE, autoWidth = TRUE)
      )
    }, fillContainer = FALSE)

    shiny::observeEvent(input$map_cell_edit, {
      info <- input$map_cell_edit
      df <- map_df()
      j <- info$col + 1L
      df[info$row, j] <- DT::coerceValue(info$value, df[info$row, j])
      map_df(df)
    })

    shiny::observeEvent(input$add_row, {
      obs <- trimws(input$obs_col_pick %||% "")
      sim <- trimws(input$sim_col_pick %||% "")
      if (!nzchar(obs) || !nzchar(sim)) {
        shiny::showNotification("Choose an observed column and a simulated column.", type = "warning")
        return()
      }
      df <- map_df()
      if (nrow(df) && any(df$obs_col == obs & df$sim_col == sim)) {
        shiny::showNotification("That pair is already in the map.", type = "warning")
        return()
      }
      lab <- trimws(input$pair_label %||% "")
      if (!nzchar(lab)) lab <- obs
      w <- suppressWarnings(as.numeric(input$pair_weight))
      if (!length(w) || !is.finite(w)) w <- 1
      map_df(rbind(
        df,
        data.frame(obs_col = obs, sim_col = sim, label = lab, weight = w,
                   stringsAsFactors = FALSE)
      ))
    })

    shiny::observeEvent(input$remove_row, {
      df <- map_df()
      sel <- input$map_rows_selected
      if (!nrow(df) || !length(sel)) {
        shiny::showNotification("Select a mapping row to remove.", type = "warning")
        return()
      }
      map_df(df[-as.integer(sel), , drop = FALSE])
    })

    output$preview <- DT::renderDT({
      dt <- sets()[[set_id]]$data
      shiny::req(dt)
      DT::datatable(as.data.frame(dt), rownames = FALSE,
                    options = list(scrollX = TRUE, pageLength = 8))
    })
  })
}

mod_observe_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(8, 4),
    shiny::tagList(
      bslib::card(
        fill = FALSE,
        class = "obs-data-card",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center gap-2 flex-wrap",
          shiny::span("Observations"),
          shiny::div(
            class = "d-flex gap-2",
            shiny::actionButton(ns("add_obs"), "Add observation", class = "btn-sm btn-outline-secondary"),
            shiny::actionButton(ns("remove_obs"), "Remove tab", class = "btn-sm btn-outline-danger")
          )
        ),
        shiny::tabsetPanel(
          id = ns("obs_nav"),
          type = "tabs",
          shiny::tabPanel(
            title = "obs",
            value = "obs_main",
            mod_obs_set_ui(ns("obs_main"))
          )
        )
      ),
      bslib::card(
        bslib::card_header("Score"),
        shiny::p(class = "step-hint",
          "Quick check of the mapping on this page. Uses the observation tabs you have set up (or the tabs listed under Named objectives if any are selected). Save is only needed for Calibrate / Sensitivity / Design."
        ),
        shiny::actionButton(ns("score"), "Score current simulation", class = "btn-outline-primary"),
        shiny::verbatimTextOutput(ns("result"), placeholder = TRUE),
        shiny::plotOutput(ns("fitplot"), height = "280px")
      )
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Save objective function"),
      shiny::p(class = "step-hint",
        "Each objective has a tag used on Calibrate, Sensitivity, and Design. ",
        "It is a weighted mean of one or more observation tabs (different files, joins, metrics, and .dlf logs)."
      ),
      shiny::textInput(ns("of_name"), "Tag / name", placeholder = "e.g. yield, all, swc"),
      shiny::selectizeInput(
        ns("of_members"), "Observation tabs",
        choices = character(), multiple = TRUE,
        options = list(placeholder = "e.g. yield, SWC")
      ),
      shiny::textInput(ns("of_weights"), "Weights (comma-separated, same order)",
                       placeholder = "e.g. 1, 1"),
      shiny::checkboxInput(ns("maximize"), "Treat combined score as maximise (e.g. KGE)", value = FALSE),
      shiny::div(
        class = "d-flex gap-2 flex-wrap",
        shiny::actionButton(ns("save_of"), "Save Objective", class = "btn-primary"),
        shiny::actionButton(ns("remove_of"), "Remove selected", class = "btn-outline-danger")
      ),
      shiny::selectInput(ns("active_of"), "Active objective", choices = character()),
      shiny::verbatimTextOutput(ns("of_list"), placeholder = TRUE)
    )
  )
}

mod_observe_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    first <- new_obs_set("obs", id = "obs_main")
    sets <- shiny::reactiveVal(stats::setNames(list(first), first$id))
    started <- shiny::reactiveVal("obs_main")
    last_plot <- shiny::reactiveVal(NULL)
    score_text <- shiny::reactiveVal("Map columns, then score. Saving the objective is only needed later for calibration.")

    shiny::observe({
      project$obs_sets <- sets()
    })

    shiny::observeEvent(project$session_reset, {
      extra <- setdiff(names(sets()), "obs_main")
      for (sid in extra) {
        tryCatch(
          shiny::removeTab("obs_nav", target = sid, session = session),
          error = function(e) NULL
        )
      }
      first_set <- new_obs_set("obs", id = "obs_main")
      sets(stats::setNames(list(first_set), first_set$id))
      started("obs_main")
      last_plot(NULL)
      score_text("Map columns, then score. Saving the objective is only needed later for calibration.")
      shiny::updateTextInput(session, "of_name", value = "")
      shiny::updateSelectizeInput(session, "of_members", choices = character(), selected = character())
      shiny::updateTextInput(session, "of_weights", value = "")
      shiny::updateCheckboxInput(session, "maximize", value = FALSE)
      shiny::updateSelectInput(session, "active_of", choices = character())
    }, ignoreInit = TRUE, priority = 10)

    shiny::observeEvent(project$session_load, {
      extra <- setdiff(names(sets()), "obs_main")
      for (sid in extra) {
        tryCatch(
          shiny::removeTab("obs_nav", target = sid, session = session),
          error = function(e) NULL
        )
      }
      saved <- project$obs_sets
      if (is.null(saved) || !length(saved)) {
        saved <- stats::setNames(list(new_obs_set("obs", id = "obs_main")), "obs_main")
      } else if (!"obs_main" %in% names(saved)) {
        nms <- names(saved)
        first <- saved[[1]]
        first$id <- "obs_main"
        rest <- saved[-1]
        names(rest) <- nms[-1]
        saved <- c(stats::setNames(list(first), "obs_main"), rest)
      }
      sets(saved)
      started_ids <- started()
      for (sid in setdiff(names(saved), "obs_main")) {
        s <- saved[[sid]]
        shiny::insertTab(
          inputId = "obs_nav",
          tab = shiny::tabPanel(
            title = s$tag %||% sid,
            value = sid,
            mod_obs_set_ui(session$ns(sid))
          ),
          select = FALSE,
          session = session
        )
        if (!sid %in% started_ids) {
          started_ids <- c(started_ids, sid)
          mod_obs_set_server(sid, sets, sid, project)
        }
      }
      started(started_ids)
      last_plot(NULL)
      nms <- names(project$objectives %||% list())
      shiny::updateSelectInput(
        session, "active_of",
        choices = nms,
        selected = if (length(nms)) project$active_of %||% nms[[1]] else character()
      )
    }, ignoreInit = TRUE, priority = 10)

    mod_obs_set_server("obs_main", sets, "obs_main", project)

    shiny::observe({
      tags <- vapply(sets(), function(s) s$tag, character(1))
      tags <- tags[nzchar(tags)]
      shiny::updateSelectizeInput(
        session, "of_members",
        choices = stats::setNames(tags, tags),
        selected = intersect(shiny::isolate(input$of_members) %||% character(), tags)
      )
    })

    shiny::observe({
      nms <- names(project$objectives %||% list())
      sel <- project$active_of %||% input$active_of
      if (!length(nms)) {
        shiny::updateSelectInput(session, "active_of", choices = character())
      } else {
        if (is.null(sel) || !sel %in% nms) sel <- nms[[1]]
        shiny::updateSelectInput(session, "active_of", choices = nms, selected = sel)
      }
    })

    shiny::observeEvent(input$active_of, {
      nm <- input$active_of
      if (!nzchar(nm %||% "") || !nm %in% names(project$objectives %||% list())) return()
      project$active_of <- nm
      of <- project$objectives[[nm]]
      project$objective <- of$spec
      project$maximize <- isTRUE(of$maximize)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$add_obs, {
      all <- sets()
      n <- length(all) + 1L
      s <- new_obs_set(paste0("obs", n))
      all[[s$id]] <- s
      sets(all)
      shiny::insertTab(
        inputId = "obs_nav",
        tab = shiny::tabPanel(
          title = s$tag,
          value = s$id,
          mod_obs_set_ui(session$ns(s$id))
        ),
        select = TRUE,
        session = session
      )
      if (!s$id %in% started()) {
        started(c(started(), s$id))
        mod_obs_set_server(s$id, sets, s$id, project)
      }
    })

    shiny::observeEvent(input$remove_obs, {
      all <- sets()
      if (length(all) <= 1L) {
        shiny::showNotification("Keep at least one observation tab.", type = "warning")
        return()
      }
      sid <- input$obs_nav
      if (is.null(sid) || !sid %in% names(all)) sid <- names(all)[[length(all)]]
      if (identical(sid, "obs_main") && length(all) > 1L) {
        # allow removing the first tab if others exist
      }
      all[[sid]] <- NULL
      sets(all)
      shiny::removeTab("obs_nav", target = sid, session = session)
    })

    parse_weights <- function(n) {
      raw <- trimws(input$of_weights %||% "")
      if (!nzchar(raw)) return(rep(1, n))
      w <- suppressWarnings(as.numeric(split_csv(raw)))
      if (length(w) != n || any(!is.finite(w)))
        stop("Weights must be ", n, " numeric values, in the same order as the observation tabs.")
      w
    }

    shiny::observeEvent(input$save_of, {
      tryCatch({
        name <- trimws(input$of_name)
        if (!nzchar(name)) stop("Give the objective a tag/name before saving.")
        spec <- compile_named_objective(
          name, unname(sets()), input$of_members, parse_weights(length(input$of_members))
        )
        of <- list(
          name = name,
          members = as.character(input$of_members),
          weights = parse_weights(length(input$of_members)),
          maximize = isTRUE(input$maximize),
          spec = spec
        )
        objs <- project$objectives %||% list()
        objs[[name]] <- of
        project$objectives <- objs
        project$active_of <- name
        project$objective <- spec
        project$maximize <- isTRUE(of$maximize)
        shiny::updateSelectInput(session, "active_of", choices = names(objs), selected = name)
        log_append(project, "Objective '", name, "' saved (", paste(of$members, collapse = "+"), ").")
        shiny::showNotification(paste0("Saved objective '", name, "'."), type = "message")
      }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    shiny::observeEvent(input$remove_of, {
      nm <- input$active_of
      objs <- project$objectives %||% list()
      if (!nzchar(nm %||% "") || !nm %in% names(objs)) {
        shiny::showNotification("Select an objective to remove.", type = "warning")
        return()
      }
      objs[[nm]] <- NULL
      project$objectives <- objs
      if (length(objs)) {
        nxt <- names(objs)[[1]]
        project$active_of <- nxt
        project$objective <- objs[[nxt]]$spec
        project$maximize <- isTRUE(objs[[nxt]]$maximize)
      } else {
        project$active_of <- NULL
        project$objective <- NULL
        project$maximize <- FALSE
      }
    })

    output$of_list <- shiny::renderText({
      objs <- project$objectives %||% list()
      if (!length(objs)) return("No named objectives yet.")
      lines <- vapply(names(objs), function(nm) {
        of <- objs[[nm]]
        w <- paste(of$weights, collapse = ", ")
        dir <- if (isTRUE(of$maximize)) "max" else "min"
        paste0(nm, "  [", dir, "]  ", paste(of$members, collapse = " + "), "  (w=", w, ")")
      }, character(1))
      paste(lines, collapse = "\n")
    })

    shiny::observeEvent(input$score, {
      tryCatch({
        all_sets <- unname(sets())
        members <- as.character(input$of_members %||% character())
        members <- members[nzchar(members)]
        if (length(members)) {
          spec <- compile_named_objective(
            input$of_name, all_sets, members, parse_weights(length(members))
          )
        } else {
          sid <- input$obs_nav
          ids <- names(sets())
          if (is.null(sid) || !sid %in% ids) sid <- ids[[1]]
          set <- sets()[[sid]]
          tag <- set$tag %||% "obs"
          spec <- composite_objective(
            stats::setNames(list(obs_set_to_spec(set)), tag),
            weights = 1,
            name = tag
          )
        }
        path <- resolve_objective_sim_file(spec, project)
        if (!nzchar(path) || !file.exists(path))
          stop("Choose a simulated .dlf on the observation tab (or run Daisy so Output/ has a log).")
        res <- evaluate_objective(spec, path, return_per_pair = TRUE, plot = TRUE)
        project$last_score <- res
        last_plot(res$plot)
        metric_lab <- {
          if (inherits(spec, "daisyr_composite_objective")) {
            mets <- vapply(spec$components, function(comp) {
              m <- comp$metric %||% ""
              if (length(m) && !is.na(m)) as.character(m)[[1]] else ""
            }, character(1))
            nms <- names(spec$components)
            uniq <- unique(mets[nzchar(mets)])
            if (length(uniq) == 1L) uniq
            else if (length(mets)) paste(sprintf("%s=%s", nms, mets), collapse = ", ")
            else ""
          } else {
            spec$metric %||% ""
          }
        }
        bits <- paste0(
          "objective = ", spec$name %||% "draft",
          if (nzchar(metric_lab)) paste0("\nmetric = ", metric_lab) else "",
          "\nsummary = ", signif(res$summary, 5)
        )
        if (!is.null(res$per_component))
          bits <- paste0(bits, "\n\n", paste(utils::capture.output(print(res$per_component)), collapse = "\n"))
        if (!is.null(res$per_variable))
          bits <- paste0(bits, "\n\n", paste(utils::capture.output(print(res$per_variable)), collapse = "\n"))
        score_text(bits)
        log_append(project, "Draft score for '", spec$name %||% "draft", "' = ", signif(res$summary, 5))
      }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    output$result <- shiny::renderText({
      score_text()
    })

    output$fitplot <- shiny::renderPlot({
      plots <- last_plot()
      shiny::req(plots)
      print(plots[[1]])
    })
  })
}
