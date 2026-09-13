mod_parameters_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        fill = FALSE,
        bslib::card_header("Parameters file"),
        shiny::p(class = "step-hint",
          "Lists every free parameter Daisy will vary. Load an existing ",
          shiny::tags$code("parameters.yaml"),
          " (older ", shiny::tags$code("registry.yaml"),
          " files still work), or build the tables on the right and save."
        ),
        shiny::textInput(ns("path"), "Parameters file", width = "100%",
                         placeholder = "e.g. parameters.yaml"),
        shiny::div(
          class = "d-flex gap-2 flex-wrap",
          shiny::actionButton(ns("load"), "Load YAML", class = "btn-outline-primary"),
          shiny::actionButton(ns("save"), "Save YAML", class = "btn-primary"),
          shiny::actionButton(ns("validate"), "Validate", class = "btn-outline-secondary")
        ),
        shiny::verbatimTextOutput(ns("status"), placeholder = TRUE),
        shiny::hr(),
        shiny::h6("Placeholders in templates"),
        shiny::p(class = "step-hint",
          "Names found as ", shiny::tags$code("{{...}}"), " in the template folder. Select a row and add it. Names ending in ",
          shiny::tags$code("_plf"), " become a PLF (shape parameters plus a curve row). Change the curve family in PLF curves."
        ),
        shiny::actionButton(ns("add_selected"), "Add selected", class = "btn-sm btn-outline-primary mb-2"),
        DT::DTOutput(ns("placeholders"))
      ),
      shiny::tagList(
        bslib::card(
          fill = FALSE,
          bslib::card_header("Parameters"),
          shiny::p(class = "step-hint",
            "One row per free parameter. Double-click a cell to edit. Add names only from Placeholders in templates. Select a row and click Remove parameter to delete it."
          ),
          shiny::div(
            class = "d-flex gap-2 flex-wrap mb-2",
            shiny::actionButton(ns("remove_param"), "Remove parameter", class = "btn-sm btn-outline-danger")
          ),
          DT::DTOutput(ns("params"))
        ),
        shiny::conditionalPanel(
          condition = "output.has_plf",
          ns = ns,
          bslib::card(
            fill = FALSE,
            bslib::card_header("PLF curves"),
            shiny::p(class = "step-hint",
              "Filled when you add a ", shiny::tags$code("_plf"), " placeholder. Double-click the curve cell to change family (shape parameters update to match). Select a row and click Remove curve to delete it."
            ),
            shiny::div(
              class = "d-flex gap-2 flex-wrap mb-2",
              shiny::actionButton(ns("remove_curve"), "Remove curve", class = "btn-sm btn-outline-danger")
            ),
            DT::DTOutput(ns("curves"))
          )
        )
      )
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("parameters.yaml"),
      shiny::p(class = "step-hint",
        "This is the file daisyr reads. Copy the tables into YAML, then Validate or Save. Editing YAML here does not change the tables until you Load or Validate."
      ),
      shiny::actionButton(ns("tables_to_yaml"), "Tables \u2192 YAML", class = "btn-outline-secondary"),
      yaml_editor_ui(ns("yaml"),
                     placeholder = "e.g. paste parameters YAML here",
                     height = "320px")
    )
  )
}

mod_parameters_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    params_df <- shiny::reactiveVal(params_to_df(NULL))
    curves_df <- shiny::reactiveVal(curves_to_df(NULL))
    suppress_yaml <- shiny::reactiveVal(FALSE)

    output$has_plf <- shiny::reactive(nrow(curves_df()) > 0)
    shiny::outputOptions(output, "has_plf", suspendWhenHidden = FALSE)

    sync_from_param_config <- function() {
      params_df(params_to_df(project$config))
      curves_df(curves_to_df(project$config))
      if (nzchar(project$config_yaml)) {
        suppress_yaml(TRUE)
        update_yaml_editor(session, "yaml", project$config_yaml)
      }
      if (nzchar(project$config_path)) {
        shiny::updateTextInput(session, "path", value = basename(project$config_path))
      }
    }

    shiny::observeEvent(project$config_tick, {
      sync_from_param_config()
    }, ignoreNULL = FALSE)

    shiny::observeEvent(project$session_reset, {
      params_df(params_to_df(NULL))
      curves_df(curves_to_df(NULL))
      update_yaml_editor(session, "yaml", "")
      shiny::updateTextInput(session, "path", value = "")
    }, ignoreInit = TRUE)

    output$params <- DT::renderDT({
      DT::datatable(
        params_df(),
        editable = TRUE,
        rownames = FALSE,
        selection = "single",
        options = list(dom = "t", scrollX = TRUE, paging = FALSE)
      )
    })

    output$curves <- DT::renderDT({
      df <- curves_df()
      if (nrow(df)) {
        fam <- plf_curve_families()
        extra <- setdiff(as.character(df$curve), fam)
        df$curve <- factor(as.character(df$curve), levels = c(fam, extra))
      }
      DT::datatable(
        df,
        editable = TRUE,
        rownames = FALSE,
        selection = "single",
        options = list(dom = "t", scrollX = TRUE, paging = FALSE)
      )
    })

    shiny::observeEvent(input$params_cell_edit, {
      info <- input$params_cell_edit
      df <- params_df()
      j <- info$col + 1L
      df[info$row, j] <- DT::coerceValue(info$value, df[info$row, j])
      params_df(df)
    })

    shiny::observeEvent(input$curves_cell_edit, {
      info <- input$curves_cell_edit
      df <- curves_df()
      j <- info$col + 1L
      colname <- names(df)[j]
      old_params <- split_csv(df[info$row, "params"])
      old_curve <- as.character(df[info$row, "curve"])
      df[info$row, j] <- DT::coerceValue(info$value, df[info$row, j])
      if (identical(colname, "curve")) {
        new_curve <- trimws(as.character(df[info$row, "curve"]))
        if (!new_curve %in% plf_curve_families()) {
          df[info$row, "curve"] <- old_curve
          curves_df(df)
          shiny::showNotification(
            paste("Unknown curve family. Use one of:", paste(plf_curve_families(), collapse = ", ")),
            type = "warning"
          )
          return()
        }
        curve_nm <- as.character(df[info$row, "name"])
        new_params <- plf_shape_names(curve_nm, new_curve)
        df[info$row, "params"] <- paste(new_params, collapse = ", ")
        curves_df(df)
        params_df(replace_param_names(params_df(), old_params, new_params))
        shiny::showNotification(paste("Updated shape parameters for", curve_nm), type = "message")
      } else {
        curves_df(df)
      }
    })

    output$placeholders <- DT::renderDT({
      td <- template_dir_of(project)
      hits <- find_dai_placeholders(td)
      DT::datatable(
        hits,
        rownames = FALSE,
        selection = "single",
        options = list(dom = "t", pageLength = 8, scrollY = "160px", paging = FALSE)
      )
    })

    param_config_file_path <- function() {
      rel <- trimws(input$path)
      if (!nzchar(rel)) {
        if (!nzchar(project$project_dir)) stop("Set a project directory first.")
        rel <- if (file.exists(file.path(project$project_dir, "parameters.yaml")))
          "parameters.yaml"
        else if (file.exists(file.path(project$project_dir, "registry.yaml")))
          "registry.yaml"
        else
          "parameters.yaml"
      }
      if (grepl("^[A-Za-z]:|^/|^\\\\", rel) || grepl("^[A-Za-z]:/", rel)) return(rel)
      if (!nzchar(project$project_dir)) stop("Set a project directory first.")
      file.path(project$project_dir, rel)
    }

    shiny::observeEvent(input$load, {
      tryCatch({
        path <- param_config_file_path()
        if (!file.exists(path)) stop("File not found: ", path)
        yaml_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
        apply_param_config(project, yaml_text, path = path)
        shiny::showNotification("Parameters loaded.", type = "message")
      }, error = function(e) {
        project$config_ok <- FALSE
        project$config_msg <- err_text(e)
        shiny::showNotification(err_text(e), type = "error")
      })
    })

    selected_placeholder <- function() {
      hits <- find_dai_placeholders(template_dir_of(project))
      i <- input$placeholders_rows_selected
      if (is.null(i) || !length(i) || !nrow(hits)) {
        shiny::showNotification("Select a placeholder in the table.", type = "warning")
        return(NULL)
      }
      hits[as.integer(i)[[1]], ]
    }

    shiny::observeEvent(input$add_selected, {
      row <- selected_placeholder()
      if (is.null(row)) return()
      ph <- as.character(row$placeholder)
      if (is_plf_placeholder(ph)) {
        curve_nm <- plf_name_from_placeholder(ph)
        if (!nzchar(curve_nm)) {
          shiny::showNotification("Placeholder is only _plf; give it a name before the suffix.", type = "warning")
          return()
        }
        cdf <- curves_df()
        if (curve_nm %in% cdf$name || ph %in% cdf$placeholder) {
          shiny::showNotification("That PLF is already in the tables.", type = "warning")
          return()
        }
        curve <- "logistic"
        shape <- plf_shape_names(curve_nm, curve)
        cr <- empty_curve_row(curve_nm, curve, paste(shape, collapse = ", "))
        cr$placeholder <- ph
        cr$from_file <- row$file
        cr$to_file <- row$file
        curves_df(rbind(cdf, cr))
        params_df(replace_param_names(params_df(), character(), shape))
        shiny::showNotification(paste("Added PLF", curve_nm, "(logistic). Change the family in PLF curves if needed."), type = "message")
        return()
      }
      df <- params_df()
      if (ph %in% df$name) {
        shiny::showNotification("That name is already in the table.", type = "warning")
        return()
      }
      add <- empty_param_row(ph)
      add$from_file <- row$file
      add$to_file <- row$file
      params_df(rbind(df, add))
      shiny::showNotification(paste("Added", ph), type = "message")
    })

    shiny::observeEvent(input$remove_param, {
      df <- params_df()
      sel <- input$params_rows_selected
      if (!nrow(df) || !length(sel)) {
        shiny::showNotification("Select a parameter row to remove.", type = "warning")
        return()
      }
      params_df(df[-as.integer(sel), , drop = FALSE])
    })

    shiny::observeEvent(input$remove_curve, {
      df <- curves_df()
      sel <- input$curves_rows_selected
      if (!nrow(df) || !length(sel)) {
        shiny::showNotification("Select a curve row to remove.", type = "warning")
        return()
      }
      i <- as.integer(sel)
      old_params <- split_csv(df[i, "params"])
      curves_df(df[-i, , drop = FALSE])
      if (length(old_params))
        params_df(replace_param_names(params_df(), old_params, character()))
    })

    yaml_from_tables <- function() {
      tables_to_param_config_yaml(params_df(), curves_df())
    }

    shiny::observeEvent(input$tables_to_yaml, {
      tryCatch({
        txt <- yaml_from_tables()
        update_yaml_editor(session, "yaml", txt)
        log_append(project, "Rebuilt YAML from parameter tables.")
      }, error = function(e) shiny::showNotification(err_text(e), type = "error"))
    })

    shiny::observeEvent(input$validate, {
      txt <- input$yaml
      if (!nzchar(trimws(txt %||% ""))) {
        tryCatch({
          txt <- yaml_from_tables()
          suppress_yaml(TRUE)
          update_yaml_editor(session, "yaml", txt)
        }, error = function(e) {
          shiny::showNotification(err_text(e), type = "error")
          return()
        })
      }
      tryCatch({
        apply_param_config(project, txt)
        shiny::showNotification("Parameters are valid.", type = "message")
      }, error = function(e) {
        project$config_ok <- FALSE
        project$config_msg <- err_text(e)
        log_append(project, "Validation failed: ", err_text(e))
        shiny::showNotification(err_text(e), type = "error")
      })
    })

    shiny::observeEvent(input$save, {
      tryCatch({
        txt <- input$yaml
        if (!nzchar(trimws(txt %||% ""))) txt <- yaml_from_tables()
        path <- param_config_file_path()
        apply_param_config(project, txt, path = path)
        writeLines(txt, path)
        log_append(project, "Wrote ", path)
        shiny::showNotification(paste("Saved", path), type = "message")
      }, error = function(e) {
        project$config_ok <- FALSE
        project$config_msg <- err_text(e)
        shiny::showNotification(err_text(e), type = "error")
      })
    })

    output$status <- shiny::renderText({
      project$config_msg
    })
  })
}
