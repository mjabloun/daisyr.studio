mod_project_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header("Project folder and Daisy"),
      shiny::p(class = "step-hint",
        "A project is a folder Daisy can run in: templates, ",
        shiny::tags$code("parameters.yaml"),
        ", observed data, and an ", shiny::tags$code("Output/"), " directory."
      ),
      shiny::div(
        class = "daisy-radio-inline",
        shiny::radioButtons(
          ns("daisy_launch"),
          "How to run Daisy",
          choices = c("Local executable" = "exe", "Command template" = "cmd"),
          selected = "exe",
          inline = TRUE
        )
      ),
      shiny::conditionalPanel(
        condition = "input.daisy_launch == 'exe'",
        ns = ns,
        shiny::textInput(ns("daisy_exe"), "Daisy executable", width = "100%",
                         placeholder = "e.g. C:/Program Files/Daisy 5.93/bin/daisy.exe")
      ),
      shiny::conditionalPanel(
        condition = "input.daisy_launch == 'cmd'",
        ns = ns,
        shiny::textAreaInput(
          ns("daisy_cmd"),
          "Command template",
          width = "100%",
          rows = 3,
          placeholder = 'e.g. "C:/Program Files/Daisy 5.93/bin/daisy.exe" "{run_file}"'
        ),
        shiny::p(class = "step-hint",
          "Must include ", shiny::tags$code("{run_file}"),
          ". You can also use ", shiny::tags$code("{daisy_exe}"), " and ",
          shiny::tags$code("{working_dir}"), "."
        )
      ),
      shiny::textInput(ns("project_dir"), "Project directory", width = "100%",
                       placeholder = "e.g. C:/path/to/my_daisy_project"),
      shiny::textInput(ns("template_dir"), "Template directory", width = "100%",
                       placeholder = "e.g. C:/path/to/dai_templates"),
      shiny::textInput(ns("run_file"), "Setup file to run (.dai, relative to project)",
                       width = "100%", placeholder = "e.g. scenario.dai"),
      shiny::div(
        class = "d-flex gap-2 flex-wrap",
        shiny::actionButton(ns("apply"), "Apply paths", class = "btn-primary"),
        shiny::actionButton(ns("load_example"), "Load calibration example", class = "btn-outline-secondary")
      )
    ),
    bslib::card(
      bslib::card_header("How to start"),
      shiny::tags$ol(
        shiny::tags$li("Choose ", shiny::strong("Local executable"), " and point at ",
          shiny::tags$code("daisy.exe"), ", or ", shiny::strong("Command template"),
          " and supply a command that includes ", shiny::tags$code("{run_file}"), "."),
        shiny::tags$li("Choose a writable project folder, or load the bundled calibration example."),
        shiny::tags$li("Build or load parameters, run one simulation, then calibrate or analyse sensitivity.")
      ),
      shiny::hr(),
      shiny::p(shiny::strong("Bundled example")),
      shiny::p(class = "step-hint",
        "Copies Andeby clay / bulk-density files into ",
        shiny::tags$code("daisyr-studio-workspace"),
        " under the current working directory and loads ready-made parameters plus observed soil water content."
      )
    )
  )
}

mod_project_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shiny::updateRadioButtons(session, "daisy_launch", selected = project$daisy_launch %||% "exe")
      shiny::updateTextInput(session, "daisy_exe", value = project$daisy_exe)
      shiny::updateTextAreaInput(session, "daisy_cmd", value = project$daisy_cmd %||% "")
      shiny::updateTextInput(session, "project_dir", value = project$project_dir)
      shiny::updateTextInput(session, "template_dir", value = project$template_dir)
      shiny::updateTextInput(session, "run_file", value = project$run_file)
    })

    shiny::observeEvent(input$apply, {
      mode <- input$daisy_launch %||% "exe"
      cmd <- trimws(input$daisy_cmd %||% "")
      if (identical(mode, "cmd") && !grepl("{run_file}", cmd, fixed = TRUE)) {
        shiny::showNotification(
          "Command template must include {run_file} so Daisy knows which .dai to run.",
          type = "error",
          duration = NULL
        )
        log_append(project, "Command template rejected: missing {run_file}.")
        return()
      }
      project$daisy_launch <- mode
      project$daisy_exe <- norm_dir(input$daisy_exe)
      project$daisy_cmd <- cmd
      project$project_dir <- norm_dir(input$project_dir)
      td <- trimws(input$template_dir)
      project$template_dir <- if (nzchar(td)) norm_dir(td) else ""
      project$run_file <- trimws(input$run_file)
      ok_exe <- daisy_launcher_ok(project)
      ok_dir <- dir.exists(project$project_dir)
      log_append(
        project,
        "Paths applied. Daisy: ", if (ok_exe) "ready" else "NOT SET",
        if (identical(mode, "cmd")) " (command template)" else "",
        "; project: ", if (ok_dir) "ok" else "missing",
        "; template: ", if (nzchar(project$template_dir)) project$template_dir else "(not set)"
      )
      if (!ok_exe) {
        msg <- if (identical(mode, "cmd"))
          "Enter a command template that includes {run_file}."
        else
          "Daisy executable not found."
        shiny::showNotification(msg, type = "error")
      } else if (!ok_dir) {
        shiny::showNotification("Project directory does not exist.", type = "error")
      } else {
        shiny::showNotification("Project paths saved.", type = "message")
      }
    })

    shiny::observeEvent(input$load_example, {
      src <- example_calibration_dir()
      if (!nzchar(src) || !dir.exists(src)) {
        shiny::showNotification("Could not find daisyr calibration_example files.", type = "error")
        return()
      }
      dest <- normalizePath(file.path(getwd(), "daisyr-studio-workspace"), winslash = "/", mustWork = FALSE)
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      copied <- file.copy(list.files(src, full.names = TRUE), dest, overwrite = TRUE)
      if (!all(copied)) {
        shiny::showNotification("Failed to copy one or more example files.", type = "error")
        return()
      }
      project$project_dir <- dest
      project$template_dir <- dest
      project$run_file <- "test-optim.dai"
      project$daisy_launch <- "exe"
      project$daisy_exe <- default_daisy_exe()
      project$daisy_cmd <- ""
      yaml_path <- file.path(dest, "parameters.yaml")
      yaml_text <- paste(readLines(yaml_path, warn = FALSE), collapse = "\n")
      tryCatch({
        apply_param_config(project, yaml_text, path = yaml_path)
      }, error = function(e) {
        project$config_yaml <- yaml_text
        project$config_path <- yaml_path
        project$config_ok <- FALSE
        project$config_msg <- err_text(e)
        log_append(project, "Example copied but parameter validation failed: ", err_text(e))
      })
      shiny::updateRadioButtons(session, "daisy_launch", selected = "exe")
      shiny::updateTextInput(session, "daisy_exe", value = project$daisy_exe)
      shiny::updateTextAreaInput(session, "daisy_cmd", value = "")
      shiny::updateTextInput(session, "project_dir", value = project$project_dir)
      shiny::updateTextInput(session, "template_dir", value = project$template_dir)
      shiny::updateTextInput(session, "run_file", value = project$run_file)
      log_append(project, "Loaded calibration example into ", dest)
      shiny::showNotification("Calibration example loaded into daisyr-studio-workspace.", type = "message")
    })
  })
}
