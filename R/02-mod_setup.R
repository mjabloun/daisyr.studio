mod_setup_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    id = ns("setup_mode"),
    bslib::nav_panel(
      title = "YAML",
      value = "yaml",
      setup_yaml_ui(ns)
    ),
    bslib::nav_panel(
      title = ".dai file",
      value = "dai",
      setup_dai_ui(ns)
    )
  )
}

setup_yaml_ui <- function(ns) {
  bslib::layout_columns(
    col_widths = c(5, 7),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Create or import a .dai setup"),
      shiny::p(class = "step-hint",
        "Optional. Build YAML on the right, then write a ",
        shiny::tags$code(".dai"), " file. Skip this page if you already have templates, or use the ",
        shiny::strong(".dai file"), " tab to edit Daisy text directly."
      ),
      shiny::h6("1. Start YAML from sections"),
      shiny::p(class = "step-hint",
        "Pick the Daisy keys you need and insert a starter YAML into the editor on the right. Only the chips in the box are inserted; click \u00d7 to drop one."
      ),
      shiny::selectizeInput(
        ns("sections"), "YAML sections to scaffold",
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "e.g. programs, columns",
          plugins = list("remove_button")
        )
      ),
      shiny::actionButton(ns("scaffold"), "Insert scaffold", class = "btn-outline-primary"),
      shiny::hr(),
      shiny::h6("2. Or import an existing .dai"),
      shiny::p(class = "step-hint",
        "Reads a Daisy setup file from the project folder and puts the YAML translation in the editor."
      ),
      shiny::textInput(ns("dai_in"), "Existing .dai to import", width = "100%",
                       placeholder = "e.g. west.dai"),
      shiny::actionButton(ns("from_dai"), "Import .dai \u2192 YAML", class = "btn-outline-secondary")
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Setup YAML"),
      shiny::h6("Write this YAML to a .dai file"),
      shiny::p(class = "step-hint",
        "Uses the YAML in the box below. A relative path such as ",
        shiny::tags$code("scenario.dai"), " or ",
        shiny::tags$code("lib/my_crop.dai"),
        " is written under the Project folder. This does not change ",
        shiny::strong("Setup file to run"),
        " on the Project page \u2014 use this for templates, library files, or any other ",
        shiny::tags$code(".dai"), "."
      ),
      shiny::div(
        class = "d-flex gap-2 align-items-end flex-wrap",
        shiny::div(
          class = "flex-grow-1",
          shiny::textInput(ns("dai_out"), "Write generated .dai as", width = "100%",
                           placeholder = "e.g. scenario.dai or setup/west.dai")
        ),
        shiny::actionButton(ns("generate"), "Generate .dai from YAML", class = "btn-primary mb-3")
      ),
      shiny::hr(),
      shiny::h6("YAML editor"),
      shiny::p(class = "step-hint",
        "YAML highlighting with two-space indent. Tab / Shift+Tab change indent; Ace keeps structure as you type. To mark a parameter, write the placeholder with double braces, e.g. ",
        shiny::tags$code("{{clay_Ap}}"),
        " \u2014 not the bare name ",
        shiny::tags$code("clay_Ap"),
        "."
      ),
      yaml_editor_ui(ns("yaml"), placeholder = "e.g. paste or scaffold Daisy YAML here"),
      shiny::p(class = "step-hint", "Daisy syntax this YAML would produce:"),
      shiny::verbatimTextOutput(ns("preview"), placeholder = TRUE)
    )
  )
}

setup_dai_ui <- function(ns) {
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      fill = FALSE,
      bslib::card_header("Load a .dai file"),
      shiny::p(class = "step-hint",
        "Load a ", shiny::tags$code(".dai"),
        " from the project folder into the editor. No YAML conversion. Edit and save on the right \u2014 useful for templates."
      ),
      shiny::textInput(
        ns("dai_load"), "Existing .dai to load", width = "100%",
        placeholder = "e.g. west.dai or template/test-optim_Generic.dai"
      ),
      shiny::actionButton(ns("dai_open"), "Load .dai", class = "btn-outline-primary")
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header(".dai editor"),
      shiny::h6("Save this text to a .dai file"),
      shiny::p(class = "step-hint",
        "Uses the Daisy text in the box below. A relative path is written under the Project folder. This does not change ",
        shiny::strong("Setup file to run"),
        " on the Project page."
      ),
      shiny::div(
        class = "d-flex gap-2 align-items-end flex-wrap",
        shiny::div(
          class = "flex-grow-1",
          shiny::textInput(
            ns("dai_save"), "Save .dai as", width = "100%",
            placeholder = "e.g. west.dai or template/west_Generic.dai"
          )
        ),
        shiny::actionButton(ns("dai_write"), "Save .dai", class = "btn-primary mb-3")
      ),
      shiny::hr(),
      shiny::p(class = "step-hint",
        "To mark a parameter, write the placeholder with double braces, e.g. ",
        shiny::tags$code("{{clay_Ap}}"),
        " \u2014 not the bare name ",
        shiny::tags$code("clay_Ap"),
        "."
      ),
      dai_editor_ui(
        ns("dai_text"),
        placeholder = "e.g. load a .dai file, or paste Daisy setup text here"
      )
    )
  )
}

mod_setup_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(TRUE, {
      secs <- tryCatch(list_dai_sections(), error = function(e) NULL)
      if (is.null(secs)) return()
      shiny::updateSelectizeInput(
        session, "sections",
        choices = stats::setNames(secs$section, paste(secs$section, "\u2014", secs$description)),
        selected = character(0)
      )
    }, once = TRUE)

    shiny::observeEvent(project$session_reset, {
      update_yaml_editor(session, "yaml", "")
      update_dai_editor(session, "dai_text", "")
      shiny::updateTextInput(session, "dai_in", value = "")
      shiny::updateTextInput(session, "dai_out", value = "")
      shiny::updateTextInput(session, "dai_load", value = "")
      shiny::updateTextInput(session, "dai_save", value = "")
      shiny::updateSelectizeInput(session, "sections", selected = character())
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$scaffold, {
      secs <- input$sections
      if (is.null(secs) || !length(secs)) {
        shiny::showNotification("Select at least one section.", type = "warning")
        return()
      }
      txt <- scaffold_dai_yaml(sections = secs)
      update_yaml_editor(session, "yaml", txt)
      log_append(project, "Scaffolded DAI YAML (", length(secs), " sections).")
    })

    shiny::observeEvent(input$from_dai, {
      tryCatch({
        need_project(project)
        path <- resolve_project_file(project, input$dai_in, must_exist = TRUE)
        cfg <- read_dai(path)
        yml <- yaml::as.yaml(cfg, indent = 2)
        update_yaml_editor(session, "yaml", yml)
        log_append(project, "Imported ", path, " to YAML.")
        shiny::showNotification("Imported .dai into the YAML editor.", type = "message")
      }, error = function(e) {
        shiny::showNotification(err_text(e), type = "error")
      })
    })

    shiny::observeEvent(input$generate, {
      tryCatch({
        need_project(project)
        out_path <- resolve_project_file(project, input$dai_out)
        cfg <- yaml::yaml.load(input$yaml)
        dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
        generate_dai(cfg, output_path = out_path)
        log_append(project, "Wrote ", out_path)
        shiny::showNotification(paste("Wrote", out_path), type = "message")
      }, error = function(e) {
        shiny::showNotification(err_text(e), type = "error")
      })
    })

    shiny::observeEvent(input$dai_open, {
      tryCatch({
        need_project(project)
        path <- resolve_project_file(project, input$dai_load, must_exist = TRUE)
        update_dai_editor(session, "dai_text", read_text_file(path))
        if (!nzchar(trimws(input$dai_save %||% "")))
          shiny::updateTextInput(session, "dai_save", value = trimws(input$dai_load))
        log_append(project, "Loaded ", path, " into the .dai editor.")
        shiny::showNotification(paste("Loaded", path), type = "message")
      }, error = function(e) {
        shiny::showNotification(err_text(e), type = "error")
      })
    })

    shiny::observeEvent(input$dai_write, {
      tryCatch({
        need_project(project)
        out_path <- resolve_project_file(project, input$dai_save)
        write_text_file(out_path, input$dai_text %||% "")
        log_append(project, "Saved ", out_path)
        shiny::showNotification(paste("Saved", out_path), type = "message")
      }, error = function(e) {
        shiny::showNotification(err_text(e), type = "error")
      })
    })

    output$preview <- shiny::renderText({
      yml <- input$yaml
      if (!nzchar(trimws(yml %||% ""))) return("Edit YAML above to preview the Daisy syntax it would generate.")
      tryCatch({
        cfg <- yaml::yaml.load(yml)
        generate_dai(cfg)
      }, error = function(e) paste("Cannot preview yet:", err_text(e)))
    })
  })
}
