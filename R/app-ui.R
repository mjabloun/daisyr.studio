daisyr_studio_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1b5e40",
    "navbar-bg" = "#14352a",
    "enable-rounded" = TRUE
  ) |>
    bslib::bs_add_rules("
      .plots-left-col { display: flex; flex-direction: column; gap: 0.75rem; }
    ")
}

daisyr_studio_css_href <- function() {
  css_file <- system.file("www", "app.css", package = "daisyr.studio")
  stamp <- tryCatch(as.integer(file.info(css_file)$mtime), error = function(e) 1L)
  paste0("daisyrshiny/app.css?v=", stamp)
}

daisyr_studio_ui <- function() {
  shiny::addResourcePath(
    "daisyrshiny",
    system.file("www", package = "daisyr.studio", mustWork = TRUE)
  )

  bslib::page_fillable(
    theme = daisyr_studio_theme(),
    padding = 0,
    gap = 0,
    fillable_mobile = TRUE,
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = daisyr_studio_css_href()),
      shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('daisy-nav', function(value) {
        var rail = document.querySelector('.app-nav-rail');
        if (rail && rail.classList.contains('nav-locked')) return;
        Shiny.setInputValue('nav_pick', value, {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('daisy-lock-nav', function(msg) {
        var rail = document.querySelector('.app-nav-rail');
        if (!rail) return;
        rail.classList.toggle('nav-locked', !!(msg && msg.locked));
        rail.setAttribute('aria-busy', (msg && msg.locked) ? 'true' : 'false');
      });
      Shiny.addCustomMessageHandler('daisy-append-text', function(msg) {
        var el = document.getElementById(msg.id);
        if (!el) return;
        if (msg.reset) el.textContent = '';
        if (msg.line) {
          el.textContent += msg.line.replace(/\\s+$/, '') + '\\n';
          el.scrollTop = el.scrollHeight;
        }
      });
      Shiny.addCustomMessageHandler('daisy-set-progress', function(msg) {
        var bar = document.getElementById(msg.id);
        if (!bar) return;
        var wrap = bar.closest('.daisy-run-progress');
        if (msg.hide) {
          if (wrap) wrap.style.display = 'none';
          bar.style.width = '0%';
          bar.setAttribute('aria-valuenow', '0');
          return;
        }
        if (wrap) wrap.style.display = '';
        var pct = Math.max(0, Math.min(100, Number(msg.pct) || 0));
        bar.style.width = pct + '%';
        bar.setAttribute('aria-valuenow', String(Math.round(pct)));
        if (msg.label && msg.label_id) {
          var lab = document.getElementById(msg.label_id);
          if (lab) lab.textContent = msg.label;
        }
      });
    "))
    ),
    shiny::div(
      class = "app-topbar",
      shiny::span(
        class = "app-topbar-title",
        shiny::strong("daisyr"),
        shiny::span(" studio")
      ),
      shiny::span(class = "app-topbar-meta", "Daisy from R")
    ),
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        id = "nav_rail",
        position = "left",
        width = 240,
        open = "always",
        bg = "#14352a",
        fg = "#e8f5ee",
        class = "app-nav-rail",
        gap = "0.35rem",
        padding = c("1rem", "0.9rem"),
        shiny::uiOutput("nav_menu")
      ),
      bslib::layout_sidebar(
        fillable = TRUE,
        sidebar = bslib::sidebar(
          id = "session_rail",
          title = "Session",
          width = 280,
          position = "right",
          open = TRUE,
          gap = "0.4rem",
          padding = c("0.7rem", "0.85rem"),
          class = "session-rail",
          shiny::actionButton(
            "session_init", "Initialise",
            class = "btn-outline-secondary w-100"
          ),
          shiny::p(
            class = "step-hint",
            "Reset this session to a blank project without closing the app. Files on disk are left as they are."
          ),
          shiny::hr(),
          shiny::textInput(
            "session_name", "Save session as",
            width = "100%",
            placeholder = "e.g. clay_calib"
          ),
          shiny::actionButton(
            "session_save", "Save session",
            class = "btn-outline-primary w-100"
          ),
          shiny::selectInput(
            "session_pick", "Saved sessions",
            choices = character(),
            width = "100%"
          ),
          shiny::actionButton(
            "session_reload", "Load session",
            class = "btn-outline-secondary w-100"
          ),
          shiny::p(
            class = "step-hint",
            "Stored as ", shiny::tags$code(".rds"),
            " in ", shiny::tags$code("daisyr-studio-sessions"),
            " under your home folder, so they show up without a project directory. Older copies in a project folder are still listed. Paths and loaded tables come back; Daisy output is re-read from ",
            shiny::tags$code(".dlf"), " files on disk."
          ),
          shiny::uiOutput("session_status")
        ),
        bslib::navset_hidden(
          id = "main_page",
          bslib::nav_panel(title = "project", value = "project", mod_project_ui("project")),
          bslib::nav_panel(title = "setup", value = "setup", mod_setup_ui("setup")),
          bslib::nav_panel(title = "parameters", value = "parameters", mod_parameters_ui("parameters")),
          bslib::nav_panel(title = "simulate", value = "simulate", mod_simulate_ui("simulate")),
          bslib::nav_panel(title = "plots", value = "plots", mod_plots_ui("plots")),
          bslib::nav_panel(title = "observe", value = "observe", mod_observe_ui("observe")),
          bslib::nav_panel(title = "sensitivity", value = "sensitivity", mod_sensitivity_ui("sensitivity")),
          bslib::nav_panel(title = "calibrate", value = "calibrate", mod_calibrate_ui("calibrate")),
          bslib::nav_panel(title = "design", value = "design", mod_design_ui("design"))
        )
      )
    )
  )
}
