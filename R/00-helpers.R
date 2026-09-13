`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

default_daisy_exe <- function() {
  tryCatch(get_daisy_path(), error = function(e) {
    "C:/Program Files/Daisy 5.93/bin/daisy.exe"
  })
}

daisy_cmd_of <- function(project) {
  if (!identical(project$daisy_launch %||% "exe", "cmd")) return(NULL)
  cmd <- trimws(project$daisy_cmd %||% "")
  if (!nzchar(cmd)) return(NULL)
  if (!grepl("{run_file}", cmd, fixed = TRUE)) {
    stop("Command template must include {run_file} so Daisy knows which .dai to run.")
  }
  cmd
}

daisy_launcher_ok <- function(project) {
  if (identical(project$daisy_launch %||% "exe", "cmd")) {
    cmd <- trimws(project$daisy_cmd %||% "")
    return(nzchar(cmd) && grepl("{run_file}", cmd, fixed = TRUE))
  }
  nzchar(project$daisy_exe %||% "") && file.exists(project$daisy_exe)
}

example_calibration_dir <- function() {
  d <- system.file("extdata", "calibration_example", package = "daisyr")
  if (nzchar(d) && dir.exists(d)) return(d)
  ""
}

log_append <- function(project, ...) {
  msg <- paste0(format(Sys.time(), "%H:%M:%S"), "  ", paste0(..., collapse = ""))
  project$log <- c(msg, project$log)
  if (length(project$log) > 300L) project$log <- project$log[seq_len(300L)]
}

shiny_daisy_progress <- function(i, n, label = "Daisy run") {
  n <- max(as.integer(n), 1L)
  i <- max(as.integer(i), 0L)
  shiny::setProgress(
    value = min(0.99, i / n),
    detail = if (i <= 0L)
      sprintf("Starting %s 1 of %d", label, n)
    else
      sprintf("%s %d of %d", label, i, n)
  )
}

send_daisy_progress <- function(session, bar_id, lab_id, i, n, label = "Daisy run",
                                detail = NULL) {
  n <- max(as.integer(n), 1L)
  i <- max(as.integer(i), 0L)
  pct <- 100 * (i / n)
  if (i < n) pct <- min(pct, 99)
  lab <- detail %||% (
    if (i <= 0L)
      sprintf("Starting %s 1 of %d", label, n)
    else if (i >= n)
      sprintf("Finished %d of %d", n, n)
    else
      sprintf("%s %d of %d", label, i, n)
  )
  session$sendCustomMessage("daisy-set-progress", list(
    id = bar_id, label_id = lab_id, pct = pct, label = lab, hide = FALSE
  ))
}

hide_daisy_progress <- function(session, bar_id) {
  session$sendCustomMessage("daisy-set-progress", list(id = bar_id, hide = TRUE))
}

with_nav_lock <- function(session, project, fun) {
  if (!is.function(fun)) stop("with_nav_lock() needs a function to run after the nav lock is shown.")
  project$run_busy <- TRUE
  session$sendCustomMessage("daisy-lock-nav", list(locked = TRUE))
  # onFlushed is not a reactive consumer; isolate so fun() can read project$*.
  session$onFlushed(function() {
    shiny::isolate({
      tryCatch(
        fun(),
        finally = {
          project$run_busy <- FALSE
          session$sendCustomMessage("daisy-lock-nav", list(locked = FALSE))
        }
      )
    })
  }, once = TRUE)
  invisible(NULL)
}

empty_project_state <- function() {
  list(
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
    metamodel = NULL
  )
}

reset_project_session <- function(project) {
  tick <- (project$session_reset %||% 0L) + 1L
  st <- empty_project_state()
  for (nm in names(st)) project[[nm]] <- st[[nm]]
  project$session_reset <- tick
  invisible(tick)
}

studio_session_home_dir <- function() {
  home <- Sys.getenv("USERPROFILE", unset = "")
  if (!nzchar(home)) home <- Sys.getenv("HOME", unset = "")
  if (!nzchar(home)) home <- path.expand("~")
  file.path(home, "daisyr-studio-sessions")
}

studio_session_dir <- function(project = NULL) {
  studio_session_home_dir()
}

studio_session_search_dirs <- function(project = NULL) {
  dirs <- studio_session_home_dir()
  pd <- if (is.list(project) || inherits(project, "reactivevalues"))
    project$project_dir %||% "" else ""
  if (nzchar(pd) && dir.exists(pd))
    dirs <- c(dirs, file.path(pd, "daisyr-studio-sessions"))
  wd <- getwd()
  if (nzchar(wd))
    dirs <- c(dirs, file.path(wd, "daisyr-studio-sessions"))
  unique(normalizePath(dirs, winslash = "/", mustWork = FALSE))
}

safe_session_stem <- function(name) {
  stem <- gsub("[^A-Za-z0-9._-]+", "_", trimws(name %||% ""))
  stem <- gsub("^_+|_+$", "", stem)
  if (!nzchar(stem))
    stem <- format(Sys.time(), "%Y%m%d_%H%M%S")
  stem
}

snapshot_project_state <- function(project) {
  keys <- setdiff(names(empty_project_state()), "run_busy")
  st <- lapply(keys, function(nm) project[[nm]])
  names(st) <- keys
  st$last_sim <- NULL
  st
}

write_studio_session <- function(path, state, label = "") {
  obj <- list(
    format = "daisyr.studio.session",
    version = 1L,
    saved_at = Sys.time(),
    label = as.character(label %||% ""),
    state = state
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(obj, path)
  invisible(path)
}

read_studio_session <- function(path) {
  if (!file.exists(path)) stop("Session file not found: ", path)
  obj <- readRDS(path)
  if (!is.list(obj) || !identical(obj$format, "daisyr.studio.session"))
    stop("Not a daisyr studio session file: ", path)
  st <- obj$state
  if (!is.list(st)) stop("Session file is missing state: ", path)
  st
}

migrate_legacy_session_state <- function(st) {
  pairs <- list(
    config = "registry",
    config_yaml = "registry_yaml",
    config_path = "registry_path",
    config_ok = "registry_ok",
    config_msg = "registry_msg"
  )
  for (new in names(pairs)) {
    old <- pairs[[new]]
    if (!(new %in% names(st)) && old %in% names(st))
      st[[new]] <- st[[old]]
  }
  st
}

apply_project_state <- function(project, st) {
  if (!is.list(st)) stop("Session state must be a list.")
  st <- migrate_legacy_session_state(st)
  base <- empty_project_state()
  for (nm in names(base)) {
    project[[nm]] <- if (nm %in% names(st)) st[[nm]] else base[[nm]]
  }
  project$run_busy <- FALSE
  project$config_tick <- (project$config_tick %||% 0L) + 1L
  project$session_load <- (project$session_load %||% 0L) + 1L
  invisible(TRUE)
}

list_studio_sessions <- function(project = NULL) {
  files <- character()
  for (d in studio_session_search_dirs(project)) {
    if (!dir.exists(d)) next
    files <- c(files, list.files(d, pattern = "[.]rds$", full.names = TRUE, ignore.case = TRUE))
  }
  if (!length(files)) return(character())
  files <- normalizePath(files, winslash = "/", mustWork = FALSE)
  stems <- tolower(sub("[.]rds$", "", basename(files), ignore.case = TRUE))
  files <- files[!duplicated(stems)]
  info <- file.info(files)
  files <- files[order(info$mtime, decreasing = TRUE, na.last = TRUE)]
  labs <- sub("[.]rds$", "", basename(files), ignore.case = TRUE)
  stats::setNames(files, labs)
}

resolve_studio_session_file <- function(pick, project = NULL) {
  pick <- as.character(pick %||% "")[1]
  if (!nzchar(pick)) stop("Choose a saved session to load.")
  if (file.exists(pick))
    return(normalizePath(pick, winslash = "/", mustWork = FALSE))
  hits <- list_studio_sessions(project)
  if (length(hits)) {
    if (pick %in% hits)
      return(unname(hits[match(pick, as.character(hits))]))
    named <- unname(hits[names(hits) == pick])
    if (length(named)) return(named[[1]])
    base_hit <- unname(hits[basename(hits) %in% c(pick, paste0(pick, ".rds"))])
    if (length(base_hit)) return(base_hit[[1]])
  }
  cand <- file.path(
    studio_session_home_dir(),
    if (grepl("[.]rds$", pick, ignore.case = TRUE)) pick else paste0(pick, ".rds")
  )
  if (file.exists(cand))
    return(normalizePath(cand, winslash = "/", mustWork = FALSE))
  stop("Session file not found: ", pick)
}

err_text <- function(e) {
  paste(conditionMessage(e), collapse = "\n")
}

safe_call <- function(expr) {
  tryCatch(
    list(ok = TRUE, value = expr, error = NULL),
    error = function(e) list(ok = FALSE, value = NULL, error = err_text(e))
  )
}

need_project <- function(project) {
  if (!daisy_launcher_ok(project))
    stop("Set a valid Daisy executable or a command template on the Project page.")
  if (!nzchar(project$project_dir) || !dir.exists(project$project_dir))
    stop("Set an existing project directory on the Project page.")
  invisible(TRUE)
}

need_param_config <- function(project) {
  if (is.null(project$config))
    stop("Load or build parameters first (Parameters page).")
  if (!isTRUE(project$config_ok))
    stop("Parameters are not valid: ", project$config_msg)
  invisible(TRUE)
}

need_objective <- function(project) {
  if (is.null(project$objective) || !inherits(project$objective, "daisyr_objective"))
    stop("Save a named objective on the Observe page first.")
  invisible(TRUE)
}

empty_obs_map <- function() {
  data.frame(
    obs_col = character(), sim_col = character(), label = character(), weight = numeric(),
    stringsAsFactors = FALSE
  )
}

new_obs_set <- function(tag = "obs", id = NULL) {
  list(
    id = id %||% paste0("obs_", as.integer(stats::runif(1, 1e6, 9e6))),
    tag = tag,
    path = "",
    data = NULL,
    join_cols = "Date",
    sim_file = "",
    summary_groups = character(),
    summary_fun = "",
    metric = "RMSE",
    map = empty_obs_map()
  )
}

obs_set_to_spec <- function(set) {
  tag <- set$tag %||% set$id
  if (is.null(set$data) || !NROW(set$data))
    stop("Load observed data for tab '", tag, "'.")
  vm <- set$map
  if (is.null(vm) || !nrow(vm) || any(!nzchar(trimws(vm$obs_col)) | !nzchar(trimws(vm$sim_col))))
    stop("Fill obs_col and sim_col for every mapping row on '", tag, "'.")
  join <- split_csv(set$join_cols %||% "Date")
  if (!length(join)) join <- "Date"
  sim <- set$sim_file %||% ""
  if (!nzchar(sim))
    stop("Choose a simulated .dlf for '", tag, "'.")
  objective_spec(
    obs_data = set$data,
    value_map = vm,
    join_cols = join,
    sim_reader = make_sim_reader(
      groups = set$summary_groups %||% character(),
      fun = set$summary_fun %||% ""
    ),
    metric = set$metric %||% "RMSE",
    output_file = basename(sim)
  )
}

compile_named_objective <- function(name, sets, member_tags, weights = NULL) {
  name <- trimws(as.character(name %||% ""))
  if (!nzchar(name)) name <- "draft"
  member_tags <- unique(as.character(member_tags %||% character()))
  member_tags <- member_tags[nzchar(member_tags)]
  if (!length(member_tags))
    stop("Pick at least one observation tab for objective '", name, "'.")
  tags <- vapply(sets, function(s) s$tag, character(1))
  specs <- lapply(member_tags, function(tag) {
    hit <- which(tags == tag)
    if (!length(hit)) stop("No observation tab named '", tag, "'.")
    if (length(hit) > 1L) stop("Observation tags must be unique ('", tag, "' is duplicated).")
    obs_set_to_spec(sets[[hit[[1]]]])
  })
  names(specs) <- member_tags
  if (is.null(weights) || !length(weights) || !any(is.finite(weights)))
    weights <- rep(1, length(specs))
  if (length(weights) != length(specs))
    stop("Weights must have one value per observation tab.")
  composite_objective(specs, weights = weights, name = name)
}

resolve_objective_sim_file <- function(spec, project) {
  comps <- if (inherits(spec, "daisyr_composite_objective")) spec$components else list(spec)
  files <- list_dlf_files(project$project_dir)
  for (comp in comps) {
    f <- comp$output_file %||% ""
    if (!nzchar(f)) next
    if (file.exists(f)) return(normalizePath(f, winslash = "/", mustWork = FALSE))
    rel <- file.path(project$project_dir %||% "", f)
    if (nzchar(project$project_dir %||% "") && file.exists(rel))
      return(normalizePath(rel, winslash = "/", mustWork = FALSE))
    hit <- files[tolower(basename(files)) == tolower(basename(f))]
    if (length(hit)) return(hit[[1]])
  }
  if (length(files)) return(files[[1]])
  last <- project$last_sim_path %||% ""
  if (nzchar(last) && file.exists(last)) return(last)
  ""
}

pick_saved_objective <- function(project, name = NULL) {
  objs <- project$objectives %||% list()
  name <- as.character(name %||% "")[1]
  if (nzchar(name) && name %in% names(objs)) return(objs[[name]])
  active <- project$active_of %||% ""
  if (nzchar(active) && active %in% names(objs)) return(objs[[active]])
  if (!is.null(project$objective) && inherits(project$objective, "daisyr_objective")) {
    return(list(
      name = active,
      spec = project$objective,
      maximize = isTRUE(project$maximize)
    ))
  }
  stop("Save a named objective on the Observe page first.")
}

objective_component_list <- function(spec) {
  if (inherits(spec, "daisyr_composite_objective")) return(spec$components)
  nm <- spec$name %||% "obs"
  stats::setNames(list(spec), nm)
}

resolve_component_dlf <- function(comp, project) {
  f <- as.character(comp$output_file %||% "")[1]
  files <- list_dlf_files(project$project_dir)
  if (nzchar(f)) {
    if (file.exists(f)) return(normalizePath(f, winslash = "/", mustWork = FALSE))
    rel <- file.path(project$project_dir %||% "", f)
    if (nzchar(project$project_dir %||% "") && file.exists(rel))
      return(normalizePath(rel, winslash = "/", mustWork = FALSE))
    hit <- files[tolower(basename(files)) == tolower(basename(f))]
    if (length(hit)) return(hit[[1]])
  }
  last <- project$last_sim_path %||% ""
  if (nzchar(last) && file.exists(last)) return(last)
  if (length(files)) return(files[[1]])
  ""
}

join_obs_sim_for_plot <- function(obs_data, sim_dt, join_cols, value_map) {
  obs_dt <- data.table::copy(data.table::as.data.table(obs_data))
  sim_dt <- data.table::copy(data.table::as.data.table(sim_dt))
  join_cols <- as.character(join_cols)
  join_cols <- join_cols[nzchar(join_cols)]
  if (!length(join_cols)) join_cols <- "Date"
  if (is.data.frame(value_map)) {
    vm <- data.table::as.data.table(value_map)
  } else {
    vm <- data.table::rbindlist(lapply(value_map, data.table::as.data.table), fill = TRUE)
  }
  if (!nrow(vm) || !all(c("obs_col", "sim_col") %in% names(vm)))
    stop("Objective has no mapped observed/simulated columns.")
  if (!"label" %in% names(vm)) vm$label <- vm$obs_col
  vm$label <- as.character(vm$label)
  vm$label[!nzchar(vm$label) | is.na(vm$label)] <- as.character(vm$obs_col[!nzchar(vm$label) | is.na(vm$label)])
  overlap <- setdiff(intersect(names(obs_dt), names(sim_dt)), join_cols)
  if (length(overlap)) {
    new_nms <- paste0("sim.", overlap)
    data.table::setnames(sim_dt, overlap, new_nms)
    hit <- vm$sim_col %in% overlap
    vm$sim_col[hit] <- paste0("sim.", vm$sim_col[hit])
  }
  missing_j <- setdiff(join_cols, names(obs_dt))
  if (length(missing_j))
    stop("Observed data is missing join column(s): ", paste(missing_j, collapse = ", "))
  missing_j <- setdiff(join_cols, names(sim_dt))
  if (length(missing_j))
    stop("Simulated data is missing join column(s): ", paste(missing_j, collapse = ", "))
  list(obs = obs_dt, sim = sim_dt, value_map = vm, join_cols = join_cols)
}

objective_fit_series <- function(spec, project) {
  comps <- objective_component_list(spec)
  out <- list()
  for (nm in names(comps)) {
    comp <- comps[[nm]]
    path <- resolve_component_dlf(comp, project)
    if (!nzchar(path) || !file.exists(path))
      stop("No .dlf found for '", nm, "'. Calibrate or run Daisy first.")
    sim <- comp$sim_reader(path)
    prepared <- join_obs_sim_for_plot(comp$obs_data, sim, comp$join_cols, comp$value_map)
    obs_dt <- prepared$obs
    sim_dt <- prepared$sim
    if (!NROW(sim_dt))
      stop("Simulated series for '", nm, "' is empty.")
    xcol <- if ("Date" %in% names(sim_dt)) "Date" else prepared$join_cols[[1]]
    vm <- prepared$value_map
    for (i in seq_len(nrow(vm))) {
      lab <- as.character(vm$label[[i]])
      if (!nzchar(lab)) lab <- as.character(vm$obs_col[[i]])
      key <- if (length(comps) > 1L) paste(nm, lab, sep = " / ") else lab
      sim_col <- vm$sim_col[[i]]
      obs_col <- vm$obs_col[[i]]
      if (!sim_col %in% names(sim_dt))
        stop("Simulated column '", sim_col, "' not found for '", key, "'.")
      if (!obs_col %in% names(obs_dt))
        stop("Observed column '", obs_col, "' not found for '", key, "'.")
      sim_rows <- data.frame(
        x = sim_dt[[xcol]],
        value = sim_dt[[sim_col]],
        series = "Simulated",
        stringsAsFactors = FALSE
      )
      obs_x <- if (xcol %in% names(obs_dt)) obs_dt[[xcol]] else obs_dt[[prepared$join_cols[[1]]]]
      obs_rows <- data.frame(
        x = obs_x,
        value = obs_dt[[obs_col]],
        series = "Observed",
        stringsAsFactors = FALSE
      )
      long <- rbind(sim_rows, obs_rows)
      out[[key]] <- ggplot2::ggplot(long, ggplot2::aes(x = .data$x, y = .data$value, colour = .data$series)) +
        ggplot2::geom_line(data = sim_rows, na.rm = TRUE) +
        ggplot2::geom_point(data = obs_rows, size = 2.2, na.rm = TRUE) +
        ggplot2::scale_colour_manual(values = c(Observed = "#c0392b", Simulated = "#1b5e40")) +
        ggplot2::labs(title = key, x = xcol, y = lab, colour = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")
    }
  }
  out
}

norm_dir <- function(path) {
  if (!nzchar(path)) return("")
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

template_dir_of <- function(project) {
  td <- project$template_dir %||% ""
  if (nzchar(td)) td else ""
}

need_template_dir <- function(project) {
  td <- template_dir_of(project)
  if (!nzchar(td))
    stop("Set a template directory on the Project page before substituting parameter values.")
  if (!dir.exists(td))
    stop("Template directory does not exist: ", td)
  td
}

run_project_daisy <- function(project, values = NULL, show_log = TRUE) {
  need_project(project)
  if (!nzchar(project$run_file)) stop("Set the setup file to run on the Project page.")
  if (!is.null(values)) {
    need_param_config(project)
    render_templates(
      project$config, values,
      template_dir = need_template_dir(project),
      output_dir = project$project_dir
    )
  }
  status <- run_daisy(
    project$run_file,
    daisy_exe = project$daisy_exe,
    working_dir = project$project_dir,
    show_log = show_log,
    cmd = daisy_cmd_of(project)
  )
  project$last_status <- as.integer(status)
  project$last_run_values <- values
  project$daisy_log_path <- find_daisy_log(project$project_dir)
  project$daisy_log_stamp <- as.numeric(Sys.time())
  project$last_status
}

find_daisy_log <- function(project_dir) {
  if (!nzchar(project_dir) || !dir.exists(project_dir)) return("")
  candidates <- c(
    file.path(project_dir, "Output", "daisy.log"),
    file.path(project_dir, "daisy.log")
  )
  exists <- vapply(candidates, file.exists, logical(1))
  if (!any(exists)) return("")
  hit <- candidates[exists]
  info <- file.info(hit)
  hit[[which.max(info$mtime)]]
}

read_daisy_log_text <- function(path, max_lines = 800L) {
  if (is.null(path) || !nzchar(path) || !file.exists(path))
    return("No daisy.log found yet. Run Daisy; it usually writes Output/daisy.log under the project folder.")
  # Daisy on Windows writes ANSI (ø in "Søren" is 0xF8), not UTF-8.
  lines <- tryCatch({
    con <- file(path, open = "r", encoding = "latin1")
    on.exit(close(con), add = TRUE)
    readLines(con, warn = FALSE, skipNul = TRUE)
  }, error = function(e) character())
  lines <- enc2utf8(as.character(lines))
  lines[is.na(lines)] <- ""
  if (!length(lines) || !any(nzchar(lines)))
    return(paste0(path, "\n\n(daisy.log is empty or could not be read)"))
  note <- ""
  if (length(lines) > max_lines) {
    note <- sprintf("\n[showing last %d of %d lines]", max_lines, length(lines))
    lines <- utils::tail(lines, max_lines)
  }
  paste0(path, note, "\n\n", paste(lines, collapse = "\n"))
}

parse_and_validate_yaml <- function(yaml_text, template_dir) {
  if (!nzchar(trimws(yaml_text)))
    stop("Parameters YAML is empty.")
  tmp <- tempfile(fileext = ".yaml")
  writeLines(yaml_text, tmp, useBytes = FALSE)
  on.exit(unlink(tmp), add = TRUE)
  config <- read_param_config(tmp)
  if (nzchar(template_dir) && dir.exists(template_dir))
    validate_param_config(config, template_dir = template_dir)
  config
}

apply_param_config <- function(project, yaml_text, path = NULL) {
  td <- template_dir_of(project)
  config <- parse_and_validate_yaml(yaml_text, td)
  project$config_yaml <- yaml_text
  project$config <- config
  project$config_tick <- (project$config_tick %||% 0L) + 1L
  project$config_ok <- TRUE
  project$config_msg <- sprintf(
    "Valid \u2014 %d name(s): %s",
    nrow(config$parameters),
    paste(config$parameters$name, collapse = ", ")
  )
  if (!is.null(path) && nzchar(path)) project$config_path <- path
  log_append(project, "Parameters validated (", nrow(config$parameters), " names).")
  config
}

params_to_df <- function(config) {
  if (is.null(config)) {
    return(empty_param_row()[0, , drop = FALSE])
  }
  as.data.frame(config$parameters[, c("name", "default", "min", "max", "from_file", "to_file", "role")],
                stringsAsFactors = FALSE)
}

empty_param_row <- function(name = "") {
  data.frame(
    name = as.character(name), default = 0, min = 0, max = 1,
    from_file = "", to_file = "", role = "direct",
    stringsAsFactors = FALSE
  )
}

empty_curve_row <- function(name = "", curve = "logistic", params = "") {
  data.frame(
    name = as.character(name), from_file = "", to_file = "",
    placeholder = if (nzchar(name)) paste0(name, "_PLF") else "",
    x_values = "0, 1", curve = as.character(curve),
    params = as.character(params), stringsAsFactors = FALSE
  )
}

plf_shape_args <- function(curve) {
  switch(as.character(curve),
    logistic = c("L", "k", "x0"),
    gompertz = c("L", "b", "k"),
    richards = c("L", "k", "x0", "v"),
    exponential = c("a", "b"),
    c("p1", "p2")
  )
}

plf_curve_families <- function() {
  c("logistic", "gompertz", "richards", "exponential")
}

is_plf_placeholder <- function(name) {
  grepl("_plf$", as.character(name), ignore.case = TRUE)
}

plf_name_from_placeholder <- function(name) {
  sub("_plf$", "", as.character(name), ignore.case = TRUE)
}

plf_shape_names <- function(curve_name, curve = "logistic") {
  paste0(curve_name, "_", plf_shape_args(curve))
}

replace_param_names <- function(params_df, old_names, new_names) {
  df <- params_df
  drop <- setdiff(old_names, new_names)
  if (length(drop) && nrow(df))
    df <- df[!df$name %in% drop, , drop = FALSE]
  for (sn in new_names) {
    if (sn %in% df$name) next
    df <- rbind(df, empty_param_row(sn))
  }
  df
}

curves_to_df <- function(config) {
  empty <- data.frame(
    name = character(), from_file = character(), to_file = character(),
    placeholder = character(), x_values = character(), curve = character(),
    params = character(), stringsAsFactors = FALSE
  )
  if (is.null(config) || length(config$plf_curves) == 0L) return(empty)
  do.call(rbind, lapply(config$plf_curves, function(pc) {
    data.frame(
      name = pc$name,
      from_file = pc$from_file,
      to_file = pc$to_file,
      placeholder = pc$placeholder,
      x_values = paste(pc$x_values, collapse = ", "),
      curve = pc$curve,
      params = paste(pc$params, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
}

split_csv <- function(x) {
  parts <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  parts[nzchar(parts)]
}

tables_to_param_config_yaml <- function(params_df, curves_df) {
  if (is.null(params_df) || nrow(params_df) == 0L)
    stop("Add at least one parameter before saving.")
  parameters <- lapply(seq_len(nrow(params_df)), function(i) {
    row <- params_df[i, ]
    p <- list(
      name = as.character(row$name),
      default = as.numeric(row$default),
      min = as.numeric(row$min),
      max = as.numeric(row$max)
    )
    ff <- as.character(row$from_file)
    tf <- as.character(row$to_file)
    if (length(ff) && !is.na(ff) && nzchar(ff)) {
      p$from_file <- ff
      p$to_file <- if (length(tf) && !is.na(tf) && nzchar(tf)) tf else ff
    }
    p
  })
  doc <- list(parameters = parameters)
  if (!is.null(curves_df) && nrow(curves_df) > 0L) {
    doc$plf_curves <- lapply(seq_len(nrow(curves_df)), function(i) {
      row <- curves_df[i, ]
      list(
        name = as.character(row$name),
        from_file = as.character(row$from_file),
        to_file = as.character(row$to_file),
        placeholder = as.character(row$placeholder),
        x_values = as.numeric(split_csv(row$x_values)),
        curve = as.character(row$curve),
        params = split_csv(row$params)
      )
    })
  }
  yaml::as.yaml(doc, indent = 2)
}

find_dai_placeholders <- function(dir) {
  empty <- data.frame(file = character(), placeholder = character(), stringsAsFactors = FALSE)
  if (!nzchar(dir) || !dir.exists(dir)) return(empty)
  files <- list.files(dir, pattern = "\\.(dai|DAI)$", full.names = TRUE)
  if (!length(files)) return(empty)
  rows <- list()
  for (f in files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    m <- gregexpr("\\{\\{([A-Za-z0-9_]+)\\}\\}", txt, perl = TRUE)
    raw <- unique(regmatches(txt, m)[[1]])
    nms <- gsub("\\{\\{|\\}\\}", "", raw)
    for (nm in nms) {
      rows[[length(rows) + 1L]] <- data.frame(
        file = basename(f), placeholder = nm, stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

list_dlf_files <- function(project_dir) {
  if (!nzchar(project_dir) || !dir.exists(project_dir)) return(character())
  roots <- unique(c(file.path(project_dir, "Output"), project_dir))
  files <- unlist(lapply(roots, function(d) {
    if (!dir.exists(d)) return(character())
    list.files(d, pattern = "\\.dlf$", full.names = TRUE, recursive = TRUE)
  }), use.names = FALSE)
  unique(normalizePath(files, winslash = "/", mustWork = FALSE))
}

drop_calendar_names <- function(nms) {
  nms[!grepl("^(year|month|mday|day|hour)$", nms, ignore.case = TRUE)]
}

numeric_plot_cols <- function(dt) {
  nms <- names(dt)
  keep <- vapply(dt, is.numeric, logical(1))
  drop_calendar_names(nms[keep])
}

coerce_filter_value <- function(x, value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return(NULL)
  if (inherits(x, "POSIXt")) {
    parsed <- suppressWarnings(as.POSIXct(value, tz = attr(x, "tzone") %||% ""))
    if (length(parsed) && !is.na(parsed)) return(parsed)
    d <- suppressWarnings(as.Date(value))
    if (length(d) && !is.na(d)) return(as.POSIXct(d))
    return(NULL)
  }
  if (inherits(x, "Date")) {
    parsed <- suppressWarnings(as.Date(value))
    if (length(parsed) && !is.na(parsed)) return(parsed)
    return(NULL)
  }
  if (is.numeric(x)) {
    parsed <- suppressWarnings(as.numeric(value))
    if (length(parsed) && !is.na(parsed)) return(parsed)
    return(NULL)
  }
  value
}

filter_dt_rows <- function(dt, col, op, value, value2 = NULL) {
  if (is.null(dt) || !NROW(dt)) return(dt)
  col <- as.character(col %||% "")
  if (!nzchar(col) || !col %in% names(dt)) return(dt)
  op <- as.character(op %||% "")
  x <- dt[[col]]
  keep <- NULL
  if (identical(op, "between")) {
    a <- coerce_filter_value(x, value)
    b <- coerce_filter_value(x, value2)
    if (is.null(a) || is.null(b)) return(dt)
    lo <- if (a > b) b else a
    hi <- if (a > b) a else b
    keep <- x >= lo & x <= hi
  } else {
    v <- coerce_filter_value(x, value)
    if (is.null(v)) return(dt)
    keep <- switch(
      op,
      "<"  = x < v,
      ">"  = x > v,
      "="  = x == v,
      "==" = x == v,
      "<=" = x <= v,
      ">=" = x >= v,
      NULL
    )
  }
  if (is.null(keep)) return(dt)
  keep[is.na(keep)] <- FALSE
  dt[keep, , drop = FALSE]
}

summarise_dt <- function(dt, group_cols, fun) {
  if (is.null(dt) || !NROW(dt)) return(dt)
  fun <- as.character(fun %||% "")
  if (!nzchar(fun)) return(dt)
  dt <- data.table::as.data.table(dt)
  groups <- intersect(as.character(group_cols %||% character()), names(dt))
  groups <- groups[nzchar(groups)]
  if (identical(fun, "n")) {
    if (length(groups))
      return(dt[, list(n = .N), by = groups])
    return(data.table::data.table(n = nrow(dt)))
  }
  nums <- names(dt)[vapply(dt, is.numeric, logical(1))]
  nums <- setdiff(nums, groups)
  if (!length(nums)) {
    if (length(groups)) return(unique(dt[, groups, with = FALSE]))
    return(dt)
  }
  f <- switch(
    fun,
    mean   = function(x) mean(x, na.rm = TRUE),
    sum    = function(x) sum(x, na.rm = TRUE),
    min    = function(x) suppressWarnings(min(x, na.rm = TRUE)),
    max    = function(x) suppressWarnings(max(x, na.rm = TRUE)),
    median = function(x) stats::median(x, na.rm = TRUE),
    sd     = function(x) stats::sd(x, na.rm = TRUE),
    NULL
  )
  if (is.null(f)) return(dt)
  out <- if (length(groups))
    dt[, lapply(.SD, f), by = groups, .SDcols = nums]
  else
    dt[, lapply(.SD, f), .SDcols = nums]
  src_units <- attr(dt, "units")
  if (!is.null(src_units)) {
    aligned <- stats::setNames(rep(NA_character_, ncol(out)), names(out))
    common <- intersect(names(src_units), names(out))
    aligned[common] <- unname(src_units[common])
    data.table::setattr(out, "units", aligned)
  }
  out
}

make_sim_reader <- function(daily = FALSE, groups = NULL, fun = NULL) {
  process <- function(sim, label = "sim") {
    sim <- add_date(data.table::as.data.table(sim), label)
    use_fun <- as.character(fun %||% "")
    use_groups <- groups
    if (!nzchar(use_fun) && isTRUE(daily)) {
      use_fun <- "mean"
      use_groups <- "Date"
    }
    if (!nzchar(use_fun)) return(sim)
    summarise_dt(sim, use_groups, use_fun)
  }
  function(path, ..., data = NULL) {
    label <- if (is.character(path) && length(path) == 1L && nzchar(path))
      basename(path) else "sim"
    if (!is.null(data)) return(process(data, label))
    if (is.data.frame(path)) return(process(path, "sim"))
    process(read_dlf(path), label)
  }
}

obs_sets_with_data <- function(obs_sets) {
  if (is.null(obs_sets) || !length(obs_sets)) return(list())
  Filter(function(s) !is.null(s$data) && NROW(s$data) > 0, obs_sets)
}

plot_series <- function(dt, y_cols, x_col = "Date", geom = "line",
                        fill_col = NULL, facet_col = NULL, title = NULL,
                        obs_dt = NULL, obs_x = NULL, obs_y = NULL) {
  dt <- data.table::as.data.table(dt)
  y_cols <- intersect(as.character(y_cols), names(dt))
  if (!length(y_cols)) stop("Select at least one Y column.")
  if (is.null(x_col) || !nzchar(x_col) || !x_col %in% names(dt)) {
    x_col <- if ("Date" %in% names(dt)) "Date" else names(dt)[[1]]
  }
  fill_ok <- !is.null(fill_col) && nzchar(fill_col) && fill_col %in% names(dt)
  facet_ok <- !is.null(facet_col) && nzchar(facet_col) && facet_col %in% names(dt)
  id_keep <- unique(c(x_col, if (fill_ok) fill_col, if (facet_ok) facet_col))
  long <- data.table::melt(
    dt,
    id.vars = intersect(id_keep, names(dt)),
    measure.vars = y_cols,
    variable.name = "series",
    value.name = "value"
  )
  if (identical(geom, "boxplot") && !is.factor(long[[x_col]]))
    long[[x_col]] <- as.factor(long[[x_col]])

  many_y <- length(y_cols) > 1L
  mapping <- ggplot2::aes(x = .data[[x_col]], y = .data$value)
  if (many_y) {
    mapping <- ggplot2::aes(
      x = .data[[x_col]], y = .data$value,
      colour = .data$series, fill = .data$series, group = .data$series
    )
  } else if (fill_ok) {
    mapping <- ggplot2::aes(
      x = .data[[x_col]], y = .data$value,
      colour = .data[[fill_col]], fill = .data[[fill_col]],
      group = .data[[fill_col]]
    )
  }

  p <- ggplot2::ggplot(long, mapping)
  p <- switch(
    as.character(geom),
    scatter = p + ggplot2::geom_point(alpha = 0.75, na.rm = TRUE),
    bar = p + ggplot2::geom_col(
      position = if (many_y || fill_ok) "dodge" else "stack",
      na.rm = TRUE
    ),
    boxplot = p + ggplot2::geom_boxplot(na.rm = TRUE),
    p + ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::geom_point(size = 0.7, alpha = 0.65, na.rm = TRUE)
  )
  if (facet_ok)
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)), scales = "free_y")
  if (!is.null(obs_dt) && NROW(obs_dt) && !is.null(obs_y) && nzchar(obs_y) &&
      obs_y %in% names(obs_dt)) {
    ox <- if (!is.null(obs_x) && nzchar(obs_x) && obs_x %in% names(obs_dt)) obs_x
    else if (x_col %in% names(obs_dt)) x_col
    else if ("Date" %in% names(obs_dt)) "Date"
    else NULL
    if (!is.null(ox)) {
      obs_plot <- data.frame(
        x = obs_dt[[ox]],
        y = obs_dt[[obs_y]],
        stringsAsFactors = FALSE
      )
      p <- p + ggplot2::geom_point(
        data = obs_plot,
        ggplot2::aes(x = .data$x, y = .data$y),
        inherit.aes = FALSE,
        colour = "#c0392b",
        size = 2.2,
        alpha = 0.85,
        na.rm = TRUE
      )
    }
  }
  p +
    ggplot2::labs(
      title = title,
      x = col_axis_label(dt, x_col),
      y = y_axis_label(dt, y_cols),
      colour = NULL, fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
}

col_unit <- function(dt, col) {
  if (!col %in% names(dt)) return(NA_character_)
  u <- tryCatch(unname(get_unit(dt, col)), error = function(e) NA_character_)
  if (length(u) != 1L) NA_character_ else u
}

col_axis_label <- function(dt, col) {
  u <- col_unit(dt, col)
  if (is.na(u) || !nzchar(u)) col else paste0(col, " (", u, ")")
}

y_axis_label <- function(dt, y_cols) {
  units <- unique(vapply(y_cols, function(col) col_unit(dt, col), character(1)))
  units <- units[!is.na(units) & nzchar(units)]
  if (length(units) == 1L) units else NULL
}

rel_output_path <- function(project, abs_path) {
  wd <- project$project_dir
  if (!nzchar(wd) || !nzchar(abs_path)) return(abs_path)
  wd_n <- normalizePath(wd, winslash = "/", mustWork = FALSE)
  p_n <- normalizePath(abs_path, winslash = "/", mustWork = FALSE)
  if (startsWith(tolower(p_n), tolower(wd_n))) {
    rel <- substring(p_n, nchar(wd_n) + 2L)
    return(rel)
  }
  abs_path
}

named_output_files <- function(paths, project, relative = TRUE) {
  mapped <- vapply(paths, function(p) {
    if (isTRUE(relative)) rel_output_path(project, p)
    else normalizePath(p, winslash = "/", mustWork = FALSE)
  }, character(1))
  nms <- tools::file_path_sans_ext(basename(mapped))
  nms <- make.unique(gsub("[^A-Za-z0-9_]+", "_", nms), sep = "_")
  stats::setNames(mapped, nms)
}

dt_preview <- function(x, n = 12L) {
  DT::datatable(
    utils::head(as.data.frame(x), n),
    rownames = FALSE,
    options = list(dom = "t", scrollX = TRUE, pageLength = n)
  )
}

status_pill <- function(ok, ok_lab = "Ready", bad_lab = "Needed") {
  if (isTRUE(ok)) {
    htmltools::span(ok_lab, class = "status-pill status-ok")
  } else {
    htmltools::span(bad_lab, class = "status-pill status-bad")
  }
}

sa_index_table <- function(sa_obj) {
  if (inherits(sa_obj, "morris")) {
    ee <- sa_obj[["ee", exact = TRUE]]
    if (is.null(ee)) return(NULL)
    data.frame(
      name  = colnames(ee),
      mu      = colMeans(ee),
      mu_star = colMeans(abs(ee)),
      sigma   = apply(ee, 2, stats::sd),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  } else if (inherits(sa_obj, "sobol") || inherits(sa_obj, "soboljansen")) {
    S <- sa_obj[["S", exact = TRUE]]
    T_idx <- sa_obj[["T", exact = TRUE]]
    if (is.null(S) || is.null(T_idx)) return(NULL)
    data.frame(
      name = rownames(S),
      S = S[["original"]],
      ST = T_idx[["original"]],
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
}

yaml_editor_ui <- function(id, placeholder = "", height = "420px") {
  ace_editor_ui(id, placeholder = placeholder, height = height, mode = "yaml")
}

dai_editor_ui <- function(id, placeholder = "", height = "520px") {
  ace_editor_ui(id, placeholder = placeholder, height = height, mode = "text")
}

ace_editor_ui <- function(id, placeholder = "", height = "420px", mode = "yaml") {
  shinyAce::aceEditor(
    outputId = id,
    value = "",
    mode = mode,
    theme = "textmate",
    height = height,
    fontSize = 13,
    debounce = 250,
    wordWrap = FALSE,
    showLineNumbers = TRUE,
    highlightActiveLine = TRUE,
    tabSize = 2L,
    useSoftTabs = TRUE,
    showInvisibles = FALSE,
    showPrintMargin = FALSE,
    autoComplete = "disabled",
    placeholder = placeholder,
    autoScrollEditorIntoView = TRUE,
    minLines = 14L,
    maxLines = 48L
  )
}

update_yaml_editor <- function(session, editorId, value) {
  shinyAce::updateAceEditor(session, editorId, value = value %||% "", mode = "yaml")
}

update_dai_editor <- function(session, editorId, value) {
  shinyAce::updateAceEditor(session, editorId, value = value %||% "", mode = "text")
}

resolve_project_file <- function(project, rel, must_exist = FALSE) {
  rel <- trimws(rel %||% "")
  if (!nzchar(rel)) stop("Give a file path.")
  abs_win <- grepl("^[A-Za-z]:[/\\\\]", rel)
  path <- if (abs_win || startsWith(rel, "/") || startsWith(rel, "\\\\")) {
    rel
  } else {
    file.path(project$project_dir, rel)
  }
  if (isTRUE(must_exist) && !file.exists(path))
    stop("File not found: ", path)
  path
}

read_text_file <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  paste(lines, collapse = "\n")
}

write_text_file <- function(path, txt) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  txt <- txt %||% ""
  conn <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(conn), add = TRUE)
  writeLines(unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE), conn)
}
