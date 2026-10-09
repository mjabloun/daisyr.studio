test_that("params_to_df exposes expression and tables_to_yaml writes it", {
  skip_if_not_installed("daisyr")
  skip_if_not_installed("yaml")
  yaml_txt <- "
parameters:
  - name: Ap_silt
    from_file: soil.dai
    to_file: soil.dai
    default: 0.25
    min: 0.10
    max: 0.40
  - name: Ap_sand
    from_file: soil.dai
    to_file: soil.dai
    default: 0.50
    min: 0.30
    max: 0.70
  - name: Ap_clay
    from_file: soil.dai
    to_file: soil.dai
    expression: 1 - Ap_silt - Ap_sand
    min: 0
    max: 1
"
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(yaml_txt, tmp)
  config <- daisyr::read_param_config(tmp)
  df <- daisyr.studio:::params_to_df(config)
  expect_true("expression" %in% names(df))
  expect_equal(df$expression[df$name == "Ap_clay"], "1 - Ap_silt - Ap_sand")
  expect_equal(df$role[df$name == "Ap_clay"], "derived")
  expect_equal(df$expression[df$name == "Ap_silt"], "")

  out <- daisyr.studio:::tables_to_param_config_yaml(df, NULL)
  parsed <- yaml::yaml.load(out)
  clay <- parsed$parameters[[which(vapply(parsed$parameters, function(p) p$name, "") == "Ap_clay")]]
  expect_equal(clay$expression, "1 - Ap_silt - Ap_sand")
  expect_null(clay$default)
})

test_that("a _PARTIT placeholder round-trips as a partit entry", {
  skip_if_not_installed("daisyr")
  skip_if_not_installed("yaml")
  expect_true(daisyr.studio:::is_partit_placeholder("Shoot_PARTIT"))
  expect_false(daisyr.studio:::is_partit_placeholder("Shoot_PLF"))
  expect_equal(daisyr.studio:::partit_name_from_placeholder("Shoot_PARTIT"), "Shoot")

  params <- do.call(rbind, lapply(seq_len(4), function(i) {
    specs <- daisyr.studio:::partit_shape_specs()
    row <- daisyr.studio:::empty_param_row(paste0("Shoot_", specs$suffix[i]))
    row$default <- specs$default[i]
    row$min <- specs$min[i]
    row$max <- specs$max[i]
    row$from_file <- ""
    row$to_file <- ""
    row$role <- "curve_input"
    row
  }))
  partit <- daisyr.studio:::empty_partit_row(
    "Shoot",
    paste(daisyr.studio:::partit_shape_names_studio("Shoot"), collapse = ", ")
  )
  partit$from_file <- "crop.dai"
  partit$to_file <- "crop.dai"
  out <- daisyr.studio:::tables_to_param_config_yaml(params, NULL, partit)
  expect_true(grepl("partit:", out, fixed = TRUE))
  expect_false(grepl("plf_curves:", out, fixed = TRUE))

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(out, tmp)
  config <- daisyr::read_param_config(tmp)
  expect_equal(config$partit$Shoot$placeholder, "Shoot_PARTIT")
  expect_equal(config$parameters[name == "Shoot_sorg_steepness"]$role, "curve_input")
  expect_equal(length(config$partit$Shoot$x_values), 21)
})

test_that("empty_param_row includes an expression column", {
  row <- daisyr.studio:::empty_param_row("Ap_clay")
  expect_true("expression" %in% names(row))
  expect_equal(row$expression, "")
})
