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

test_that("empty_param_row includes an expression column", {
  row <- daisyr.studio:::empty_param_row("Ap_clay")
  expect_true("expression" %in% names(row))
  expect_equal(row$expression, "")
})
