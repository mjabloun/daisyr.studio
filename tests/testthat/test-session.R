test_that("legacy registry session fields migrate to config", {
  proj <- list2env(daisyr.studio:::empty_project_state(), parent = emptyenv())
  st <- list(
    registry_yaml = "name: clay\n",
    registry_ok = TRUE,
    registry_path = "C:/tmp/registry.yaml",
    registry = list(parameters = "legacy")
  )
  daisyr.studio:::apply_project_state(proj, st)
  expect_identical(proj$config_yaml, "name: clay\n")
  expect_true(proj$config_ok)
  expect_identical(proj$config_path, "C:/tmp/registry.yaml")
  expect_identical(proj$config, list(parameters = "legacy"))
})

test_that("studio session files round-trip project paths and config yaml", {
  st <- daisyr.studio:::empty_project_state()
  st$project_dir <- "C:/tmp/daisy_proj"
  st$run_file <- "scenario.dai"
  st$config_yaml <- "name: clay\n"
  st$config_ok <- TRUE
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  daisyr.studio:::write_studio_session(tmp, st, label = "trial")
  got <- daisyr.studio:::read_studio_session(tmp)
  expect_identical(got$project_dir, "C:/tmp/daisy_proj")
  expect_identical(got$run_file, "scenario.dai")
  expect_identical(got$config_yaml, "name: clay\n")
  expect_true(got$config_ok)
  expect_null(got$last_sim)
})

test_that("session search includes the user home folder", {
  home <- daisyr.studio:::studio_session_home_dir()
  expect_match(home, "daisyr-studio-sessions$")
  dirs <- daisyr.studio:::studio_session_search_dirs(NULL)
  expect_true(
    normalizePath(home, winslash = "/", mustWork = FALSE) %in% dirs
  )
})
