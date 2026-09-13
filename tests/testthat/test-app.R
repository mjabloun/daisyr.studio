test_that("daisyr_studio_app returns a Shiny app object", {
  skip_if_not_installed("daisyr")
  skip_if_not_installed("shiny")
  app <- daisyr_studio_app()
  expect_s3_class(app, "shiny.appobj")
})
