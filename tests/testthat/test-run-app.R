# zhn_run_app() launches a Shiny app; we mock shiny::runApp so the test never
# blocks, and assert the temp-copy contract documented in run-app.R.

test_that("zhn_run_app copies the app to a temp dir and runs it from there", {
  testthat::skip_on_os("mac")
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    runApp = function(appDir, ...) {
      captured$dir <- appDir
      invisible(NULL)
    },
    .package = "shiny"
  )
  # When the installed app dir exists (it does under load_all), the function
  # copies it into tempdir() and runs from the copy.
  res <- zhn_run_app()
  expect_null(res)
  expect_true(nzchar(captured$dir))
  # The run dir is the temp copy, not the installed library path.
  expect_match(captured$dir, "zhncommandR-app")
  expect_true(dir.exists(captured$dir))
})

test_that("zhn_run_app falls back to the installed dir when copy fails", {
  captured <- new.env(parent = emptyenv())
  app_dir <- system.file("shiny", "zhncommandR", package = "zhncommandR")
  skip_if(!nzchar(app_dir), "app dir not available")
  local_mocked_bindings(
    file.copy = function(...) FALSE,
    .package = "base"
  )
  local_mocked_bindings(
    runApp = function(appDir, ...) {
      captured$dir <- appDir
      invisible(NULL)
    },
    .package = "shiny"
  )
  zhn_run_app()
  expect_identical(normalizePath(captured$dir), normalizePath(app_dir))
})
