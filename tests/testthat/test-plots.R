test_that("zhn_theme is a ggplot theme and the palette is stable", {
  expect_s3_class(zhn_theme(), "theme")
  expect_s3_class(zhn_theme(transparent = TRUE), "theme")
  expect_identical(zhn_pal$accent, "#1565C0")
})

test_that("bar figures return title-less ggplots with the accent fill", {
  cohort <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  p <- zhn_plot_diagnoses(cohort, col = "diagnose")
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)

  y <- zhn_plot_cases_by_year(cohort)
  expect_s3_class(y, "ggplot")
  expect_null(y$labels$title)
})

test_that("KM renders out of the box on the example data (OS / death_event)", {
  skip_if_not_installed("ggsurvfit")
  cohort <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  expect_true(all(c("os", "death_event") %in% names(cohort)))
  p <- zhn_plot_km(cohort, "os", "death_event", title = "Beispiel-OS")
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
  expect_identical(attr(p, "n_groups"), 1L)
})

test_that("KM toggles change the built object", {
  skip_if_not_installed("ggsurvfit")
  co <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  no_rt <- zhn_plot_km(co, "os", "death_event", risktable = FALSE)
  with_rt <- zhn_plot_km(co, "os", "death_event", risktable = TRUE)
  # risk table turns the return into a patchwork composite
  expect_false(inherits(no_rt, "patchwork"))
  expect_s3_class(with_rt, "patchwork")
})

test_that("KM log-rank p-value computes with a 2-level group", {
  skip_if_not_installed("ggsurvfit")
  co <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  fr <- zhncommandR:::.km_frame(co, "os", "death_event", "primaerfall")
  expect_identical(fr$n_groups, 2L)
  sd <- survival::survdiff(survival::Surv(time, event) ~ grp, data = fr$df)
  pv <- stats::pchisq(sd$chisq, df = 1, lower.tail = FALSE)
  expect_true(is.finite(pv) && pv >= 0 && pv <= 1)
  expect_s3_class(
    zhn_plot_km(co, "os", "death_event", group_col = "primaerfall",
                show_pvalue = TRUE),
    "patchwork"
  )
})

test_that("KM HR + cox.zph run with a 2-level group", {
  skip_if_not_installed("ggsurvfit")
  co <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  fr <- zhncommandR:::.km_frame(co, "os", "death_event", "primaerfall")
  cox <- survival::coxph(survival::Surv(time, event) ~ grp, data = fr$df)
  ci <- summary(cox)$conf.int[1, ]
  expect_true(all(is.finite(ci[c(1, 3, 4)])))
  expect_silent(survival::cox.zph(cox))
  expect_s3_class(
    zhn_plot_km(co, "os", "death_event", group_col = "primaerfall",
                show_hr = TRUE),
    "patchwork"
  )
})

test_that("KM pairwise (BH) has choose(k,2) rows in [0,1]", {
  skip_if_not_installed("ggsurvfit")
  co <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  pw <- zhn_km_pairwise(co, "os", "death_event", "geschlecht")
  k <- length(unique(stats::na.omit(co$geschlecht)))
  expect_identical(nrow(pw), as.integer(choose(k, 2)))
  expect_true(all(pw$p_BH >= 0 & pw$p_BH <= 1))
})

test_that("KM y-scales build", {
  skip_if_not_installed("ggsurvfit")
  co <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  for (ys in c("survival", "percent", "cuminc")) {
    expect_s3_class(zhn_plot_km(co, "os", "death_event", y_scale = ys),
                    "patchwork")
  }
})

test_that("KM export composite writes non-empty PNG and PDF", {
  skip_on_cran()
  skip_on_os("mac")  # grid text rendering is unstable on the macOS CI runner
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("ragg")
  co <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  p <- zhn_plot_km(co, "os", "death_event", group_col = "geschlecht")
  png <- withr::local_tempfile(fileext = ".png")
  pdf <- withr::local_tempfile(fileext = ".pdf")
  zhn_save_plot(p, png, format = "png", width = 8, height = 6.5)
  zhn_save_plot(p, pdf, format = "pdf", width = 8, height = 6.5)
  expect_gt(file.info(png)$size, 0)
  expect_gt(file.info(pdf)$size, 0)
})

test_that("KM curve + risk table form one aligned composite that renders", {
  skip_if_not_installed("ggsurvfit")
  skip_on_cran()
  skip_on_os("mac")  # renders to a device; grid text unstable on macOS runner
  co <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  # worst case for misalignment: grouped, 3-row risk table, legend on.
  p <- zhn_plot_km(co, "os", "death_event", group_col = "geschlecht",
                   risktable = TRUE,
                   risktable_stats = c("n.risk", "n.censor", "n.event"),
                   legend_pos = "top")
  # ggsurvfit_build returns the patchwork composite that left/right-aligns the
  # curve panel and the risk-table panel onto one shared x grid.
  expect_s3_class(p, "patchwork")
  # Render to a real device (export re-triggers any misalignment); must succeed.
  f <- withr::local_tempfile(fileext = ".png")
  zhn_save_plot(p, f, "png", width = 8, height = 6.5)
  expect_gt(file.info(f)$size, 0)
})

test_that("KM guards: all-NA event and non-numeric time raise conditions", {
  skip_if_not_installed("ggsurvfit")
  expect_error(zhn_plot_km(data.frame(t = 1:5, e = NA), "t", "e"),
               "0/1-Ereignisse")
  expect_error(zhn_plot_km(data.frame(t = letters[1:5], e = c(0, 1, 0, 1, 0)),
                           "t", "e"),
               "nicht numerisch")
})

test_that("zhn_save_plot writes non-empty PNG (600 dpi) and PDF", {
  skip_on_cran()
  skip_on_os("mac")  # grid text rendering is unstable on the macOS CI runner
  skip_if_not_installed("ragg")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) +
    ggplot2::geom_bar(fill = zhn_pal$accent) +
    zhn_theme()
  png <- withr::local_tempfile(fileext = ".png")
  pdf <- withr::local_tempfile(fileext = ".pdf")
  zhn_save_plot(p, png, format = "png", width = 6, height = 4)
  zhn_save_plot(p, pdf, format = "pdf", width = 6, height = 4)
  expect_gt(file.info(png)$size, 0)
  expect_gt(file.info(pdf)$size, 0)
})

test_that(".slugify transliterates umlauts and falls back", {
  expect_identical(
    zhncommandR:::.slugify("Kaplan-Meier Überlebenskurve"),
    "kaplan_meier_ueberlebenskurve"
  )
  expect_identical(zhncommandR:::.slugify("   "), "figure")
  expect_identical(zhncommandR:::.slugify("OS – AML!!"), "os_aml")
})
