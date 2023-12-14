skip_if_not_installed("wooldridge")
df <- wooldridge::wagepan
Y <- "lwage"
G <- "nr"
T <- "year"
D <- "union"
controls <- c("hours")
placebo <- 2
dynamic <- 2


test_that("basic results", {
  result = did_multiplegt(
    df, Y, G, T, D, controls, placebo = placebo, dynamic = dynamic,
    average_effect = "prop_number_switchers"
  )

  expect_equal(result$placebo_2, -0.09930194, tolerance = 1e-5)
  expect_equal(result$N_placebo_2, 2158)
  expect_equal(result$placebo_1, 0.08446127, tolerance = 1e-5)
  expect_equal(result$N_placebo_1,2842)
  expect_equal(result$effect, 0.02147226, tolerance = 1e-5, ignore_attr = TRUE)
  expect_equal(result$N_effect, 3815)
  expect_equal(result$N_switchers_effect, 508)
})

test_that("basic results 2", {
  cluster = NULL
  recat_treatment = NULL
  trends_nonparam = NULL
  trends_lin = "nr"
  result = did_multiplegt(df, Y, G, T, D, controls, placebo = placebo, dynamic = dynamic,
                          trends_lin = trends_lin, trends_nonparam = trends_nonparam)


  expect_equal(result$placebo_2, -0.09930194, tolerance = 1e-5)
  expect_equal(result$N_placebo_2, 2158)
  expect_equal(result$placebo_1, 0.08446127, tolerance = 1e-5)
  expect_equal(result$N_placebo_1,2842)
  expect_equal(result$effect, 0.02147226, tolerance = 1e-5, ignore_attr = TRUE)
  expect_equal(result$N_effect, 3815)
  expect_equal(result$N_switchers_effect, 508)
})

test_that("basic results 3", {
  cluster = NULL
  recat_treatment = NULL
  trends_nonparam = "black"
  trends_lin = NULL
  result = did_multiplegt(df, Y, G, T, D, controls, placebo = placebo, dynamic = dynamic,
                          trends_lin = trends_lin, trends_nonparam = trends_nonparam)

  expect_equal(result$placebo_2, -0.09930194, tolerance = 1e-5)
  expect_equal(result$N_placebo_2, 2158)
  expect_equal(result$placebo_1, 0.08446127, tolerance = 1e-5)
  expect_equal(result$N_placebo_1,2842)
  expect_equal(result$effect, 0.02147226, tolerance = 1e-5, ignore_attr = TRUE)
  expect_equal(result$N_effect, 3815)
  expect_equal(result$N_switchers_effect, 508)
})


