library(AnomalyDetection)
context("Evaluation: AnomalyDetectionTs")

test_that("last day, both directions, with plot", {
  results <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', only_last='day', plot=T)
  expect_equal(length(results$anoms), 2)
  expect_equal(length(results$anoms[[2L]]), 25)
  expect_equal(class(results$plot), c("gg", "ggplot"))
})

test_that("both directions, e_value, with longterm", {
  results <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', longterm=TRUE, e_value=TRUE)
  expect_equal(length(results$anoms), 3)
  expect_equal(length(results$anoms[[2L]]), 129)
  expect_equal(results$plot, NULL)
})

test_that("both directions, e_value, threshold set to med_max", {
  results <- AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', threshold="med_max", e_value=TRUE)
  expect_equal(length(results$anoms), 3)
  expect_equal(length(results$anoms[[2L]]), 86)
  expect_equal(results$plot, NULL)
})

test_that("Anomalies are detected when not padding time gaps", {
  results <- AnomalyDetectionTs(raw_data_time_gaps, max_anoms=0.01, direction='both', alpha = 1e-12, longterm = FALSE, threshold='p99', only_last = 'hr', e_value=TRUE, pad = NULL)
  expect_equal(length(results$anoms), 3)
  expect_equal(length(results$anoms[[2L]]), 1)
})

test_that("Anomalies are not detected when padding time gaps", {
  results <- AnomalyDetectionTs(raw_data_time_gaps, max_anoms=0.01, direction='both', alpha = 1e-12, longterm = FALSE, threshold='p99', only_last = 'hr', e_value=TRUE, pad = 'interpolate')
  expect_equal(length(results$anoms), 0)
  expect_equal(results$plot, NULL)
})

test_that("When only daily seasonality is decomposed in multiple seasonalities data -> anomaly detected", {
  results <- AnomalyDetectionTs(raw_data_multiple_seasonalities, max_anoms=0.01, direction='both', alpha = 1e-12, longterm = FALSE, threshold='p99', only_last = 'hr', e_value=TRUE, pad = 'interpolate')
  expect_equal(length(results$anoms), 3)
  expect_equal(length(results$anoms[[2L]]), 1)
})

test_that("When both daily and weekly seasonalities are decomposed -> no anomalies found", {
  results <- AnomalyDetectionTs(raw_data_multiple_seasonalities, max_anoms=0.01, direction='both', alpha = 1e-12, longterm = FALSE, threshold='p99', only_last = 'hr', e_value=TRUE, pad = 'interpolate', seasonalities = list(1L,7L))
  expect_equal(length(results$anoms), 0)
})
