test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("download_wdi works", {

  expect_named(
    download_wdi("NY.GDP.PCAP.KD"),
    c("country", "year", "region", "income",
      "is_highlight", "highlighted_country",
      "ind_1", "highlight_ind_1"
      ),
    ignore.order = TRUE
  )

  expect_named(
    download_wdi(c("NY.GDP.PCAP.KD", "SI.POV.GINI")),
    c("country", "year", "region", "income",
      "is_highlight", "highlighted_country",
      "ind_1", "ind_2",
      "highlight_ind_1", "highlight_ind_2"),
    ignore.order = TRUE
  )

  testthat::expect_gt(
    nrow(download_wdi("NY.GDP.PCAP.KD")),
    0
  )

})

test_that("modulus_breaks do not make things too weird", {

  # p_custom = 1 should mean no transformation
  custom_breaks_fn <- modulus_breaks(p_custom = 1, n.breaks_default = 10)

  test_limits <- c(0.21037169, 9.27377033)

  standard_breaks_fn <- scales::extended_breaks(n = 10)

  custom_breaks <- custom_breaks_fn(test_limits)
  standard_breaks <- standard_breaks_fn(test_limits)

  expect_lt(
    (length(custom_breaks) - length(standard_breaks)) / max(length(custom_breaks), length(standard_breaks)),
    0.2
  )

  expect_gt(
    length(intersect(standard_breaks, custom_breaks)) / max(length(custom_breaks), length(standard_breaks)),
    0.8
  )
})

# TODO: add tests for the plots
# https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot
# https://github.com/r-lib/vdiffr

