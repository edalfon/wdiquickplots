test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("download_wdi_ind works", {

  expect_named(
    download_wdi_ind("NY.GDP.PCAP.KD"),
    c("country", "year", "ind_1", "region", "income", "highlight_ind_1"),
    ignore.order = TRUE
  )

  expect_named(
    download_wdi_ind(c("NY.GDP.PCAP.KD", "SI.POV.GINI")),
    c("country", "year", "ind_1", "ind_2", "region", "income",
      "highlight_ind_1", "highlight_ind_2"),
    ignore.order = TRUE
  )

  testthat::expect_gt(
    nrow(download_wdi_ind("NY.GDP.PCAP.KD")),
    0
  )

})
