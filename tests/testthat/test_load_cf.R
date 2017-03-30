context("Loading curve fit results")

test_that('gr_cf_read loads all curve fits in a folder', {
  folder <- 'load_cf_data'
  data <- gr_cf_read(folder)

  expect_is(data, "tbl_df")
  expect_true(all(c('id', 'x', 'y') %in% colnames(data)))
})
