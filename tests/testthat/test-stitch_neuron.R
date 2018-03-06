context("distal_to")

test_that("distal_to works", {
  # test_neurons=read.neurons.catmaid(c(7495258,7495266))
  # saveRDS(test_neurons, file='tests/testthat/testdata/test_neurons.rds')
  test_neurons=readRDS('testdata/test_neurons.rds')
  expect_known_value(stitch_neurons(test_neurons), file='testdata/stitch_neurons_ref.rds')
})
