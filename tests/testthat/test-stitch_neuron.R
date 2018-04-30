context("stitch_neuron(s)")

test_that("stitch_neurons works", {
  # just a couple of anonymous neurites that I traced for this purpose
  # on 2018-03-06
  # test_neurons=read.neurons.catmaid(c(7495258,7495266))
  # saveRDS(test_neurons, file='tests/testthat/testdata/test_neurons.rds')
  test_neurons=readRDS('testdata/test_neurons.rds')
  expect_known_value(stitch_neurons(test_neurons), file='testdata/stitch_neurons_ref.rds')
})
