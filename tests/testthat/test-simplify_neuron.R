context("simplify_neuron")

test_that("simplify_neuron works", {
  expect_known_value(simplify_neuron(Cell07PNs[[1]]),
                     file='testdata/simplify_neuron_ref.rds')
  expect_equal(simplify_neuron(Cell07PNs[[1]]),
                     simplify_neuron_multi(Cell07PNs[[1]], 1))
  expect_equal(nlapply(Cell07PNs, simplify_neuron),
               nlapply(Cell07PNs, simplify_neuron_multi, 1))
})
