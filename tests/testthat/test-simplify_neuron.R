context("simplify_neuron")

test_that("simplify_neuron works", {
  n1 = Cell07PNs[[1]]
  expect_known_value(simplify_neuron(n1),
                     file = 'testdata/simplify_neuron_ref.rds')
  expect_equal(simplify_neuron(n1, n=0), spine(n1))

  # check that PointNo fields can be arbitrary integers
  n1g = as.ngraph(n1, weights = TRUE)
  newids = sample(n1$d$PointNo) + 500L
  n1g = igraph::set.vertex.attribute(n1g, 'name', value = newids)
  n1.scrambledids = as.neuron(n1g)
  # nb just check equality of the seglists because the PointNo/Parent
  # fields will of course be different
  expect_equal(as.seglist(simplify_neuron(n1)),
               as.seglist(simplify_neuron(n1.scrambledids, 1)))
  expect_equal(as.seglist(simplify_neuron(n1, n=10)),
               as.seglist(simplify_neuron(n1.scrambledids, n=10)))
})
