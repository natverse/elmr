context("fetchn_fafb")

test_that("fetchn can use FAFB as reference brain", {
  expect_equal(fetchn_fafb(dense_core_neurons, reference = FAFB, mirror = F),
    dense_core_neurons)
  expect_equal(fetchn_fafb(dense_core_neurons, reference = FAFB14, mirror = F),
    dense_core_neurons)
})
