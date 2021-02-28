test_that("unspike works", {
  skip_if_not(packageVersion("catmaid")>="0.10.1")
  dcn=force(dense_core_neurons)
  expect_equal(unspike(dcn, threshold = 5000), dcn)
})
