context("nblast_fafb")

test_that("print nblast_fafb result", {
  nblastres=readRDS('testdata/nblastres.rds')
  expect_known_output(nblastres, 'testdata/nblastres.print', print = T)
})
