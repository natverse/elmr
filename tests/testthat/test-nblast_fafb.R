context("nblast_fafb2")

test_that("print nblast_fafb result", {
  assign("yu2010.dps", readRDS('testdata/yu2010.dps.rds'))
  cat(ls(), file = 'out.txt')
  nblastres=readRDS('testdata/nblastres.rds')
  expect_known_output(nblastres, 'testdata/nblastres.print', print = T)
})
