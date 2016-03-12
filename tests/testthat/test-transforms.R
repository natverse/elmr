context("transforms")

test_that("can transform using elm landmarks",{
  vgloms.jfrc2013=data.frame(X=c(316,229), Y=c(143, 139), Z=c(26,22), row.names=c("V_L", "V_R"))
  expect_equal(xform_brain(vgloms.jfrc2013, ref=JFRC2013, sample=FAFB11))
})