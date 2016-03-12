context("transforms")

test_that("can transform using elm landmarks",{
  vgloms.jfrc2013=data.frame(X=c(316, 229),
                             Y=c(143, 139), Z=c(26, 22),
                             row.names=c("V_L", "V_R"))
  vgloms.fafb11=data.frame(X = c(-161.510, -161.569),
                           Y = c(-157.878, -157.880),
                           Z = c(89.815, 89.808),
                           row.names = c("V_L", "V_R"))

  expect_equal(xform_brain(vgloms.jfrc2013, ref=JFRC2013, sample=FAFB11),
               vgloms.fafb11, tolerance=1)
})