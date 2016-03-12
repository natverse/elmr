context("transforms")

test_that("can transform using elm landmarks",{
  skip_if_not_installed('nat.flybrains')
  vgloms.jfrc2013=data.frame(X=c(316, 229),
                             Y=c(143, 139), Z=c(26, 22),
                             row.names=c("V_L", "V_R"))
  vgloms.fafb11=data.frame(X = c(695446.6, 567066.5),
                           Y = c(427872.3, 421129.0),
                           Z = c(52743.34, 58401.36),
                           row.names = c("V_L", "V_R"))

  expect_equal(xform_brain(vgloms.jfrc2013, sample  = JFRC2013, reference = FAFB11)
               , vgloms.fafb11, tolerance=0.001)

  # make sure that we can wrap the points in an object and get the same
  # results.
  fakenl <- neuronlist(dotprops(vgloms.jfrc2013, k=2))
  fakenl.fafb <- xform_brain(fakenl, sample  = JFRC2013, reference = FAFB11)
  expect_equal(xyzmatrix(fakenl.fafb), xyzmatrix(vgloms.fafb11), tolerance=0.001)

  # see if we can do a test using additional bridging registrations
  tb <- try(bridging_sequence("JFRC2", "JFRC2013"), silent = TRUE)
  if(inherits(tb, 'try-error'))
    skip("JFRC2_JFRC2013 bridging reg not available")

  expect_is(kc1 <- xform_brain(nat::kcs20[1], sample=FCWB, reference = FAFB11),
            'neuronlist')
  # NB there is some round trip error
  expect_equal(xform_brain(kc1, reference = FCWB, sample=FAFB11),
               kcs20[1],
               tolerance=0.2)
})
