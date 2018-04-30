context("distal_to")

test_that("distal_to works", {
  x=dense_core_neurons[[1]]
  dcv=x$tags[['dense core vesicles']]
  dcv.idx=match(dcv, x$d$PointNo)
  expect_equal(dtdcv <- distal_to(x, node.pointno = dcv),
               distal_to(x, node.idx = dcv.idx))

  # manually work out
  matching_seg_id=which(sapply(x$SegList, function(x) any(dcv.idx %in% x)))
  matching_seg=x$SegList[[matching_seg_id]]

  matching_part = matching_seg[seq.int(from = which(matching_seg == dcv.idx),
                               to = length(matching_seg))]

  expect_equal(matching_part, dtdcv[1:length(matching_part)],
               label = 'Initial part of distal_to returns correct indices')

  # check that if we specify the soma things still work
  expect_equal(distal_to(x, node.pointno = dcv, root.idx = x$StartPoint), dtdcv)
  expect_equal(distal_to(x, node.pointno = dcv, root.pointno = x$tags$soma), dtdcv)
})
