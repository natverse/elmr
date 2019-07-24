context("test-open_fafb.R")

test_that("open_fafb works", {
  baseline <-
    data.frame(
      treenode_id = c(10362518L, 18699757L, 18699746L),
      parent_id = c(10362517L, 18699755L, 18699745L),
      x = c(363719, 455145, 455425),
      y = c(149281, 244639, 245487),
      z = c(181280, 34680, 33680),
      confidence = c(5L, 5L, 5L),
      radius = c(-1,-1,-1),
      skid = c(16L, 16L, 16L),
      edition_time = c(1490896452.62488, 1508753040.35593, 1508753020.87421),
      user_id = c(117L, 167L, 167L),
      url = c(
        "https://neuropil.janelia.org/tracing/fafb/v14/?pid=1&zp=181280&yp=149281&xp=363719&tool=tracingtool&sid0=5&s0=1.000000&active_skeleton_id=16&active_node_id=10362518",
        "https://neuropil.janelia.org/tracing/fafb/v14/?pid=1&zp=34680&yp=244639&xp=455145&tool=tracingtool&sid0=5&s0=1.000000&active_skeleton_id=16&active_node_id=18699757",
        "https://neuropil.janelia.org/tracing/fafb/v14/?pid=1&zp=33680&yp=245487&xp=455425&tool=tracingtool&sid0=5&s0=1.000000&active_skeleton_id=16&active_node_id=18699746"
      ),
      stringsAsFactors = FALSE
    )

  testdf <- baseline[,-ncol(baseline)]
  expect_equal(open_fafb(testdf), baseline$url)
  # make a fake connection to specify server URL
  fakeconn=catmaid_connection(server="https://neuropil.janelia.org/tracing/fafb/v14/")
  expect_equal(open_fafb(testdf, server=fakeconn), baseline$url)
  fakeconn2=catmaid_connection(server="https://neuropil.janelia.org/tracing/fafb/v14-seg/")
  expect_equal(open_fafb(testdf, server=fakeconn2), sub("v14", "v14-seg", baseline$url))

  expect_equal(open_fafb(testdf[1,]), baseline$url[1])
  expect_error(open_fafb(testdf[1,], active_node_id = testdf$treenode_id,
                       active_skeleton_id = testdf$skid, open=FALSE))
  expect_equal(open_fafb(testdf[1,], active_node_id = testdf$treenode_id[1],
                         active_skeleton_id = testdf$skid[1], open=FALSE),
               baseline$url[1])
  expect_equal(open_fafb(xyzmatrix(testdf), active_node_id = testdf$treenode_id,
                         active_skeleton_id = testdf$skid, rowwise = TRUE),
               baseline$url)
  expect_length(open_fafb(xyzmatrix(testdf), rowwise = TRUE), 3)
  expect_length(open_fafb(testdf[,c("x","y","z")], rowwise = TRUE), 3)
})

