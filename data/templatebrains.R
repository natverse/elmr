FAFB11 <- nat.templatebrains::templatebrain(
  "FAFB11",
  type = 'Stitched serial section EM volume of whole adult female brain', sex = 'F',
  dims = c(307041, 169832, 7062), voxdims = c(4,4,35), units = 'nm',
  BoundingBox=nat::boundingbox(nat::im3d(dims = c(307041, 169832, 7062),
                                         voxdims = c(4,4,35))),
  origin=c(0,0,0)
)

# Created by
# FAFB12=elmr:::make_template()
FAFB12 <- nat.templatebrains::templatebrain(
  name = "FAFB12",
  type = 'Stitched serial section EM volume of whole adult female brain',
  sex = 'F',
  dims = c(229791L, 142889L, 7063L),
  voxdims = c(4, 4, 35),
  origin = c(0, 0, 0),
  BoundingBox = structure(c(0, 919160, 0, 571552, 0, 247170), .Dim = 2:3, class = "boundingbox"),
  units = "nm"
)

FAFB13 <- nat.templatebrains::templatebrain(
  name = "FAFB13",
  type = 'Stitched serial section EM volume of whole adult female brain',
  sex = 'F',
  # FIXME these dims are certainly wrong!
  dims = c(229791L, 142889L, 7063L),
  voxdims = c(4, 4, 35),
  origin = c(0, 0, 0),
  BoundingBox = structure(c(0, 919160, 0, 571552, 0, 247170), .Dim = 2:3, class = "boundingbox"),
  units = "nm"
)
