#' templatebrain for assembled FAFB full adult female brain EM volume
#'
#' @description \bold{FAFB} \code{\link[nat.templatebrains]{templatebrain}}
#'   objects encapsulate the physical dimensions of assembled FAFB serial
#'   section data. There is a different \code{templatebrain} for each assembly
#'   version.They are used to define the transformations between FAFB and other
#'   templates like \code{\link[nat.flybrains]{JFRC2013}} (see
#'   \code{\link{xform_brain}})
#'
#' @details  \code{FAFB12} is the default FAFB assembly since 2016-04-01.
#' @name FAFB
#' @docType data
#' @seealso \code{\link[nat.templatebrains]{templatebrain}},
#'   \code{\link{xform_brain}}
#' @examples
#' FAFB12
#' dim(FAFB12)
#' voxdims(FAFB12)
#' boundingbox(FAFB12)
NULL

#' @description \code{FAFB12} Assembly v12 of FAFB full adult female brain EM
#'   volume (2016-04-01)
#' @name FAFB12
#' @rdname FAFB
#' @docType data
NULL

#' @description \code{FAFB11} Assembly v11 of FAFB full adult female brain EM
#'   volume (2016-02-09)
#'
#' @name FAFB11
#' @rdname FAFB
#' @docType data
NULL

#' ELM Landmarks created by Bock & Saalfeld groups at Janelia
#'
#' These are used to map locations in EM (FAFB) space to JFRC2013 (aka DPX)
#' space, in conjunction with a thin plate spline library.
#'
#' @details X,Y,Z correspond to locations in JFRC2013 template brain. However,
#'   they are in raw voxel coordinates. In contrast, X1,Y1,Z1 (which contain
#'   locations in FAFB space) are in nm.
#'
#' @name elm.landmarks
#' @docType data
#' @examples
#' head(elm.landmarks)
#'
#' \dontrun{
#' # Show landmarks in the context of the JFRC2013 template brain
#' library(nat.flybrains)
#' plot3d(JFRC2013)
#' # note that the landmarks are in raw voxel coordinates and must be scaled
#' xyz=scale(elm.landmarks[,c("X","Y","Z")], scale = 1/voxdims(JFRC2013), center = FALSE)
#' spheres3d(xyz, col = ifelse(elm.landmarks$Use, "green", 'red'), radius = 4)
#' }
NULL


#' Sample EM tracings (homolougous pair of neurons wrapping around lateral horn)
#'
#' @details These neurons were traced by Greg Jefferis because they appeared to be a
#' homologous pair from the left and right side of the brain. They contain dense
#' core vesicles (which made it easier to find matching cell bodies) and skirt
#' around the outside of the lateral horn.
#'
#' @examples
#' \dontrun{
#' dense_core_neurons <- read.neurons.catmaid(c(713968, 822008))
#' }
#' @docType data
#' @name dense_core_neurons
NULL
