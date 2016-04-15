#' Assembly v11 of FAFB full adult female brain EM volume
#'
#' @name FAFB11
#' @docType data
#' @examples
#' FAFB11
NULL

#' Assembly v12 of FAFB full adult female brain EM volume
#'
#' @name FAFB12
#' @docType data
#' @examples
#' FAFB12
NULL

#' ELM Landmarks created by Bock & Saalfeld groups at Janelia
#'
#' These are used to map locations in EM (FAFB) space to JFRC2013 (aka DPX)
#' space, in conjunction with a thin plate spline library.
#'
#' @details X,Y,Z correspond to locati?ons in JFRC2013 template brain. However,
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
