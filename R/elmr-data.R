#' templatebrain for assembled FAFB full adult female brain EM volume
#'
#' @description \code{FAFB} Current default assembly of FAFB (an alias for
#'   FAFB13 since 2016-12-07)
#'
#' @details \bold{FAFB} \code{\link[nat.templatebrains]{templatebrain}} objects
#'   encapsulate the physical dimensions of assembled FAFB serial section data.
#'   There is a different \code{templatebrain} for each assembly version. They
#'   are used to define the transformations between FAFB and other templates
#'   like \code{\link[nat.flybrains]{JFRC2013}} (see \code{\link{xform_brain}})
#'
#'   \code{FAFB13} is the default FAFB assembly since 2016-12-07. If you want to
#'   use a specific assembly then refer to it with a versioned name. If you just
#'   want to use the current working assembly, then use \code{FAFB}.
#'
#' @name FAFB
#' @docType data
#' @seealso \code{\link[nat.templatebrains]{templatebrain}},
#'   \code{\link{xform_brain}}
#' @examples
#' FAFB13
#' dim(FAFB13)
#' voxdims(FAFB13)
#' boundingbox(FAFB13)
NULL

#' @description \code{FAFB14} Assembly v14 of FAFB full adult female brain EM
#'   volume (2017-09 - ).
#' @name FAFB14
#' @rdname FAFB
#' @docType data
NULL

#' @description \code{FAFB13} Assembly v13 of FAFB full adult female brain EM
#'   volume (2016-12-07 - ).
#' @name FAFB13
#' @rdname FAFB
#' @docType data
NULL


#' @description \code{FAFB12} Assembly v12 of FAFB full adult female brain EM
#'   volume (2016-04-01 to 2016-12-06)
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
#'   The current iteration of these landmarks are in FAFB13 space
#'
#' @name elm.landmarks
#' @docType data
#' @references
#' See \url{https://github.com/saalfeldlab/elm} for the original source of the
#' landmarks.
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

#' @description \code{elm.landmarks.12} contains the landmark positions used for
#'   the FAFB12 assembly as a historical record.
#' @name elm.landmarks.12
#' @rdname elm.landmarks
#' @docType data
NULL


#' Sample EM tracings (homologous pair of neurons wrapping around lateral horn)
#'
#' @details These neurons were traced by Greg Jefferis because they appeared to be a
#' homologous pair from the left and right side of the brain. They contain dense
#' core vesicles (which made it easier to find matching cell bodies) and skirt
#' around the outside of the lateral horn.
#'
#' @examples
#' plot(dense_core_neurons)
#' \dontrun{
#' dense_core_neurons <- read.neurons.catmaid(c(713968, 822008))
#' }
#' @docType data
#' @name dense_core_neurons
NULL

#' FAFB13 brain and neuropil surface objects
#'
#' @description \code{FAFB13.surf} describes the neuropil surface of the FAFB13
#'   brain.
#'
#' @details These neuropil regions are generated from the
#'   \code{\link[nat.flybrains]{JFRC2.surf}} and
#'   \code{\link[nat.flybrains]{JFRC2NP.surf}} objects, respectively. Because
#'   they are not defined from FAFB13 itself but transformed using bridging
#'   registrations, they are only as good as those bridging registrations. In
#'   particular although the \code{\link{elm.landmarks}} used to bridge
#'   FAFB13-JFRC2013 are well defined in the fly's right brain hemisphere, they
#'   are somewhat less dense in other areas. Therefore these surfaces are clearly
#'   likely less accurate in the left brain hemisphere and show significanct
#'   displacements in the optic lobes. Note that updated landmarks in March 2017
#'   significantly improved the situation.
#'
#'   The acronyms for the neuropil regions are those used in Ito et al. 2014 and
#'   can also be looked up at \url{http://virtualflybrain.org}.
#' @examples
#' boundingbox(FAFB13.surf)
#' FAFB13NP.surf$RegionList
#' \dontrun{
#' plot3d(FAFB13.surf)
#' # a short cut which plots the surface in semi-transparent grey.
#' clear3d()
#' plot3d(FAFB13)
#'
#' # plot right lateral horn
#' plot3d(FAFB13NP.surf, "LH_R", alpha=0.4)
#' # plot both antennal lobe surfaces (second arg is regex if no exact match)
#' # note use of initial anchor (^) to avoid matches to LAL
#' plot3d(FAFB13NP.surf, "^AL", alpha=0.4)
#' }
#' @docType data
#' @name FAFB13.surf
#' @seealso \code{\link{FAFB13}}, \code{\link{FAFB13}}
NULL

#' @description \code{FAFB13NP.surf} is a surface object for the Ito et al
#'   (2014) neuropil regions in FAFB13 space
#' @docType data
#' @name FAFB13NP.surf
#' @rdname FAFB13.surf
NULL
