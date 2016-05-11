#' Bridge light and EM flybrain datasets
#'
#' \bold{elmr} builds on the \bold{nat} and \bold{catmaid} packages to provide
#' tools to read, analyse, plot, transform and convert neuroanatomical data,
#' especially as it relates to whole brain EM voumes generated at the HHMI
#' Janelia Research Campus (groups of Davi Bock, Stephan Saalfeld and many
#' collaborators).
#'
#' @section Interesting functions/data:
#'
#'   \itemize{
#'
#'   \item \code{\link{xform_brain}}
#'
#'   allows you to transform FAFB data into a large number of other template
#'   brain spaces by making use of the \code{\link{nat.templatebrains}} and
#'   \code{\link{nat.flybrains}} packages. The ability to move beyond the
#'   \code{\link[nat.flybrains]{JFRC2013}} template brain depends on having a
#'   functional CMTK installation. See \code{\link[nat]{cmtk.bindir}} for more
#'   information and advice about installation.
#'
#'   \item \code{\link{fetchn_fafb}} allows you to fetch FAFB catmaid tracings
#'   and transform to a new template.
#'
#'   \item \code{\link{nblast_fafb}} allows you to nblast catmaid tracings
#'   against a set of neuron skeletons e.g. from flycircuit.
#'
#'   \item \code{\link{open_fafb}} allows you to open a catmaid session in your
#'   browser at a specific xyz location based on selecting a position in any
#'   template brain space e.g. a flycircuit.tw neuron or a JFRC2010 registered
#'   GAL4 line.
#'
#'   \item \code{\link{stitch_neurons}} allows neuron fragments to be stitched
#'   together reasonably intelligently.
#'
#'   \item \code{\link{FAFB12}} a
#'   \code{\link[nat.templatebrains]{templatebrain}} object specifying the
#'   dimensions of the FAFB EM volume.
#'
#'   \item \code{\link{elm.landmarks}} a set of landmarks that can define a thin
#'   plate spline transform mapping FAFB to light level template brains.
#'
#'   }
#' @section Authentication: Interacting with a catmaid server will normally
#'   require authentication. Please see
#'   \url{https://github.com/jefferis/rcatmaid#Authentication} for details of
#'   how to set up authentication. If you regularly use a particular catmaid
#'   server it is recommended to put authentication in your
#'   \code{\link{Rprofile}} file.
#'
#' @name elmr-package
#' @aliases elmr
#' @docType package
#' @keywords package
#' @import nat.templatebrains
NULL
