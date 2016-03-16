#' Bridge light and EM flybrain datasets
#'
#' \bold{elmr} provides builds on the \bold{nat} and \bold{catmaid} packages to
#' provide tools to read, analyse, plot, transform and convert neuroanatomical
#' data, especially as it relates to whole brain EM voumes generated at the HHMI
#' Janelia Research Campus (groups of Davi Bock, Stephan Saalfeld and many
#' collaborators).
#'
#' Interesting functions include:
#'
#' \itemize{
#'
#' \item \code{\link{xform_brain}}
#'
#' which allows you to transform FAFB data into a large number of other template
#' brain spaces by making use of the \code{\link{nat.templatebrains}} and
#' \code{\link{nat.flybrains}} packages. The ability to move beyond the
#' \code{\link[nat.flybrains]{JFRC2013}} template brain depends on having a
#' functional CMTK installation. See \code{\link[nat]{cmtk.bindir}} for more
#' information and advice about installation.
#'
#' \item \code{\link{stitch_neurons}} which allows neuron fragments to be
#' stitched together reasonably intelligently.
#'
#' }
#' @name elmr-package
#' @aliases elmr
#' @docType package
#' @keywords package
#' @import nat.templatebrains
NULL