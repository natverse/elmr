#' Defunct function(s) in the elmr package
#'
#' These functions are removed from the elmr package.
#' @rdname elmr-defunct
#' @name elmr-defunct
#' @aliases elmr-defunct
#' @docType package
#' @section Details:
#' \tabular{rl}{
#'   \code{distal_to} \tab is moved to \code{\link[nat]{distal_to}}\cr
#'   \code{simplify_neuron} \tab is moved to \code{\link[nat]{simplify_neuron}}\cr
#'   \code{stitch_neuron} \tab is moved to \code{\link[nat]{stitch_neuron}}\cr
#'   \code{stitch_neurons} \tab is moved to \code{\link[nat]{stitch_neurons}}\cr
#'   \code{prune_twigs} \tab is moved to \code{\link[nat]{prune_twigs}}\cr
#'   \code{tpsreg} \tab is moved to \code{\link[nat]{tpsreg}}\cr
#' }
#'
`elmr-defunct` <- function() {
 #Just a dummy function to prevent listing of the first function below in the index file..
}
distal_to <- function() {
  .Defunct("distal_to",package="nat")
}
simplify_neuron <- function() {
  .Defunct("simplify_neuron",package="nat")
}
stitch_neuron <- function() {
  .Defunct("stitch_neuron",package="nat")
}
stitch_neurons <- function() {
  .Defunct("stitch_neurons",package="nat")
}
prune_twigs <- function() {
  .Defunct("prune_twigs",package="nat")
}
tpsreg <- function() {
  .Defunct("tpsreg",package="nat")
}
NULL
