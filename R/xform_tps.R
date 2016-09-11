#' Thin plate spline registrations via nat::xform and friends
#'
#' @description \code{tpsreg} creates an object encapsulating a thin plate spine
#'   transform mapping a paired landmark set.
#' @param sample,reference Matrices defining the sample (or floating) and
#'   reference (desired target after transformation) spaces. See details.
#' @param ... additional arguments passed to xformpoints.tpsreg
#' @details  Note that we use the \bold{nat} convention for naming the
#'   sample/reference space arguments but these actually clash with the
#'   nomenclature in the underlying \code{Morpho3d::tps3d} function. \itemize{
#'
#'   \item refmat (Morpho3d) == sample (nat)
#'
#'   \item tarmat (Morpho3d) == reference (nat)
#'
#'   }
#' @export
#' @seealso \code{\link{reglist}}, \code{\link[nat]{read.landmarks}}
tpsreg<-function(sample, reference, ...){
  structure(list(refmat=data.matrix(sample), tarmat=data.matrix(reference), ...),
            class='tpsreg')
}

#' @description \code{xformpoints.tpsreg} provides
#' @rdname tpsreg
#' @param reg The \code{tpsreg} registration object
#' @param points The 3D points to transform
#' @param swap Whether to change the direction of registration (default of
#'   \code{NULL} checks if reg has a \code{attr('swap'=TRUE)}) otherwise
#' @export
#' @importFrom nat xformpoints
xformpoints.tpsreg <- function(reg, points, swap=NULL, ...){
  if(isTRUE(swap) || isTRUE(attr(reg, 'swap'))) {
    tmp=reg$refmat
    reg$refmat=reg$tarmat
    reg$tarmat=tmp
  }
  do.call(Morpho::tps3d, c(list(x=points), reg,  list(...)))
}
