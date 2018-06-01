#' Title
#'
#' @param x A \code{\link{ngraph}} object
#' @param type They type of arrows (lines by default)
#' @param soma radius of soma (or \code{FALSE} to supress plotting)
#' @param labels Whether to label nodes/alll points with their raw index (not id)
#' @param ... Additional arguments passed to \code{\link{arrow3d}}
#'
#' @export
#'
#' @importFrom igraph as_edgelist V
#' @importFrom rgl arrow3d par3d spheres3d text3d
#' @examples
#' plot3d(as.ngraph(Cell07PNs[[1]]))
plot3d.ngraph <- function(x, type='lines', soma=1, labels=c('none', "nodes","all"), ...) {
  labels=match.arg(labels)
  el=igraph::as_edgelist(x, names=F)
  xyz=xyzmatrix(x)

  draw_edge <- function(edge, ...) {
    e1=edge[1]
    e2=edge[2]
    if(e1 == e2) return()
    p1=xyz[e1,]
    p2=xyz[e2,]
    if(any(is.na(c(p1,p2))) || p1==p2) {
      message("Bad edge: ", edge)
      return()
    }
    # cat(edge,"\n")
    arrow3d(xyz[edge[1],], xyz[edge[2],], type=type, ...)
  }
  op=par3d(skipRedraw=T)
  on.exit(par3d(op))
  apply(el, 1, draw_edge, ...)
  rp=rootpoints(x)
  if(isTRUE(all.equal(soma, FALSE))) {
    # don't plot
  } else spheres3d(xyz[rp,, drop=F], col='magenta', radius=soma)
  if(labels!='none') {
    all_points=igraph::V(x)
    pointsel <- if(isTRUE(labels=='all')) {
      all_points
    } else {
      unique(c(branchpoints(x), rootpoints(x), endpoints(x)))
    }
    text3d(xyz[pointsel,],texts = pointsel)
  }
}
