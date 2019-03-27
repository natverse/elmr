#' Remove all twigs less than a certain path length from a neuron
#'
#' @param x A \code{\link{neuron}} or \code{\link{neuronlist}} object
#' @param twig_length Twigs shorter than this will be pruned
#' @param ... Additional arguments passed to \code{\link{nlapply}},
#'   \code{\link{prune_vertices}} and eventually \code{\link{as.ngraph}}.
#'
#' @export
#' @examples
#' # Prune twigs up to 5 microns long
#' pt5=prune_twigs(Cell07PNs[1:3], twig_length = 5)
#' # compare original (coloured) and pruned (black) neurons
#' plot(Cell07PNs[1:3], WithNodes=FALSE, lwd=2, xlim=c(240,300), ylim=c(120, 90))
#' plot(cp.pt, WithNodes=FALSE, add=TRUE, lwd=2, col='black')
prune_twigs <- function (x, ...) UseMethod('prune_twigs')

#' @export
#' @rdname prune_twigs
#' @importFrom nat prune_vertices
prune_twigs.neuron <- function(x, twig_length, ...) {
  ng = as.ngraph(x, weights = T)
  if (!igraph::is_dag(ng)) {
    stop("I can't prune neurons with cycles!")
  }
  root <- rootpoints(ng, original.ids=FALSE)
  if(length(root)!=1)
    stop("I can't prune neurons with more than 1 root!")

  # plan is to label all nodes with their longest distal diameter
  # distance table from endpoints/leaves (which are now rootpoints)
  # to branchpoints
  # rows are source i.e. branchpoints
  # cols are leaves
  leaves=setdiff(endpoints(ng, original.ids=FALSE), root)
  bps=branchpoints(ng, original.ids=FALSE)
  # rows are bps, cols are eps
  dd=igraph::distances(ng, v=bps, to=leaves, mode = 'out')

  # find index of branchpoint that is furthest from end
  max_bp_idx <- apply(dd, 2, function(x) {
    # set values that are too long to signalling length of -1
    too_long=x>twig_length
    if(all(too_long)) return(NA_integer_)
    x[too_long]=-1
    wm=which.max(x)
    if(is.finite(x[wm]) & x[wm]>0) wm else NA_integer_
  })
  # nb these are indices not ids
  eps_to_prune <- leaves[is.finite(max_bp_idx)]
  bps_to_start <- bps[max_bp_idx[is.finite(max_bp_idx)]]

  # now we could find paths to those end points but some of these
  # paths will overlap with paths to vertices we want to keep ...
  # so instead, let's find paths to the end points we want to keep
  eps_to_keep <- setdiff(leaves, eps_to_prune)

  # however, it's a bit more complicated than that, because we
  # will also want to keep paths up until the branch points proximal to
  # the twigs that we are pruning - even if we prune all the twigs
  # downstream of those branch points
  nodes_to_keep=c(eps_to_keep, bps_to_start)

  # find paths from root to all the nodes that we'll keep
  # note that mode = 'o' should be fine, but mode 'all' is probably safer
  res2 <-
    igraph::shortest_paths(
      ng,
      from = root,
      to = nodes_to_keep,
      mode = 'all',
      weights = NA
    )
  verts_to_keep=unique(unlist(res2$vpath))
  # now we can basically prune everything not in that set
  prune_vertices(ng, verts_to_keep, invert=TRUE, ...)
}

#' @rdname prune_twigs
#' @inheritParams nat::nlapply
#' @export
prune_twigs.neuronlist <- function(x, twig_length, OmitFailures=NA, ...) {
  if(missing(twig_length)) stop("Missing twig_length argument")
  nlapply(x, prune_twigs, twig_length=twig_length, OmitFailures=OmitFailures, ...)
}
