#' Simplify a neuron to include its root and specified number of branch points
#' @details If the neuron already contains fewer than or exactly the requested
#'   number of branches, then the original neuron is returned. The approach is
#'   to build up the new neuron starting from the longest tree including no
#'   branches all the way up to the longest tree containing n branches. The
#'   distance calculations are only carried out once so it should be reasonably
#'   efficient. Nevertheless at each iteration, the longest path from the tree
#'   so far to the newly selected leaf is calculated and it is likely that this
#'   step could be avoided. Furthermore for large values of n, pruning excess
#'   branches rather than building would presumably be more efficient.
#'
#'   This is still a somewhat experimental function (hence its location in elmr
#'   rather than nat).
#' @param x A \code{\link[nat]{neuron}} to simplify
#' @param n Required number of branch points (default=1, minimum 0)
#' @param ... Additional arguments (currently ignored)
#'
#' @return The simplified \code{neuron} or the untouched original neuron for
#'   neurons that have <=n branch points.
#' @importFrom nat prune_vertices endpoints rootpoints branchpoints
#' @export
#' @seealso \code{\link[nat]{spine}}
#' @examples
#' \donttest{
#' dl1=read.neuron.catmaid(catmaid_skids('name:DL1')[1])
#' dl1.simp=simplify_neuron(dl1)
#' dl1.simp4=simplify_neuron(dl1, n=4)
#' plot(dl1, col='green', WithNodes = F)
#' plot(dl1.simp, col='red', add = T)
#' plot(dl1.simp4, col='blue', add = T)
#' }
simplify_neuron <- function(x, n=1, ...) {
  nbps=length(branchpoints(x))
  if (nbps <= n)
    return(x)
  if (n < 0)
    stop("Must request >=0 branch points!")

  ng = as.ngraph(x, weights = T)
  if (!igraph::is_dag(ng)) {
    stop("I can't simplify neurons with cycles!")
  }
  # plan is to label all nodes with their longest distal diameter
  # distance table from endpoints/leaves (which are now rootpoints)
  # to branchpoints
  # rows are source i.e. branchpoints
  # cols are leaves
  leaves=setdiff(endpoints(ng, original.ids=FALSE), rootpoints(ng, original.ids=FALSE))
  bps=branchpoints(ng, original.ids=FALSE)
  dd=igraph::distances(ng, v=bps, to=leaves, mode = 'out')

  # so we know how many descendant paths we can consider for each node
  bpdesccount=igraph::ego_size(ng, order = 1, nodes = bps, mode='out', mindist = 1)
  names(bpdesccount)=bps
  bpsused=rep(0L, length(bps))
  names(bpsused)=bps
  lp <- function(from, to) {
    res=igraph::get.shortest.paths(
    ng,
    from = from,
    to = to,
    mode = "out")
    as.integer(res$vpath[[1]])
  }
  lp_verts=list()
  for (i in 0:n) {
    if (i == 0) {
      # initialisation
      start = rootpoints(ng, original.ids=FALSE)
      furthest_leaf_idx = which.max(apply(dd, 2,  function(x) max(x[is.finite(x)])))
    } else {
      # select the bps that we can consider
      # must be currently in use but not all used up
      bps_available = bpsused > 0 & bpsused < bpdesccount

      # find the length we could add for each leaf
      # nb this will be the smallest value that can be added to
      # currently selected nodes
      additional_length = apply(dd[bps_available,], 2, min, na.rm = T)
      # remove any infinite values
      additional_length[!is.finite(additional_length)] = 0
      # the next leaf to add is the one with max length
      furthest_leaf_idx = which.max(additional_length)
      start = which.min(dd[bps_available, furthest_leaf_idx])
      # nb we need the vertex index in the original graph
      start = match(names(start), names(igraph::V(ng)))
    }
    furthest_leaf = leaves[furthest_leaf_idx]
    # strike off selected leaf
    dd[, furthest_leaf_idx] = Inf
    # find path to that leaf
    lp_verts[[i+1]] = lp(start, furthest_leaf)
    # add one to count of any bps used
    bpsused[bps %in% lp_verts[[i+1]]] = bpsused[bps %in% lp_verts[[i+1]]] + 1
  }
  # ok now we have as output a list of vertices defining selected paths
  # subset original neuron keeping vertices in that list
  # subset(x, unique(unlist(lp_verts)))
  prune_vertices(ng, unique(unlist(lp_verts)), invert = T)
}
