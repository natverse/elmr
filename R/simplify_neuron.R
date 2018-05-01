#' Simplify a neuron to include its root and two longest branches
#' @details This is still a somewhat experimental function (hence its location
#'   in elmr rather than nat). It would be nice to add the option to retain the
#'   most important N branches, rather than the first 1 as here.
#' @param x A \code{\link[nat]{neuron}} to simplify
#' @param ... Additional arguments (currently ignored)
#'
#' @return The simplified \code{neuron} or the untouched original neuron for
#'   neurons that have <=1 branch point.
#' @export
#' @seealso \code{\link[nat]{spine}}
#' @examples
#' \donttest{
#' dl1=read.neuron.catmaid(catmaid_skids('name:DL1')[1])
#' dl1.simp=simplify_neuron(dl1)
#' plot(dl1, col='green', WithNodes = F)
#' plot(dl1.simp, col='red', add = T)
#' }
simplify_neuron <- function(x, ...) {
  if(length(nat::branchpoints(x))<2)
    return(x)
  ng=as.ngraph(x, weights=T)
  if(!igraph::is_dag(ng)) {
    stop("I can't simplify neurons with cycles!")
  }
  # find spine of original neuron
  lps = igraph::shortest.paths(graph = ng, x$StartPoint, to = x$EndPoints, mode = "out")
  end=x$EndPoints[which.max(lps)]
  longestpath = igraph::get.shortest.paths(
    ng,
    from = x$StartPoint,
    to = end,
    mode = "out",
    output = 'both'
    )
  # delete spine nodes
  ng2=igraph::delete_edges(ng, edges = longestpath$epath[[1]])
  # get longest path in what's left
  d=igraph::get_diameter(ng2, directed = T)
  # nb we have to convert to integer because otherwise igraph complains that
  # they come from separate graphs
  nodes_to_keep=unique(c(as.integer(longestpath$vpath[[1]]), as.integer(d)))
  # make a neuron from the graph made from those two longet paths
  as.neuron(igraph::induced_subgraph(ng, nodes_to_keep))
}

#' Simplify neuron to given number of branchpoints
#' @importFrom nat prune_vertices endpoints rootpoints branchpoints
simplify_neuron_multi <- function(x, n, ...) {
  if (length(nat::branchpoints(x)) < n)
    return(x)
  ng = as.ngraph(x, weights = T)
  if (!igraph::is_dag(ng)) {
    stop("I can't simplify neurons with cycles!")
  }
  # plan is to label all nodes with their longest distal diameter
  # distance table from endpoints/leaves (which are now rootpoints)
  # to branchpoints
  # rows are source i.e. branchpoints
  # cols are leaves
  leaves=setdiff(endpoints(ng), rootpoints(ng))
  bps=branchpoints(ng)
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
      start = rootpoints(ng)
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
      # nb we need the actual vertex number in the original graph
      start = as.integer(names(start))
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


# reverse graph - didn't need this in the end
  # # and the id of that target endpoint
  # # start by reversing all edges of original graph
  # el=igraph::as_edgelist(ng)
  # ngr = ngraph(
  #   el[, 2:1],
  #   directed = T,
  #   weights = igraph::edge_attr(ng, 'weight'),
  #   vertexnames = 1:igraph::gorder(ng)
  # )
  # dd=igraph::distances(ngr, to=branchpoints(ngr), v=rootpoints(ngr), mode = 'out')
