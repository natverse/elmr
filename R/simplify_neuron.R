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
