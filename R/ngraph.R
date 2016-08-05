#' Return indices of points in a neuron distal to a given node
#'
#' @param x A neuron
#' @param node.idx,node.pointno The id of node beyond which distal points will
#'   be selected. \code{node.idx} defines the integer index (counting from 1)
#'   into the neuron's point array.
#' @param root The root node of the neuron for the purpose of selection.
#' @param ... Additional
#' @export
#' @importFrom nat rootpoints
distal_to <- function(x, node.idx=NULL, node.pointno=NULL, root=rootpoints(x),
                      ...) {
  if(is.null(node.idx)) {
    if(is.null(node.pointno))
      stop("At least one of node.idx or node.pointno must be supplied")
    node.idx=pmatch(node.pointno, x$d$PointNo)
    if(is.na(node.idx))
      stop("Invalid node.pointno. It should match an entry in x$d$PointNo!")
  }

  if(length(root)>1) stop("A single unique root point must be supplied")

  g=as.ngraph(x)
  gdfs=igraph::dfs(g, root=node.idx, unreachable = FALSE)
  as.integer(gdfs$order)[!is.na(gdfs$order)]
}
