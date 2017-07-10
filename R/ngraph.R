#' Return indices of points in a neuron distal to a given node
#'
#' @param x A neuron
#' @param node.idx,node.pointno The id of node beyond which distal points will
#'   be selected. \code{node.idx} defines the integer index (counting from 1)
#'   into the neuron's point array.
#' @param root The root node of the neuron for the purpose of selection. You
#'   will rarely need to change this from the default value
#'   (\code{rootpoints(x)}).
#' @param ... Additional
#' @export
#' @importFrom nat rootpoints
#' @seealso \code{\link[nat]{subset.neuron}}, \code{\link[nat]{prune}}
#' @examples
#' \donttest{
#' ## Fetch a finished DL1 projection neuron
#' finished_pns=catmaid_get_neuronnames('annotation:^LH_DONE')
#' # should only be one neuron but pick first just in case
#' dl1skid=names(grep('DL1', finished_pns, value = TRUE))[1]
#' dl1=read.neuron.catmaid(dl1skid)
#'
#' ## subset to part of neuron distal to a tag "SCHLEGEL_LH"
#' # nb distal_to can accept either the PointNo vertex id or raw index as a
#' # pivot point
#' dl1.lh=subset(dl1, distal_to(dl1,node.pointno = dl1$tags$SCHLEGEL_LH))
#' plot(dl1,col='blue', WithNodes = FALSE)
#' plot(dl1.lh, col='red', WithNodes = FALSE, add=TRUE)
#' }
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
