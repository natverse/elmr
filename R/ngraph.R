#' Return indices of points in a neuron distal to a given node
#'
#' @param x A neuron
#' @param node.idx,node.pointno The id(s) of node(s) beyond which distal points
#'   will be selected. \code{node.idx} defines the integer index (counting from
#'   1) into the neuron's point array whereas \code{node.pointno} matches the
#'   PointNo column which will be the catmaid id for a node.
#' @param root.idx,root.pointno The root node of the neuron for the purpose of
#'   selection. You will rarely need to change this from the default value. See
#'   \code{node} argument for the difference between \code{root.idx} and
#'   \code{root.pointno} forms.
#' @return Integer 1-based indices into the point array of points that are
#'   distal to the specified node(s) when traversing the neuron from the root to
#'   that node. Will be a vector one node is specified, a list otherwise
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
distal_to <- function(x, node.idx=NULL, node.pointno=NULL, root.idx=NULL,
                      root.pointno=NULL) {
  if(is.null(node.idx)) {
    if(is.null(node.pointno))
      stop("At least one of node.idx or node.pointno must be supplied")
    node.idx=match(node.pointno, x$d$PointNo)
    if(any(is.na(node.idx)))
      stop("One or more invalid node.pointno. Should match entries in x$d$PointNo!")
  }
  if(is.null(root.idx) && is.null(root.pointno)) {
    g=as.ngraph(x)
  } else {
    if(!is.null(root.pointno))
      root.idx=match(root.pointno, x$d$PointNo)
    if(length(root.idx)>1)
      stop("A single unique root point must be supplied")
    # we need to re-root the graph onto the supplied root
    gorig=as.ngraph(x)
    g=nat:::as.directed.usingroot(gorig, root = root.idx)
  }
  mydfs <- function(x, g) {
    gdfs=igraph::dfs(g, root = x, unreachable = FALSE)
    as.integer(gdfs$order)[!is.na(gdfs$order)]
  }
  l=sapply(node.idx, mydfs, g, simplify = FALSE)
  if(length(node.idx)==1) l[[1]] else l
}
