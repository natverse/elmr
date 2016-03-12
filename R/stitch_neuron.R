# Find raw vertex ids and distance for closest end points of two neurons
closest_ends<-function(a, b){
  epa=nat::endpoints(a)
  epb=nat::endpoints(b)
  axyz=a$d[epa,c("X","Y","Z")]
  bxyz=b$d[epb,c("X","Y","Z")]
  nnres=nabor::knn(axyz, bxyz, k=1)
  b_idx=which.min(nnres$nn.dists)
  a_idx=nnres$nn.idx[b_idx,1]
  return(list(a_idx=epa[a_idx], b_idx=epb[b_idx], dist=min(nnres$nn.dists)))
}

has_soma<-function(x){
  !is.null(x$tags$soma)
}

#' Stitch two neurons together at their closest endpoints
#'
#' @param a,b Neurons to join together
#' @details Note that for CATMAID neurons the neuron with the soma tag will be
#'   treated as the first (master neuron). Furthermore in this case the PointNo
#'   (aka node id) should already be unique. Otherwise it will be adjusted to
#'   ensure this.
#' @export
#' @importFrom nat as.ngraph as.neuron xyzmatrix
#' @seealso \code{\link{stitch_neurons}}
stitch_neuron<-function(a, b){

  # if there are any clashes in PointNo, augment those in second neuron
  if(any(a$d$PointNo%in%b$d$PointNo)){
    b$d$PointNo=b$d$PointNo+max(a$d$PointNo)
    b$d$Parent=b$d$Parent+max(a$d$PointNo)
  }

  ag=as.ngraph(a)
  bg=as.ngraph(b)
  abg=as.ngraph(ag+bg)
  # find closest node (or endpoint?) in each neuron and join those
  ce=closest_ends(a, b)
  a_pointno=a$d$PointNo[ce$a_idx]
  b_pointno=b$d$PointNo[ce$b_idx]
  abg=abg+igraph::edge(which(igraph::vertex_attr(abg,'label')==a_pointno),
                       which(igraph::vertex_attr(abg,'label')==b_pointno))

  as.neuron(as.ngraph(abg))
}

#' Stitch multiple fragments into single neuron using nearest endpoints
#'
#' @details Neurons will be ordered by default such the largest (by node count)
#'   neuron with a soma tag is the \code{master} neuron - i.e. the one
#'   containing the root node. Fragments are joined recursively in this sort
#'   order each time simply picking the closest fragment to the current
#'   \emph{master}. Closest is here defined by the distance between nearest
#'   endpoints.
#' @param x A neuronlist containing fragments of a single neuron
#' @param prefer_soma When TRUE (the default) the fragment tagged as the soma
#'   will be used as the master neuron.
#' @param sort When TRUE (the default) the fragments will be sorted by the
#'   number of nodes they contain before stitching.
#' @param warndist If distance is greater than this value, create a warning.
#' @return A single \code{neuron} object containing all input fragments.
#' @importFrom nat is.neuronlist
#' @importFrom igraph E set_edge_attr
#' @seealso \code{\link{stitch_neuron}}
#' @export
#' @examples
#' \dontrun{
#' pn57334=read.neurons.catmaid("name:PN 57334")
#' # there were 3 fragments when I wrote this that all contained PN 57334 in
#' # their name
#' length(pn57334)
#' summary(pn57334)
#' pn57334.whole=stitch_neurons(pn57334)
#' summary(pn57334.whole)
#' plot3d(pn57334.whole)
#' }
stitch_neurons <- function(x, prefer_soma=TRUE, sort=TRUE, warndist=1000) {
  if(!is.neuronlist(x)) stop("x must be a neuronlist object!")
  if(length(x)<=1) return(x)

  if(prefer_soma) {
    svec=sapply(x, has_soma)
  } else {
    svec=rep(0,length(x))
  }
  if(sort){
    nnodes=sapply(x, function(n) nrow(n$d))
    eps=1/(max(nnodes)+1)
    svec=(eps+svec)*nnodes
  }
  if(any(svec>0))
    x=x[order(svec, decreasing = TRUE)]

  if(length(x)==2) return(stitch_neuron(x[[1]], x[[2]]))
  #
  dists=sapply(x[-1], function(n) closest_ends(x[[1]], n)$dist)
  mindist=min(dists)
  if(isTRUE(is.finite(warndist)) && mindist>warndist){
    warning("Suspicious minimum distance between fragments ( ",mindist, ")!")
  }
  chosen=which.min(dists)+1
  x[[1]]=stitch_neuron(x[[1]], x[[chosen]])
  # no need to sort any more
  stitch_neurons(x[-chosen], prefer_soma=FALSE, sort=FALSE)
}
