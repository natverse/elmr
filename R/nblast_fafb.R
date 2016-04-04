fetchn<-function(skids, mirror=TRUE, conn=NULL, reference=nat.flybrains::FCWB) {
  x=catmaid::read.neurons.catmaid(skids, conn=conn)
  xt=xform_brain(x, sample="FAFB12", reference = reference)
  if(mirror) xt=mirror_brain(xt, reference)
  attr(xt,'space')=as.character(reference)
  xt
}

fetchdp<-function(skids, mirror=TRUE, conn=NULL, ...) {
  xt=fetchn(skids=skids, mirror=mirror, conn=conn, ...)
  xdp=nat::dotprops(xt, resample=1, k=5)
  attr(xdp,'space')=attr(xt,'space')
  xdp
}


#' NBLAST EM tracing against flycircuit neurons
#'
#' @details Still depends on an object called \code{dps} containing registered
#'   flycircuit.tw neurons that must be obtained by contacting Greg Jefferis.
#' @param skids catmaid skeleton ids (see \code{\link[catmaid]{catmaid_skids}})
#' @param db A neuronlist object containing neurons to search
#' @param conn a \code{catmaid} connection object (see
#'   \code{\link[catmaid]{catmaid_connection}})
#' @param mirror whether to mirror the neuron (default \code{TRUE} since
#'   flycircuit neurons are on fly's left and most FAFB tracings are on fly's
#'   right.)
#' @param normalised Whether to return normalised NBLAST scores
#' @param .parallel Whether to parallelise the nblast search (see details of
#'   \code{\link[nat.nblast]{nblast}} for how to use the parallel interface
#'   provided by the \code{doMC} package.)
#' @param ... Additional parameters passed to \code{\link[nat.nblast]{nblast}}
#'
#' @return an object of class nblastfafb for which \code{plot3d}, \code{summary}
#'   and \code{hist} methods exist.
#' @export
#' @importFrom nat.nblast nblast
#' @seealso \code{\link[nat.nblast]{nblast}}
#'
#' @examples
#' \dontrun{
#' # first load dps object (see details)
#'
#' # nblast neuron 27884
#' PN27884f=nblast_fafb(27884, mirror = FALSE)
#' # summary table of results
#' summary(PN27884f)
#' # plot results, just top hit
#' plot3d(PN27884f, hits=1)
#' }
nblast_fafb <- function(skids, db=NULL, conn=NULL, mirror=TRUE, normalised=TRUE, .parallel=T, ...) {
  if(is.null(db)){
    if(exists('dps')) db=get('dps') else{
      stop("You must have the dps object containing flycircuit neurons loaded!\n",
           "See details of nblast_fafb documentation!")
    }
  }
  n=fetchn(skids=skids, mirror=mirror, conn=conn)
  if(length(n)>1) n=elmr::stitch_neurons(n)
  else n=n[[1]]
  xdp=nat::dotprops(n, resample=1, k=5)
  sc=nat.nblast::nblast(xdp, db, normalised=normalised, .parallel=.parallel, ...)
  sc=sort(sc, decreasing = T)
  scr=nblast(db[names(sc)[1:100]], nat::neuronlist(xdp), normalised=normalised,
             .parallel=.parallel, ...)
  reslist=list(sc=sc, scr=scr, n=n)
  reslist$space="FCWB"
  class(reslist)='nblastfafb'
  reslist
}

#' @export
summary.nblastfafb <- function(object, n=10, sortmu=T, ...) {
  gns=names(object$sc)[1:n]

  df=data.frame(score=object$sc[1:n])
  if(!is.null(object$scr)){
    df$muscore=(df$score+object$scr[1:n])/2
  }
  df$ntype=flycircuit::fc_neuron_type(gns)
  df$glom=flycircuit::fc_glom(gns)
  df=cbind(df, dps[gns,c("Driver", "Gender")])
  if(sortmu && !is.null(df['muscore'])){
    df$n=1:n
    df=df[order(df$muscore, decreasing = T), ]
  }
 df
}

#' @export
#' @importFrom rgl plot3d
plot3d.nblastfafb <- function(x, hits=1:10, surf=TRUE, add=TRUE, ...){
  if(!add) rgl::open3d()
  plot3d(x$n, lwd=3, col='black', WithNodes=FALSE)
  if(surf){
    surfname=paste0(x$space,'.surf')
    if(exists(surfname))
      plot3d(get(surfname), col='grey', alpha=.25)
  }
  if(x$space=='FCWB' && length(hits)){
    plot3d(names(x$sc[hits]), soma=TRUE)
  }
}

hist.nblastfafb<-function(x, br=100, col='red', ...) {
  hist(x$sc, br=br, col=col, ...)
}
