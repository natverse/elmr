fetchn<-function(skids, mirror=TRUE, conn=NULL, ref=nat.flybrains::FCWB) {
  x=read.neurons.catmaid(skids, conn=conn)
  xt=xform_brain(x, sample="FAFB12", ref=ref)
  if(mirror) xt=mirror_brain(xt, ref)
  attr(xt,'space')=as.character(ref)
  xt
}

fetchdp<-function(skids, mirror=TRUE, conn=NULL, ...) {
  xt=fetchn(skids=skids, mirror=mirror, conn=conn, ...)
  xdp=dotprops(xt, resample=1, k=5)
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
#'   \code{\link[nat.nblast]{nblast}})
#' @param ... Additional parameters passed to \code{\link[nat.nblast]{nblast}}
#'
#' @return an object of class nblastfafb for which \code{plot3d}, \code{summary}
#'   and \code{hist} methods exist.
#' @export
#' @seealso \code{\link[nat.nblast]{nblast}}
#'
#' @examples
#' \dontrun{
#' # load dps object (see details)
#'
#' # load https://github.com/jefferislab/nat.flybrains
#' library(nat.flybrains)
#'
#' # nblast neuron 27884
#' PN27884f=nblast_fafb(27884, mirror = FALSE)
#' # summary table of results
#' summary(PN27884f)
#' # plot results, just top hit
#' plot3d(PN27884f, hits=1)
#' }
nblast_fafb <- function(skids, db=dps, conn=NULL, mirror=TRUE, normalised=TRUE, .parallel=T, ...) {
  n=fetchn(skids=skids, mirror=mirror, conn=conn)
  if(length(n)>1) n=elmr::stitch_neurons(n)
  else n=n[[1]]
  xdp=dotprops(n, resample=1, k=5)
  sc=nblast(xdp, db, normalised=normalised, .parallel=.parallel, ...)
  sc=sort(sc, decreasing = T)
  scr=nblast(db[names(sc)[1:100]], neuronlist(xdp), normalised=normalised,
             .parallel=.parallel, ...)
  reslist=list(sc=sc, scr=scr, n=n)
  reslist$space="FCWB"
  class(reslist)='nblastfafb'
  reslist
}

#' @export
summary.nblastfafb <- function(object, n=10, sortmu=T, kable=FALSE, ...) {
  gns=names(object$sc)[1:n]

  df=data.frame(score=object$sc[1:n])
  if(!is.null(object$scr)){
    df$muscore=(df$score+object$scr[1:n])/2
  }
  df$ntype=fc_neuron_type(gns)
  df$glom=fc_glom(gns)
  df=cbind(df, dps[gns,c("Driver", "Gender")])
  if(sortmu && !is.null(df['muscore'])){
    df$n=1:n
    df=df[order(df$muscore, decreasing = T), ]
  }
  if(kable) {
    knitr::kable(df, digits = 3, ...)
  } else df
}

#' @export
plot3d.nblastfafb <- function(x, hits=1:10, surf=TRUE, add=TRUE, ...){
  if(!add) open3d()
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
