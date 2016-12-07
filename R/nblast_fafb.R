#' Fetch neurons from FAFB catmaid server, transforming them as appropriate
#'
#' @description \code{fetchn_fafb} is a thin wrapper around the
#'   \code{catmaid::\link[catmaid]{read.neurons.catmaid}} function with the
#'   additional convenience of auromating conversion to a reference brain.
#' @export
#' @inheritParams nblast_fafb
#' @param reference The reference brain to which the neurons will be
#'   transformed, either a \code{\link[nat.templatebrains]{templatebrain}}
#'   object such as \code{\link[nat.flybrains]{FCWB}} or a character vector
#'   naming such an object.
#' @examples
#' \dontrun{
#' # fetch all neurons with an annotation exactly matching "KC" and convert
#' # to JFRC2013 space
#' kcs=fetchn_fafb("annotation:^KC$", reference=JFRC2013)
#' }
fetchn_fafb<-function(skids, mirror=TRUE, conn=NULL, reference=nat.flybrains::FCWB) {
  x=catmaid::read.neurons.catmaid(skids, conn=conn)
  xt=xform_brain(x, sample="FAFB13", reference = reference)
  if(mirror) xt=mirror_brain(xt, reference)
  xt
}

#' @export
#' @description \code{fetchdp_fafb} extends \code{fetchn_fafb} by additionally converting
#'   neurons to the \code{\link[nat]{dotprops}} representation suitable for
#'   \code{\link[nat.nblast]{nblast}}.
#' @rdname fetchn_fafb
fetchdp_fafb<-function(skids, mirror=TRUE, conn=NULL, ...) {
  xt=fetchn_fafb(skids=skids, mirror=mirror, conn=conn, ...)
  xdp=nat::dotprops(xt, resample=1, k=5)
  regtemplate(xdp)=regtemplate(xt)
  xdp
}


#' NBLAST EM tracing against flycircuit (or other databases of) neurons
#'
#' @details Still depends on having a \code{\link[nat]{neuronlist}} containing
#'   registered neurons (usually from flycircuit.tw). The example code downloads
#'   a set of projection neurons. The full data must be requested from Greg
#'   Jefferis.
#' @param skids catmaid skeleton ids (see \code{\link[catmaid]{catmaid_skids}})
#' @param db A neuronlist object containing neurons to search. Defaults to the
#'   value of \code{options('nat.default.neuronlist')}.
#' @param conn a \code{catmaid} connection object (see
#'   \code{\link[catmaid]{catmaid_connection}})
#' @param mirror whether to mirror the neuron (default \code{TRUE} since
#'   flycircuit neurons are on fly's left and most FAFB tracings are on fly's
#'   right.)
#' @param normalised Whether to return normalised NBLAST scores
#' @param reverse Treat the FAFB skeleton as NBLAST target rather than query
#'   (sensible if \code{db} contains partial skeletons/tracts; default
#'   \code{FALSE}).
#' @param .parallel Whether to parallelise the nblast search (see details of
#'   \code{\link[nat.nblast]{nblast}} for how to use the parallel interface
#'   provided by the \code{doMC} package.)
#' @param ... Additional parameters passed to \code{\link[nat.nblast]{nblast}}
#' @inheritParams fetchn_fafb
#'
#' @return an object of class nblastfafb for which \code{plot3d}, \code{summary}
#'   and \code{hist} methods exist.
#' @export
#' @importFrom nat.nblast nblast
#' @seealso \code{\link[nat.nblast]{nblast}}
#'
#' @examples
#' \dontrun{
#' # first load neuronlist object containing registered neurons (see details)
#' allpndps=flycircuit::load_si_data('allpndps.rds')
#' # ... and set that as the default for queries and plotting
#' options(nat.default.neuronlist='allpndps')
#'
#' # then make sure you are loged in to catmaid server
#' # catmaid::catmaid_login(<your connection args>)
#'
#' # nblast neuron 27884
#' PN27884f=nblast_fafb(27884, mirror = FALSE)
#' # summary table of results
#' summary(PN27884f)
#' # plot results, just top hit
#' plot3d(PN27884f, hits=1)
#' }
nblast_fafb <- function(skids, db=NULL, conn=NULL, mirror=TRUE, normalised=TRUE,
                        reverse=FALSE, reference=nat.flybrains::FCWB,
                        .parallel=TRUE, ...) {
  db=getdb(db)
  if(.parallel){
    if(!isNamespaceLoaded('foreach') || foreach::getDoParWorkers()==1){
      warning("see ?nblast and doMC::registerDoMC for details of setting up parallel nblast")
    }
  }
  n=fetchn_fafb(skids=skids, mirror=mirror, conn=conn, reference = reference)
  # store the templatebrain for later
  tb=regtemplate(n)
  if(length(n)>1) n=elmr::stitch_neurons(n)
  else n=n[[1]]
  regtemplate(n)=tb
  xdp=nat::dotprops(n, resample=1, k=5)
  if(reverse) {
    sc=nat.nblast::nblast(db, nat::neuronlist(xdp), normalised=normalised,
                          .parallel=.parallel, ...)
    sc=sort(sc, decreasing = T)
    scr=nblast(nat::neuronlist(xdp), db[names(sc)[1:100]], normalised=normalised,
               .parallel=.parallel, ...)
  } else {
    sc=nat.nblast::nblast(xdp, db, normalised=normalised, .parallel=.parallel, ...)
    sc=sort(sc, decreasing = T)
    scr=nblast(db[names(sc)[1:100]], nat::neuronlist(xdp), normalised=normalised,
               .parallel=.parallel, ...)
  }
  reslist=list(sc=sc, scr=scr, n=n)
  class(reslist)='nblastfafb'
  reslist
}

getdb <- function(db){
  if(is.null(db)) {
    defaultdb <- getOption('nat.default.neuronlist')
    if(!is.null(defaultdb) && exists(defaultdb)) {
      db=get(defaultdb)
    } else {
      stop("You must have a neuronlist containing registered neurons loaded!\n",
           "See details of nblast_fafb documentation!")
    }
  }
  db
}

#' @export
summary.nblastfafb <- function(object, n=10, sortmu=T, db=NULL, ...) {
  gns=names(object$sc)[1:n]

  df=data.frame(score=object$sc[1:n])
  if(!is.null(object$scr)){
    df$muscore=(df$score+object$scr[1:n])/2
  }
  df$ntype=flycircuit::fc_neuron_type(gns)
  df$glom=flycircuit::fc_glom(gns)

  db=getdb(db)
  dbdf=as.data.frame(db)
  available_cols=intersect(c("Driver", "Gender"), colnames(dbdf))
  df <- if(length(available_cols))
    cbind(df, dbdf[gns, available_cols])
  else
    cbind(df, dbdf[gns, ])
  if(sortmu && !is.null(df['muscore'])){
    df$n=1:n
    df=df[order(df$muscore, decreasing = T), ]
  }
 df
}

#' @export
#' @importFrom rgl plot3d
plot3d.nblastfafb <- function(x, hits=1:10, surf=TRUE, add=TRUE, db=NULL, ...){
  if(!add) rgl::open3d()
  plot3d(x$n, lwd=3, col='black', WithNodes=FALSE, ...)
  if(surf){
    rt=regtemplate(x$n)
    if(!is.null(rt))
      plot3d(rt, col='grey', alpha=.25)
  }
  # Now plot the hits if requested
  if(length(hits)) {
    db=getdb(db)
    plot3d(names(x$sc[hits]), db=db, soma=TRUE)
  }
}

#' @importFrom graphics hist
#' @export
hist.nblastfafb<-function(x, br=100, col='red', ...) {
  hist(x$sc, br=br, col=col, ...)
}
