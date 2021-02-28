#' Remove artefactual spikes in neuron tracing due to registration errors
#' @details \code{unspike} currently uses a very simple algorithm to remove big
#'   jumps: it just looks for XY jumps bigger than a specified distance and
#'   removes the first associated node. It currently cannot deal with two
#'   slightly more difficult situations: \itemize{
#'
#'   \item When there are two or more points that are far out of register.
#'
#'   \item When there a branch or end points that are out of register.
#'
#'   }
#'
#'   Handling consecutive points or end points should be a fairly
#'   straightforward addition. Branch points require a little more thought.
#'
#' @section node ids and connectors: unspike has the side effect of losing the
#'   CATMAID node ids in the output neuron. connector/tag information is
#'   retained although the node ids are translated to the new numbering system.
#'
#' @param x A neuron, or neuronlist
#' @param threshold A numeric threshold for an XY distance that would be
#'   considered suspicious.
#' @param ... arguments passed to methods
#'
#' @return A neuron or neuronlist with artefactual spikes removed. Segments that
#'   cannot currently be fixed are left untouched with a warning issued.
#' @export
#' @examples
#' \donttest{
#' da1pns=read.neurons.catmaid("annotation:^glomerulus DA1$")
#' # set threshold to 5000 nm
#' da1pns.fix=unspike(da1pns, threshold=5000)
#' plot(da1pns, col='red', WithNodes=FALSE)
#' # overplot fixed neuron leaving red spikes visible
#' plot(da1pns.fix, col='black', WithNodes=FALSE, add=TRUE)
#' }
#' @seealso \code{\link[nat]{smooth_neuron}}
unspike <- function(x, threshold, ...) UseMethod('unspike')


#' @export
#' @importFrom catmaid copy_tags_connectors
unspike.catmaidneuron <- function(x, threshold, ...) {
  r = NextMethod()
  copy_tags_connectors(new=r, old=x)
}

#' @export
#' @rdname unspike
#' @importFrom nat seglist2swc as.seglist
#' @importFrom nat as.neuron
unspike.neuron <- function(x, threshold, ...) {
  # extract original vertex array before resampling
  cols=c("X","Y","Z")
  if(!is.null(x$d$W)) cols=c(cols, 'W')
  # if(!is.null(x$d$Label)) cols=c(cols, 'Label')
  d=data.matrix(x$d[, cols, drop=FALSE])
  # if(!is.null(d$Label)) d$Label=as.integer(d$Label)
  if(any(is.na(d[,1:3])))
    stop("Unable to resample neurons with NA points")

  # fetch all segments and process each segment in turn
  sl=as.seglist(x, all = T, flatten = T)
  npoints=nrow(d)
  dl=list(d)
  modified <- FALSE
  for (i in seq_along(sl)){
    s=sl[[i]]
    # interpolate this segment
    dold=d[s, , drop=FALSE]
    dnew=unspike_segment(dold, threshold = threshold, ...)
    if(is.null(dnew)) next
    modified=TRUE
    dl[[length(dl)+1]]=dnew
    # if we've got here, we need to do something
    # add new points to the end of the swc block
    # and give them sequential point numbers
    newids=seq.int(from = npoints+1, length.out = nrow(dnew))
    npoints=npoints+nrow(dnew)
    # replace internal ids in segment so that proximal point is connected to head
    # and distal point is connected to tail
    sl[[i]]=c(s[1], newids, s[length(s)])
  }
  # If we didn't need to touch anything, just return x
  if(!modified) return(x)
  d=do.call(rbind, dl)
  d=as.data.frame(d)
  rownames(d)=NULL
  # let's deal with the label column which was dropped - assume that always the
  # same within a segment
  head_idxs=sapply(sl, "[", 1)
  seglabels=x$d$Label[head_idxs]

  # in order to avoid re-ordering the segments when as.neuron.ngraph is called
  # we can renumber the raw indices in the seglist (and therefore the vertices)
  # in a strictly ascending sequence based on the seglist
  # it is *much* more efficient to compute this on a single vector rather than
  # separately on each segment in the seglist. However this does involve some
  # gymnastics
  usl=unlist(sl)
  old_ids=unique(usl)
  # reorder vertex information to match this
  d=d[old_ids,]

  node_per_seg=sapply(sl, length)
  df=data.frame(id=usl, seg=rep(seq_along(sl), node_per_seg))
  df$newid=match(df$id, old_ids)
  sl=split(df$newid, df$seg)
  labels_by_seg=rep(seglabels, node_per_seg)
  # but there will be some duplicated ids (branch points) that we must remove
  d$Label=labels_by_seg[!duplicated(df$newid)]
  swc=seglist2swc(sl, d)
  as.neuron(swc, origin=match(x$StartPoint, old_ids))
}

#' @export
#' @rdname unspike
#' @importFrom nat nlapply
unspike.neuronlist <- function(x, threshold, ...) {
  res=nlapply(x, unspike, threshold=threshold, ...)
  regtemplate(res) <- regtemplate(x)
  res
}

#' @importFrom nat xyzmatrix
unspike_segment <- function(d, threshold, ...){
  # we must have at least 2 points to resample
  if(nrow(d) < 2) return(NULL)
  dxyz=xyzmatrix(d)

  # find cumulative length stopping at each original point on the segment
  diffs=diff(dxyz)
  diffsxy=sqrt(rowSums(diffs[,1:2, drop=FALSE]*diffs[,1:2, drop=FALSE]))
  bad_deltas=diffsxy>threshold
  if(!any(bad_deltas)) {
    return(NULL)
  }
  if(sum(bad_deltas)==1){
    # we could get rid of this if it were a terminal branch
    # how do we know?
    warning('segment with one bad point')
    return(NULL)
  }
  if(sum(bad_deltas)%%2){
    warning("I can only cope with non-contiguous runs of bad points fully contained within one segment!")
    return(NULL)
  }
  # nb +1 because these are found by diffing
  idxs=which(bad_deltas)+1
  badpoints=idxs[seq.int(from=1, by=2, length.out = length(idxs)/2)]
  if(any(idxs==1)) stop("I cannot cope with a bad point which is also a branch point")
  d[-badpoints,]
}
