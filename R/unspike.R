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
  allbadpoints=integer()
  modified <- FALSE
  for (i in seq_along(sl)){
    s=sl[[i]]
    # interpolate this segment
    dold=d[s, , drop=FALSE]
    badpoints=unspike_segment(dold, threshold = threshold, ...)
    if(is.null(badpoints))
      next
    modified=TRUE
    # NB unspike_segment returns the index into the point array for one segment
    # not the point array for the whole neuron
    sl[[i]]=s[-badpoints]
    allbadpoints=c(allbadpoints, s[badpoints])
  }
  # If we didn't need to touch anything, just return x
  if(!modified) return(x)

  if(x$StartPoint %in% allbadpoints) {
    warning("Original root id was removed by unspike!")
    startid=NULL
  } else {
    startid=x$d$PointNo[x$StartPoint]
  }
  swc=seglist2swc(sl, x$d)
  # now the problem is that our swc isn't quite right as the bad rows are still
  # in there. They should be just isolated points without parents as they were
  # not mentioned in the seglist
  if(!all(swc$Parent[allbadpoints] == -1L))
    stop("Error identifying bad points in unspike")
  swc=swc[-allbadpoints,,drop=FALSE]
  as.neuron(swc, origin=startid, NeuronName=x$NeuronName, skid=x$skid)
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
  badpoints
}
