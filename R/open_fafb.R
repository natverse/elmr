#' Open FAFB CATMAID in browser at a given XYZ location
#'
#' @details Note that if object \code{x} contains exactly one point then CATMAID
#'   will be opened immediately at that location, whereas if there is more than
#'   1 point, the function will stop and wait for the user to make an
#'   interactive selection in a \code{rgl} window.
#' @param x A numeric vector or any object compatible with
#'   \code{\link[nat]{xyzmatrix}} (see details)
#' @param s A selection function of the type returned by
#'   \code{\link[rgl]{select3d}}
#' @param mirror Whether to mirror the point to the opposite side of the brain
#' @param sample The template brain space associated with the coordinates in
#'   \code{x}
#' @param open Whether to open the url in the browser or simply return it.
#'   Defaults to \code{TRUE} when R is running in interactive mode.
#' @param zoom The CATMAID zoom factor (defaults to 1)
#' @details Note that
#' @export
#' @seealso xform_brain
#' @examples
#' open_fafb(c(316, 143, 26), sample=JFRC2013, open=FALSE)
#' library(nat)
#' \dontrun{
#' open3d()
#' plot3d(kcs20)
#' # waits for used to draw a selection rectangle
#' open_fafb(kcs20, sample=FCWB)
#' # same but mirrors selected points to opposite hemisphere
#' open_fafb(kcs20, sample=FCWB, mirror=TRUE)
#' }
open_fafb<-function(x, s=rgl::select3d(), mirror=FALSE, sample=elmr::FAFB11,
                    zoom=1, open=interactive()) {
  if(is.vector(x, mode='numeric') && length(x)==3 ){
    xyz=matrix(x, ncol=3)
  } else {
    xyz=xyzmatrix(x)
    if(nrow(xyz)>1){
      # calculate centroid of points inside selection
      xyz=colMeans(xyz[s(xyz),, drop=F])
      xyz=matrix(xyz, ncol=3)
    }
  }
  if(mirror)
    xyz=mirror_brain(xyz, sample)
  if(as.character(sample)!="FAFB11")
    xyz=xform_brain(xyz, sample = sample, reference = elmr::FAFB11)

  xyzi=as.integer(xyz)
  url=sprintf("https://neuropil.janelia.org/tracing/fafb/v12/?pid=1&zp=%d&yp=%d&xp=%d&tool=tracingtool&sid0=7&s0=%f",
              xyzi[3], xyzi[2], xyzi[1], zoom)
  if(open){
    browseURL(url)
    invisible(url)
  } else {
    url
  }
}
