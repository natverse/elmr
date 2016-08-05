unspike <- function(x, ...){

}

unspike.neuron <- function(x, ...) {
  x
  if(any(is.na(d[,1:3])))
    stop("Unable to resample neurons with NA points")
  # find displacements in XY

}

unspike.neuronlist <- function(x, ...) {

}

unspike_segment <- function(d, ...){
  # we must have at least 2 points to resample
  if(nrow(d) < 2) return(NULL)
  dxyz=xyzmatrix(d)
  if(!is.data.frame(d)) d=as.data.frame(d)

  # find cumulative length stopping at each original point on the segment
  diffs=diff(dxyz)
  diffsxy=sqrt(rowSums(diffs[,1:2]*diffs[,1:2]))
  diffsz=abs(diffs[,3])
  cumlengthxy=c(0,cumsum())
  cumlengthz=c(0, cumsum(diffsz))
  # find 3D position of new internal points
  # using linear approximation on existing segments
  # make an emty list for results
  dnew=list()
  for(n in names(d)) {
    dnew[[n]] <- if(!all(is.finite(d[[n]]))) {
      rep(NA, nInternalPoints)
    } else {
      approx(cumlength, d[[n]], internalPoints,
             method = ifelse(is.double(d[[n]]), "linear", "constant"))$y
    }
  }
  as.data.frame(dnew)
}