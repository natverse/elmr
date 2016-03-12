#' Transform data between template brains including FAFB
#'
#' @description this function overrides the nat.templatebrains function of the
#'   same name. It will be removed when nat.templatebrains/nat provide support
#'   for additional types of registration data.
#'
#' @description wraps \code{\link[nat.templatebrains]{xform_brain}}
#' @param x Neurons or other data
#' @inheritParams nat.templatebrains::xform_brain
#' @import nat.flybrains
#' @importFrom nat xform
#' @export
#' @examples
#' library(nat)
#' kcs13.fafb=xform_brain(kcs20[1:3], sample=FCWB, reference=FAFB11)
xform_brain<-function(x, sample, reference, ...){
  if(isTRUE(as.character(reference)=="FAFB11")){
    if(!identical(sample, nat.flybrains::JFRC2013))
      x=nat.templatebrains::xform_brain(x, sample=sample, reference=nat.flybrains::JFRC2013, ...)
    return(nat::xform(x, jfrc20132fafb))
  } else if(isTRUE(as.character(sample)=="FAFB11")) {
    x=nat::xform(x, jfrc20132fafb, swap=T)
    if(identical(reference, nat.flybrains::JFRC2013)) return(x)
  }
  nat.templatebrains::xform_brain(x, sample=sample, reference=reference, ...)
}

jfrc20132fafb <- function(xyz, ...) UseMethod("jfrc20132fafb")

jfrc20132fafb.data.frame <- function(xyz, ...) {
  xyzt=jfrc20132fafb(xyzmatrix(xyz), ...)
  xyzmatrix(xyz) <- xyzt
  xyz
}

jfrc20132fafb.matrix<-function(xyz, swap=FALSE,  ...){
  if(swap){
    xyzt=elmem2fafb(xyz, invert=TRUE)
    jfrc20132elmem(xyzt, swap=TRUE)
  } else {
    xyzt=jfrc20132elmem(xyz)
    elmem2fafb(xyzt)
  }
}

elmem2fafb<-function(xyz, invert=FALSE){
  # compose affine matrix based on translation then scaling
  am=nat::cmtkparams2affmat(sx=4,sy=4,sz=3.5)
  if(invert) am=solve(am)
  nat::xformpoints(am, xyz)
}

jfrc20132elmem<-function(xyz, swap=FALSE, sxyz=nat::voxdims(nat.flybrains::JFRC2013), ...){
  if(!swap) xyz=t(t(xyz)/sxyz)
  l0=data.matrix(elm_landmarks()[,c("X","Y","Z"), drop=F])
  l1=data.matrix(elm_landmarks()[,c("X1","Y1","Z1"), drop=F])
  if(swap) {
    res=Morpho::tps3d(xyz, l1, l0, ...)
    t(t(res)*sxyz)
  }
  else Morpho::tps3d(xyz, l0, l1, ...)
}

# this will be cached per R session
# we can decide later if we just want to bundle them with the package
#' @importFrom memoise memoise
elm_landmarks <- memoise::memoise(elm_landmarks_)

elm_landmarks_ <- function(u="https://raw.githubusercontent.com/saalfeldlab/elm/master/lm-em-landmarks.csv") {
  tf=tempfile(fileext = '.csv')
  on.exit(unlink(tf))
  downloader::download(u, destfile=tf, quiet=!interactive())
  read.csv(tf, col.names = c("Label", "Use", "X","Y","Z", "X1","Y1","Z1"))
}
