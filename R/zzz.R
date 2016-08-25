.onLoad <- function (libname, pkgname) {

  # Define and register a compound registration that maps FAFB12 to JFRC2013
  # first landmarks-based thin plate splines
  elmlm=elmr::elm.landmarks[elmr::elm.landmarks[,"Use"],]
  # JFRC2013 pixel space
  l0=data.matrix(elmlm[,c("X","Y","Z"), drop=F])
  # FAFB nm space
  l1=data.matrix(elmlm[,c("X1","Y1","Z1"), drop=F])

  # then the scale component
  m <- rgl::identityMatrix()
  diag(m) <- c(nat::voxdims(nat.flybrains::JFRC2013),1)

  JFRC2013_FAFB12 <- nat::reglist(tpsreg(l1, l0), m)
  nat.templatebrains::add_reglist(JFRC2013_FAFB12,
                                  reference = nat.flybrains::JFRC2013,
                                  sample = elmr::FAFB12)
}