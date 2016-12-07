# download elm landmarks (or use local version)
elm_landmarks_ <- function(u="https://raw.githubusercontent.com/saalfeldlab/elm/master/lm-em-landmarks.csv") {
  hu=try(HEAD(u, timeout(1)), silent = TRUE)
  internet.ok=!inherits(hu, 'try-error')
  if(internet.ok){
    tf=tempfile(fileext = '.csv')
    on.exit(unlink(tf))
    downloader::download(u, destfile=tf, quiet=!interactive())
  } else {
    tf=system.file("elm/lm-em-landmarks.csv", package = 'elmr')
  }
  x=read.csv(tf, col.names = c("Label", "Use", "X","Y","Z", "X1","Y1","Z1"), header = FALSE)
  x$Use=as.logical(x$Use)
  x
}

elm.landmarks <- elm_landmarks_()
elm.landmarks.12=elm.landmarks
# NB only transform landmarks in use.
elm.landmarks[elm.landmarks$Use,6:8]=xform_brain(elm.landmarks[elm.landmarks$Use,6:8], sample =FAFB12, reference = FAFB13, method='single')
# alternatively try to transform all
# elm.landmarks[,6:8]=xform_brain(elm.landmarks[,6:8], sample =FAFB12, reference = FAFB13, method='single')

elm.landmarks=subset(elm.landmarks, Use)
devtools::use_data(elm.landmarks, overwrite = T)
devtools::use_data(elm.landmarks.12, overwrite = T)
