## ---- message=FALSE------------------------------------------------------
full.bridging=FALSE
library(nat)
if(!nzchar(cmtk.bindir())){
  cat("CMTK not available! Some examples will not run.")
} else {
  djrok=try(nat.flybrains::download_jefferislab_registrations())
  if(inherits(djrok, "try-error")) 
    cat("Unable to download bridging registrations! Some examples will not run.")
  else full.bridging=TRUE
}
# set up for 3d plots based on rgl package
rgl::setupKnitr()
# frontal view
view3d(userMatrix=rgl::rotationMatrix(angle = pi, 1,0,0), zoom=0.6)

## ---- message=FALSE------------------------------------------------------
library(elmr)
vgloms.jfrc2013=data.frame(X=c(316,229),
  Y=c(143, 139),
  Z=c(26,22),
  row.names=c("V_L", "V_R"))
# Convert to FAFB12 coordinates
xform_brain(vgloms.jfrc2013, sample = JFRC2013, reference = FAFB12)

## ---- eval=full.bridging-------------------------------------------------
# Conversion of neurons from the IS2 light level template brain
# NB this conversion depends on a full install of nat.flybrains and CMTK
# ensure that we have all the relevant bridging registrations downloaded
Cell07PNs13.fafb=xform_brain(Cell07PNs[1:3], sample=IS2, reference=FAFB12)
plot(Cell07PNs13.fafb)

## ---- rgl=TRUE-----------------------------------------------------------
FAFB12.surf=xform_brain(JFRC2013.surf, sample = JFRC2013, reference = FAFB12)
# NB plot3d.templaterain gives nice defaults for brain surface plot but assumes
# that the surface object is called XXXX.surf
plot3d(FAFB12)
plot3d(dense_core_neurons, lwd=2, soma=3000)

## ---- eval=full.bridging, rgl=TRUE---------------------------------------
FAFB12NP.surf=xform_brain(JFRC2NP.surf, sample = JFRC2, reference = FAFB12)
plot3d(FAFB12NP.surf, "LH", alpha=0.4)
plot3d(dense_core_neurons, lwd=2, soma=3000)

## ---- eval=full.bridging-------------------------------------------------
# set the template space for the input coordinates
regtemplate(vgloms.jfrc2013)=JFRC2013
# now we do not need to specify it in the xform_brain call
vgloms.fafb=xform_brain(vgloms.jfrc2013, reference = FAFB12)
# check ouput space
regtemplate(vgloms.fafb)

