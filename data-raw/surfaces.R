library(nat.flybrains)
FAFB13.surf=xform_brain(JFRC2.surf, sample = JFRC2, reference = FAFB13)
FAFB13NP.surf=xform_brain(JFRC2NP.surf, sample = JFRC2, reference = FAFB13)
library(devtools)
use_data(FAFB13.surf, overwrite = TRUE)
use_data(FAFB13NP.surf, overwrite = TRUE)

FAFB14.surf=xform_brain(JFRC2.surf, sample = JFRC2, reference = FAFB14)
FAFB14NP.surf=xform_brain(JFRC2NP.surf, sample = JFRC2, reference = FAFB14)
use_data(FAFB14.surf, overwrite = TRUE)
use_data(FAFB14NP.surf, overwrite = TRUE)

FAFB.surf=FAFB14.surf
FAFBNP.surf=FAFB14NP.surf
use_data(FAFB.surf, overwrite = TRUE)
use_data(FAFBNP.surf, overwrite = TRUE)
