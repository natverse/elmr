<!-- badges: start -->
[![Travis-CI Build Status](https://travis-ci.org/jefferis/elmr.svg?branch=master)](https://travis-ci.org/jefferis/elmr)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](http://jefferis.github.io/elmr/reference/)
[![DOI](https://zenodo.org/badge/52780580.svg)](https://zenodo.org/badge/latestdoi/52780580)
[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.github.io)
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](https://jefferis.github.io/elmr/reference/)
<!-- badges: end -->

# elmr

Provides tools to move between adult brain EM and light level data, emphasising
the interaction between the CATMAID web application and the R [Neuroanatomy
Toolbox package](https://github.com/jefferis/nat). See also https://github.com/saalfeldlab/elm, from which this 
package borrows a name and which was used to define the landmark data sets that
provide a bridge between the FAFB EM brain and light level template.

## Quick Start

For the impatient ...

```r
# install
if (!require("devtools")) install.packages("devtools") 
devtools::install_github("jefferis/elmr")

# use
library(elmr)

# run examples 
example("open_fafb")

# get overview help for package 
?elmr 

# help for functions/data 
?FAFB
?fetchn_fafb
?nblast_fafb
?open_fafb
?xform_brain
?stitch_neurons
?tpsreg
```

## Installation

Currently there isn't a released version on [CRAN](http://cran.r-project.org/).

### Development version
You can use the **devtools** package to install the development version:

```r 
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jefferis/elmr")

# to transform skeletons to template brains other than JFRC2013, you also need
# a CMTK installation (see https://github.com/jefferis/nat/#external-dependencies)
# and to download some additional bridging registrations.
library(nat.flybrains)
download_jefferislab_registrations()
```
Note that this should also install the latest version of the necessary 
dependencies. To carry out nblast comparisons with [flycircuit.tw](http://flycircuit.tw) neurons, you may
need to install optional dependencies by installing as follows:

```
devtools::install_github("jefferis/elmr", dependencies=TRUE)
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and
[devtools](http://CRAN.R-project.org/package=devtools) to install this way.

### Updating

**elmr** and its dependencies are still under fairly heavy development. You can 
conveniently bring the development versions up to date by doing:

```r
devtools::update_packages("elmr", dependencies = TRUE)
```

## Acknowledgements

* elmr is part of a suite of R packages based on the [NeuroAnatomy
Toolbox](http://jefferislab.github.io/)

* The approach to transforming data between template brains is described in 
  
  Combining genome-scale Drosophila 3D neuroanatomical data by bridging template brains
  James D. Manton, Aaron D. Ostrovsky, Lea Goetz, Marta Costa, Torsten Rohlfing,
  Gregory S. X. E. Jefferis. bioRxiv 006353; doi:
  [10.1101/006353](https://doi.org/10.1101/006353)
  
* The [elm](https://github.com/saalfeldlab/elm) tool originally used to define
landmarks is in turn based on [BigWarp](http://fiji.sc/BigWarp):

  JA Bogovic, P Hanslovsky, A Wong, S Saalfeld, "Robust registration of calcium
  images by learned contrast synthesis", In *Biomedical Imaging (ISBI)*, 2016
  IEEE 13th International Symposium on, 1123-1126,  DOI:
  [10.1109/ISBI.2016.7493463](https://doi.org/10.1109/ISBI.2016.7493463).

* Finally, the FAFB EM dataset is described in:
  
  A Complete Electron Microscopy Volume Of The Brain Of Adult Drosophila
  melanogaster Zhihao Zheng, J. Scott Lauritzen, Eric Perlman, Camenzind G.
  Robinson, Matthew Nichols, Daniel Milkie, Omar Torrens, John Price, Corey B.
  Fisher, Nadiya Sharifi, Steven A. Calle-Schuler, Lucia Kmecova, Iqbal J. Ali,
  Bill Karsh, Eric T. Trautman, John Bogovic, Philipp Hanslovsky, Gregory S. X. E.
  Jefferis, Michael Kazhdan, Khaled Khairy, Stephan Saalfeld, Richard D. Fetter,
  Davi D. Bock bioRxiv 140905; doi: [10.1101/140905](https://doi.org/10.1101/140905)
