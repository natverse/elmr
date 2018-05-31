[![Travis-CI Build Status](https://travis-ci.org/jefferis/elmr.svg?branch=master)](https://travis-ci.org/jefferis/elmr)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](http://jefferis.github.io/elmr/reference/)
[![DOI](https://zenodo.org/badge/52780580.svg)](https://zenodo.org/badge/latestdoi/52780580)

# elmr

Provides tools to move between adult brain EM and light level data, emphasising
the interaction between the CATMAID web application and the R [Neuroanatomy
Toolbox package](https://github.com/jefferis/nat). See also https://github.com/saalfeldlab/elm, from which this 
package borrows both a name and uses data.

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
conveniently bring the devlopment versions up to date by doing:

```r
devtools::update_packages("elmr", dependencies = TRUE)
```
