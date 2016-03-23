[![Travis-CI Build Status](https://travis-ci.org/jefferis/elmr.svg?branch=master)](https://travis-ci.org/jefferis/elmr)

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
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and
[devtools](http://CRAN.R-project.org/package=devtools) to install this way.