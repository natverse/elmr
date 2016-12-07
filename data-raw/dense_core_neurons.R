# a pair of neurons that Greg traced that will be used as an example
library(catmaid)
dense_core_neurons <- read.neurons.catmaid(c(713968, 822008))
regtemplate(dense_core_neurons) <- FAFB13
devtools::use_data(dense_core_neurons, overwrite = T)
