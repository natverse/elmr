# Private function to make a template for a given CATMAID stack
#' @importFrom catmaid catmaid_fetch
make_template <- function(stackinfo=catmaid::catmaid_fetch('1/stack/7/info'), ...){
  i=nat::im3d(dims=unlist(stackinfo$dimension), voxdims=unlist(stackinfo$resolution))
  v=stringr::str_match(stackinfo$stitle, "V(\\d+)")[2]
  # TODO this is a bit of a hack due to
  # https://github.com/jefferislab/nat.templatebrains/issues/25
  attr(i,'file')=paste0("FAFB", v)
  nat.templatebrains::as.templatebrain(i)
}
