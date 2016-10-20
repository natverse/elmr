#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content http_error
#' @importFrom plyr aaply
fafb_world_mapper <- function(xyz, from, to, baseurl="http://tem-services.int.janelia.org:8080/render-ws/v1/owner/flyTEM/project/FAFB00/stack", ...) {
  if(is.data.frame(xyz)) {
    xyz=as.matrix(xyz)
  } else if(!is.matrix(xyz)){
    if(length(xyz)==3) xyz=matrix(xyz, ncol=3)
    else stop("Unrecognised form of xyz input")
  }
  try_map_1 <- function(...) {
    tt=try(map_1(...))
    if(inherits(tt,'try-error')) rep(NA_real_, 3L) else tt
  }
  res=aaply(xyz, 1, try_map_1, from=from, to=to, baseurl=baseurl,
        .progress = ifelse(interactive()&&nrow(xyz)>1, 'text', 'none'), ...)
  dimnames(res)=list(NULL, c("X","Y","Z"))
  res
}

map_1 <- function(xyz, from, to, baseurl) {
  # /v12_align_tps/z/3864/world-to-local-coordinates/95335,47208.5
  # stack, z, x, y
  subpath1=sprintf("/%s_align_tps/z/%d/world-to-local-coordinates/%f,%f",
                   from, as.integer(xyz[3]), xyz[1], xyz[2])
  res1_raw=httr::GET(paste0(baseurl, subpath1))
  if(http_error(res1_raw)){
    stop("Failed to transform points with query: ", subpath1)
  }
  res1=fromJSON(content(res1_raw, as='text', encoding='UTF-8'), simplifyVector = T)
  # v13_align_tps/tile/150126171511052023.3864.0/local-to-world-coordinates/1221.9181157357234,1370.8960213087994
  # stack, tile, x, y
  subpath2=sprintf("/%s_align_tps/tile/%s/local-to-world-coordinates/%f,%f",
                   to, res1$tileId[1], res1$local[[1]][1], res1$local[[1]][2])
  res2_raw=httr::GET(paste0(baseurl, subpath2))
  if(http_error(res2_raw)){
    stop("Failed to transform points with query: ", subpath2)
  }
  res2=fromJSON(content(res2_raw, as='text', encoding='UTF-8'), simplifyVector = T)
  unlist(res2$world)
}