#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
fafb_world_mapper <- function(xyz, from, to, baseurl="http://tem-services.int.janelia.org:8080/render-ws/v1/owner/flyTEM/project/FAFB00/stack") {
  xyzd=as.data.frame(xyz)
  names(xyzd)=c("X","Y","Z")
  # need to be integer slices
  xyzd$Z=as.integer(xyzd$Z)
  n=nrow(xyzd)

  xyzt=data.matrix(xyz)
  for(i in seq_len(n)){
    # /v12_align_tps/z/3864/world-to-local-coordinates/95335,47208.5
    # stack, z, x, y
    subpath1=sprintf("/%s_align_tps/z/%d/world-to-local-coordinates/%f,%f",
                     from, as.integer(xyzd$Z[i]), xyzd$X[i], xyzd$Y[i])
    res1_raw=httr::GET(paste0(baseurl, subpath1))
    res1=fromJSON(content(res1_raw, as='text', encoding='UTF-8'), simplifyVector = T)
    # v13_align_tps/tile/150126171511052023.3864.0/local-to-world-coordinates/1221.9181157357234,1370.8960213087994
    # stack, tile, x, y
    subpath2=sprintf("/%s_align_tps/tile/%s/local-to-world-coordinates/%f,%f",
                     to, res1$tileId[1], res1$local[[1]][1], res1$local[[1]][2])
    res2_raw=httr::GET(paste0(baseurl, subpath2))
    res2=fromJSON(content(res2_raw, as='text', encoding='UTF-8'), simplifyVector = T)
    xyzt[i,]=unlist(res2$world)
  }
  xyzt
}
