#' Read neurons from CATMAID with extra meta-data
#'
#' @description Use \code{read.neurons.catmaid} to acquire neurons from a CATMAID instance.
#' In addition, include as meta-data certain annotations that these neurons have. To select which
#' annotations appear as entries in the neurons' meta-data, select 'meta-annotations' to appear as the
#' data field. By default, meta-annotations for developmental lineage identities are used,
#' to get the lineage / hemilineage to which a FAFB Drosophila neuron belongs.
#'
#' @inheritParams catmaid::read.neurons.catmaid
#' @param meta a vector of meta-annotations to query.
#' The annotations labelled by these meta-annotations will appear as
#' entries in the \code{data.frame} attached to the returned \code{neuronlist}.
#' @param sub what to remove from pulled annotations, to clean them up a little.
#' Can be set to "" if you do not wish to remove anything.
#' @param batch numeric, if \code{FALSE} or \code{0} neurons are read from CATMAID without being batched.
#' If a number, neurons are read in batches of size \code{batch}. If \code{batch} is set to \code{TRUE}, a batch size of
#' 1000 is used. This can help overcome server time out errors.
#' @examples
#' \donttest{
#' \dontrun{
#' # Get some information on a published neuron
#' ## 11519370 was pubished first in Dolan et al. 2019
#' meta = fafb_get_meta("11519370")
#'
#' # We can get all of the lineage annotations made in the FAFB dataset
#' ## These come from two different naming schemes, one by Volker Hartenstein
#' ## and another by the groups' of K. Ito and T. Lee.
#' fafb.neurons.lineage =
#' read.neurons.fafb("annotation:Lineage_annotated")
#' }}
#' @export
#' @rdname read.neurons.fafb
read.neurons.fafb <- function(skids,
                              meta = c(
                                "neuron name",
                                "whimsical name",
                                "cell type",
                                "cell body fiber",
                                "ItoLee_Lineage",
                                "ItoLee_Hemilineage",
                                "Hartenstein_Lineage",
                                "Hartenstein_Hemilineage",
                                "transmitter",
                                "tracing status",
                                "hemibrain match",
                                "synonym",
                                "paper",
                                "citation"),
                              sub = ".*:",
                              OmitFailures = TRUE,
                              batch = FALSE,
                              ...){
  skids = catmaid::catmaid_skids(skids, ...)
  if(batch){
    if(batch==TRUE){ batch = 1000}
    batches = split(1:length(skids), ceiling(seq_along(1:length(skids))/batch))
    n = nat::neuronlist()
    for(i in batches){
      message("Reading neurons ", min(i)," to ",  max(i))
      b = read.neurons.fafb(skids = skids[min(i):max(i)], meta = meta, sub = sub, OmitFailures = OmitFailures, batch = FALSE)
      n = nat::union(n, b)
    }
  }else{
    maddf <- fafb_get_meta(skids = skids, meta, sub = sub, OmitFailures = OmitFailures)
    n <-  catmaid::read.neurons.catmaid(skids, OmitFailures = OmitFailures, ...)
    n[,] = maddf
  }
  n
}

#' @export
#' @rdname read.neurons.fafb
fafb_get_meta <- function(skids,
                          meta = c(
                            "neuron name",
                            "whimsical name",
                            "cell type",
                            "cell body fiber",
                            "ItoLee_Lineage",
                            "ItoLee_Hemilineage",
                            "Hartenstein_Lineage",
                            "Hartenstein_Hemilineage",
                            "transmitter",
                            "tracing status",
                            "hemibrain match",
                            "synonym",
                            "paper",
                            "citation"),
                          sub = ".*:",
                          OmitFailures = TRUE,
                          ...){
  skids = catmaid::catmaid_skids(skids, OmitFailures=OmitFailures,...)
  maddf <- data.frame()
  for (ma in meta) {
    mad <- tryCatch(catmaid::catmaid_query_meta_annotations(ma, OmitFailures = OmitFailures, ...),
                    error = function(e) NULL)
    if(is.null(mad)){
      next
    }
    mad$meta <- ma
    mad$field <- gsub(ma, "", mad$name)
    mad$field <- gsub(sub, "", mad$field)
    maddf <- rbind(maddf, mad)
  }
  as <- catmaid::catmaid_get_annotations_for_skeletons(skids, OmitFailures = OmitFailures, ...)
  as <- as[as$id %in% maddf$id, ]
  m <- merge(maddf, as[, c("skid", "id")])
  n = data.frame(skid = skids)
  rownames(n) <- skids
  n[, c(meta, "unique.assignment")] <- NA
  for (skid in n[, "skid"]) {
    mm <- m[m$skid == skid, ]
    unique.assignment <- TRUE
    mmm <- data.frame()
    for (ma in meta) {
      field <- mm[mm$meta == ma, "field"]
      field <- paste(field, collapse = "/")
      field = gsub("^ ","",field)
      if (grepl("Ito|Harten", ma) & length(field) > 1) {
        unique.assignment <- FALSE
      }
      else if (length(field) == 0) {
        field <- NA
      }
      mmm <- rbind(mmm, data.frame(meta = ma, field = field))
    }
    n[as.character(skid), "unique.assignment"] <- unique.assignment
    n[as.character(skid), meta] <- mmm[match(meta, mmm$meta),"field"]
  }
  if(!is.null(n$`cell body fiber`)){
    n$cellBodyFiber = n$cell_body_fiber
    n$cell_body_fiber = NULL
  }
  colnames(n) = sapply(colnames(n),function(x) paste(unlist(strsplit(x," ")),collapse = "_"))
  n$whimsical_name = catmaid_get_neuronnames(n$skid, ...)
  colnames(n) = gsub(" ","_",colnames(n))
  rownames(n) = n$skid
  n
}






#' Create list of FAFB neurons within a hemilineage
#'
#' @description This functions creates a \code{.csv} file listing every neuron annotated as belonging to a certain hemilineage
#' within the FAFB volume. A secondary hemilineage is a set of neurons born together in the late larval stage.
#' They form ~90% of the neurons in FAFB.
#'
#' @param hemilineage a valid hemilineage name. In CATMAID FAFB v14, these appear as annotation that start either in the form \code{ItoLee_Hemilineage}
#' or \code{hartenstein_Hemilineage}.
#' @param side the side of the brain for which you want to get neurons. Depends on the annotation \code{"side: left"}.
#' @param nomenclature whether you are looking for hemilineages in the form \code{ItoLee_Hemilineage}
#' or \code{hartenstein_Hemilineage}.
#' @param path where to save the outputted \code{.csv} file.
#' @param ... methods passed to \code{catmaid} functions.
#' @description To see the available hemilineages you can load the package \code{hemibrainr} and see \code{hemibrain_hemilineages}.
#' @examples
#' \donttest{
#' \dontrun{
#' ## A hemilineage of mostly laterla horn neurons
#' fafb_hemilineage_contents("DL2_dorsal", side = "right")
#' }}
#' @export
#' @rdname fafb_hemilineage_contents
fafb_hemilineage_contents <- function(hemilineage,
                                      side = c("right", "left"),
                                      nomenclature = c("ItoLee","Hartenstein"),
                                      path = getwd(),
                                      ...){
  if(!requireNamespace("fafbseg")){
    remotes::install_github("natverse/fafbseg")
  }
  side = match.arg(side)
  nomenclature = match.arg(nomenclature)
  fafbseg::choose_segmentation('flywire')
  skds = catmaid::catmaid_skids(sprintf("annotation:%s_Hemilineage: %s",nomenclature,hemilineage), ...)
  left = catmaid::catmaid_skids("annotation:side: left", ...)
  if(side=="right"){
    skds = setdiff(skds, left)
  }else{
    skds = intersect(skds, left)
  }
  hl = read.neurons.fafb(skds, OmitFailures = TRUE, ...)
  putative = suppressWarnings(catmaid::catmaid_skids(sprintf("annotation:%s_Hemilineage: %s_putative",nomenclature,hemilineage),...))
  xyz = sapply(hl, function(x) paste(nat::xyzmatrix(x)[nat::rootpoints(x),],collapse=","))
  points = do.call(rbind, lapply(hl, function(x) nat::xyzmatrix(x)[nat::rootpoints(x),]))
  fw.xyz = nat.templatebrains::xform_brain(points, sample='FAFB14', ref="FlyWire")
  colnames(fw.xyz) = c("fw.x","fw.y","fw.z")
  roots = sapply(hl, function(x)
    sprintf("https://neuropil.janelia.org/tracing/fafb/v14/?pid=1&zp=%s&yp=%s&xp=%s&tool=tracingtool&active_node_id=%s&sid0=5&s0=0",
            nat::xyzmatrix(x)[nat::rootpoints(x),]["Z"],
            nat::xyzmatrix(x)[nat::rootpoints(x),]["Y"],
            nat::xyzmatrix(x)[nat::rootpoints(x),]["X"],
            x$d$PointNo[nat::rootpoints(x)]))
  roots2 = sapply(hl, function(x)
    sprintf("https://neuropil.janelia.org/tracing/fafb/v14-seg-li-190805.0/?pid=1&zp=%s&yp=%s&xp=%s&tool=tracingtool&active_node_id=%s&sid0=5&s0=0",
            nat::xyzmatrix(x)[nat::rootpoints(x),]["Z"],
            nat::xyzmatrix(x)[nat::rootpoints(x),]["Y"],
            nat::xyzmatrix(x)[nat::rootpoints(x),]["X"],
            x$d$PointNo[nat::rootpoints(x)]))
  hl.df = hl[,]
  hl.df$confirmed = !hl.df$skid %in% putative
  hl.df$side = side
  hl.df$FAFB.url = roots
  hl.df$FAFB.seg.url = roots2
  hl.df$FAFB.xyz = xyz
  hl.df$flywire.url = ""
  hl.df = cbind(hl.df,fw.xyz)
  hl.df$flywire.id = ""
  hl.df$status = "incomplete"
  hl.df$catmaid.user = "flyconnectome"
  hl.df$flywire.user = "flyconnectome"
  hl.df$matching.user = "flyconnectome"
  hl.df$notes = ""
  file = sprintf("%s/%s_Hemilineage_%s_%s.csv",path,nomenclature,hemilineage,side)
  message("Saving .csv at: ", file)
  chosen.cols = c("skid", "whimsical_name",
                  "ItoLee_Hemilineage",
                  "Hartenstein_Hemilineage", "transmitter", "hemibrain_match",
                  "confirmed",
                  "side", "FAFB.url", "FAFB.seg.url", "FAFB.xyz",
                  "flywire.url", "fw.x", "fw.y", "fw.z",
                  "flywire.id", "status", "catmaid.user", "flywire.user", "matching.user",
                  "notes")
  utils::write.csv(hl.df[,chosen.cols],file=file, row.names= FALSE)
}








