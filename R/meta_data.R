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
                                "ItoLee_Lineage",
                                "ItoLee_Hemilineage",
                                "Hartenstein_Lineage",
                                "Hartenstein_Hemilineage",
                                "Putative_Neurotransmitter",
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
                            "ItoLee_Lineage",
                            "ItoLee_Hemilineage",
                            "Hartenstein_Lineage",
                            "Hartenstein_Hemilineage",
                            "Putative_Neurotransmitter",
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
  colnames(n) = gsub(" ","_",colnames(n))
  rownames(n) = n$skid
  n
}

