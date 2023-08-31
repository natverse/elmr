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
#' @param catmaid_get_compact_skeleton whether to use \code{catmaid::catmaid_get_compact_skeleton} or \code{catmaid::read.neurons.catmaid} to obtain neuron data. In the former case,
#' connectors and tags are not returned.
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
                              catmaid_get_compact_skeleton = TRUE,
                              ...){
  skids = catmaid::catmaid_skids(skids, ...)
  if(batch){
    if(batch==TRUE){ batch = 1000}
    batches = split(1:length(skids), ceiling(seq_along(1:length(skids))/batch))
    n = nat::neuronlist()
    for(i in batches){
      message("Reading neurons ", min(i)," to ",  max(i))
      b = read.neurons.fafb(skids = skids[min(i):max(i)], meta = meta, sub = sub, OmitFailures = OmitFailures, catmaid_get_compact_skeleton = catmaid_get_compact_skeleton, batch = FALSE)
      n = nat::union(n, b)
    }
  }else{
    maddf <- fafb_get_meta(skids = skids, meta, sub = sub, OmitFailures = OmitFailures)
    if(catmaid_get_compact_skeleton){
      sort_skel <- function(skid, ...){
        res = catmaid::catmaid_get_compact_skeleton(skid, ...)
        if (!length(res$nodes))
          stop("no valid nodes for skid:", skid)
        swc = with(res$nodes, data.frame(PointNo = id, Label = 0,
                                         X = x, Y = y, Z = z, W = radius * 2, Parent = parent_id))
        swc$Parent[is.na(swc$Parent)] = -1L
        sp = catmaid:::somapos.catmaidneuron(swc = swc, tags = res$tags)
        if (nrow(sp) == 0) {
          soma_id_in_neuron = NULL
        }
        else {
          soma_id_in_neuron = sp$PointNo
          swc$Label[match(soma_id_in_neuron, swc$PointNo)] = 1L
        }
        n = nat::as.neuron(swc, origin = soma_id_in_neuron, skid = skid, InputFileName = as.character(skid))
        n[names(res[-1])] = res[-1]
        n$skid = skid
        class(n) = c("catmaidneuron", "neuron")
        n
      }
      n <-  nat::nlapply(as.character(skids), sort_skel, connectors = FALSE, tags = FALSE, OmitFailures = OmitFailures, ...)
      n <- nat::as.neuronlist(n)
      names(n) <- unlist(sapply(n, function(x) x$skid))
    }else{
      n <-  catmaid::read.neurons.catmaid(skids, OmitFailures = OmitFailures, ...)
    }
    n[,] = maddf[names(n),]
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
                            "flywire id",
                            "flywire xyz",
                            "FAFB xyz",
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
  n$whimsical_name = catmaid::catmaid_get_neuronnames(n$skid, ...)
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
#' or \code{Hartenstein_Hemilineage}.
#' @param path where to save the outputted \code{.csv} file.
#' @param return whether to return a \code{data.frame} or write to a \code{.csv} at location \code{path}.
#' @param ... methods passed to \code{catmaid} functions.
#' @description To see the available hemilineages you can load the package \code{hemibrainr} and see \code{hemibrain_hemilineages}.
#' @examples
#' \donttest{
#' \dontrun{
#' ## A hemilineage of mostly laterla horn neurons
#' df = fafb_hemilineage_contents("DL2_dorsal", side = "right")
#'
#' # from a list of found hemilineages
#' gs = googlesheets4::read_sheet("1HI8RZJhV8aWC6hw5T0qh2
#' __9D8V0zKdtxx2kkDsuI8Y")
#' for(i in 1:nrow(gs)){
#'   hl = gs[i,"ItoLee_Hemilineage"]
#'   message("Processing ", hl)
#'   fafb_hemilineage_contents(hl, side = "right", return = "csv")
#'   fafb_hemilineage_contents(hl, side = "left", return = "csv")
#' }
#' }}
#' @export
#' @rdname fafb_hemilineage_contents
fafb_hemilineage_contents <- function(hemilineage,
                                      side = c("right", "left"),
                                      nomenclature = c("ItoLee","Hartenstein"),
                                      path = getwd(),
                                      return = c("data.frame","csv"),
                                      ...){
  if(!requireNamespace("fafbseg")){
    remotes::install_github("natverse/fafbseg")
  }
  # Get CATMAID neurons
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
  if(!length(skds)){
    warning("No skids could be found for this hemilneage and side")
    NULL
  }else{
    hl = read.neurons.fafb(skds, OmitFailures = TRUE, catmaid_get_compact_skeleton = TRUE, ...)
    simp = nat::nlapply(hl,nat::simplify_neuron,n=1, OmitFailures = TRUE, ...)
    if(!length(simp)){
      warning("No skids could be read for this hemilneage and side")
      NULL
    }else{
      putative = suppressWarnings(catmaid::catmaid_skids(sprintf("annotation:%s_Hemilineage: %s_putative",nomenclature,hemilineage),...))

      # Get xyz for primary branch points
      branchpoints = sapply(simp, function(y) nat::xyzmatrix(y)[ifelse(length(nat::branchpoints(y)),nat::branchpoints(y),max(nat::endpoints(y))),])
      branchpoints = t(branchpoints)
      FAFB.xyz = apply(branchpoints, 1, paste_coords)

      # Get FlyWire voxel coordinates
      branchpoints.flywire = nat.templatebrains::xform_brain(branchpoints, reference = "FlyWire", sample = "FAFB14", .parallel = TRUE, verbose = TRUE)
      rownames(branchpoints.flywire) = rownames(branchpoints)
      branchpoints.flywire.raw = scale(branchpoints.flywire, scale = c(4, 4, 40), center = FALSE)
      fw.ids = fafbseg::flywire_xyz2id(branchpoints.flywire.raw, rawcoords = TRUE)
      fw.ids[fw.ids=="0"] = NA
      flywire.xyz = apply(branchpoints.flywire.raw, 1, paste_coords)

      # Get URLs to rootpoints
      roots = sapply(simp, function(x)
        sprintf("https://neuropil.janelia.org/tracing/fafb/v14/?pid=1&zp=%s&yp=%s&xp=%s&tool=tracingtool&active_node_id=%s&sid0=5&s0=0",
                nat::xyzmatrix(x)[nat::rootpoints(x),]["Z"],
                nat::xyzmatrix(x)[nat::rootpoints(x),]["Y"],
                nat::xyzmatrix(x)[nat::rootpoints(x),]["X"],
                x$d$PointNo[nat::rootpoints(x)]))
      roots2 = sapply(simp, function(x)
        sprintf("https://neuropil.janelia.org/tracing/fafb/v14-seg-li-190805.0/?pid=1&zp=%s&yp=%s&xp=%s&tool=tracingtool&active_node_id=%s&sid0=5&s0=0",
                nat::xyzmatrix(x)[nat::rootpoints(x),]["Z"],
                nat::xyzmatrix(x)[nat::rootpoints(x),]["Y"],
                nat::xyzmatrix(x)[nat::rootpoints(x),]["X"],
                x$d$PointNo[nat::rootpoints(x)]))

      # Assemble data frame
      hl.data = nat:::summary.neuronlist(hl)
      hl.df = simp[,]
      hl.df$confirmed = !hl.df$skid %in% putative
      hl.df$side = side
      hl.df$FAFB.url = roots
      hl.df$FAFB.seg.url = roots2
      hl.df$FAFB.xyz = FAFB.xyz
      hl.df$flywire.xyz = flywire.xyz
      hl.df$CATMAID.nodes = hl.data[as.character(hl.df$skid),"nodes"]
      hl.df$flywire.url = ""
      hl.df$flywire.id = paste0('"',fw.ids, '"')
      hl.df$status = "incomplete"
      hl.df$catmaid.user = "flyconnectome"
      hl.df$flywire.user = "flyconnectome"
      hl.df$matching.user = "flyconnectome"
      hl.df$also.in = hl.df$total.edts =  hl.df$hemibrain.match.quality = NA
      hl.df$hemibrain.match = hl.df$hemibrain_match
      hl.df$duplicated = duplicated(fw.ids)
      hl.df$notes = ""

      # Choose columns
      chosen.cols = c(
        "FAFB.xyz",
        "skid",
        "flywire.xyz",
        "flywire.id",
        "status",
        "confirmed",
        "side",
        "CATMAID.nodes",
        "ItoLee_Hemilineage","transmitter",
        "hemibrain.match", "hemibrain.match.quality",
        "FAFB.hemisphere.match","FAFB.hemisphere.match.quality",
        "catmaid.user", "flywire.user", "matching.user",
        "duplicated", "also.in", "total.edits",
        "notes",
        "FAFB.url", "FAFB.seg.url", "flywire.url")
      hl.df= hl.df[,intersect(chosen.cols,colnames(hl.df))]

      # return
      if(return == "csv"){
        file = sprintf("%s/%s_Hemilineage_%s_%s.csv",path,nomenclature,hemilineage,side)
        message("Saving .csv at: ", file)
        #hl.df.2 = t(apply(hl.df, 2, function(r) paste0('"=""',r, '"""')))
        utils::write.csv(hl.df,file=file, row.names= FALSE)
      }else{
        hl.df
      }
    }
  }
}

# hidden
paste_coords <- function(xyz){
  paste0("(",paste(xyz,sep=",",collapse=","),")")
}





