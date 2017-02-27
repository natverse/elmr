#' Summarise tracer contributions / suggest authorship for set of neurons
#'
#' @inheritParams nblast_fafb
#' @param x A data frame containing the output of summarise_contribution
#' @param type Whether to summarise contributinons towards arbour (nodes) or
#'   synapses
#' @param ... Additional arguments passed by \code{summarise_contribution} to
#'   \code{\link[catmaid]{catmaid_get_contributor_stats}} or by
#'   \code{write_authors/write_ack} to \code{suggest_authorship}
#'
#' @return A data.frame describing contributions or for
#'   \code{write_authors/write_ack} a string describing in descending order of
#'   contribution written to the console by \code{\link{cat}}.
#' @export
#'
#' @examples
#' \donttest{
#' pnsc=summarise_contribution("annotation:^PN$")
#' suggest_authorship(pnsc)
#' write_authors(pnsc, auth=3.0)
#' write_ack(pnsc)
#'
#' pnsc=summarise_contribution("annotation:^PN$", type='synapses')
#' }
summarise_contribution <- function(skids,
                                   type=c("nodes", "synapses", "pre", "post"), ...) {

  type=match.arg(type)

  ul=catmaid_get_user_list()
  uls=ul[,c('full_name','id')]

  stats=catmaid_get_contributor_stats(skids, ...)

  df <- if(type=="synapses"){
    bind_rows(stats[c("pre_contributors", "stats$post_contributors")]) %>%
      group_by(id) %>%
      summarise(n=sum(n)) %>%
      as.data.frame
  } else {
    stats[[paste0(sub("s$","",type),'_contributors')]]
  }

  stats.summ <- inner_join(df, uls, by='id') %>%
    arrange(desc(n)) %>%
    mutate(pct=n/sum(n)*100, cpct=cumsum(pct))
  attr(stats.summ, 'type')=type
  stats.summ
}

#' @description \code{suggest_authorship} suggests how to acknowledge
#'   individuals
#' @param auth The minimum percentage of the total effort for authorship
#' @param ackn The minimum number of nodes/synapses to be acknowledged. When
#'   length of 2, the first number is assumed to be the criterion for arbour
#'   nodes, the second for synapses.
#' @export
#' @rdname summarise_contribution
suggest_authorship <- function(x, auth=5.0, ackn=c(3000, 100)) {
  if(length(ackn)>1)
    ackn=ifelse(attr(x,'type')=="nodes", ackn[1], ackn[2])
  x %>%
    filter(n >= ackn) %>%
    mutate(action = ifelse(pct >= auth, "auth", "ack"))
}

#' @description \code{write_authors} produces a list of authors
#' @export
#' @rdname summarise_contribution
write_authors <- function(x, ...) {
  if(!'action' %in% names(x)) x=suggest_authorship(x, ...)
  with(subset(x, action=='auth'), cat(paste(full_name, collapse=", ")))
}

#' @description \code{write_ack} produces an acknowldgements wording.
#' @export
#' @rdname summarise_contribution
write_ack <- function(x, ...) {
  if(!'action' %in% names(x)) x=suggest_authorship(x, ...)
  with(subset(x, action=='ack'),
       cat("We thank", paste(full_name, collapse=", "),
           "for contributing", round(sum(pct), digits = 1),
           "% of reconstructed arbour cable."))
}