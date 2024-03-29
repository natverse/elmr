#' Summarise tracer contributions / suggest authorship for set of neurons
#'
#' @description \code{summarise_contribution} produces a data.frame of
#'   contributions by user. \code{suggest_authorship} can use this to suggest
#'   which contributors meet defined authorship/acknowledgement criteria.
#' @inheritParams nblast_fafb
#' @param x A data frame containing the output of summarise_contribution
#' @param type Whether to summarise contributions towards arbour (nodes) or
#'   synapses
#' @param ... Additional arguments passed by \code{summarise_contribution} to
#'   \code{\link[catmaid]{catmaid_get_contributor_stats}} or by
#'   \code{write_authors/write_ack} to \code{suggest_authorship}
#'
#' @return A data.frame describing contributions or for
#'   \code{write_authors/write_ack} a string describing in descending order of
#'   contribution written to the console by \code{\link{cat}}.
#' @export
#' @importFrom dplyr inner_join bind_rows summarise_ group_by_ arrange_ desc
#'   mutate_ %>%
#' @importFrom catmaid catmaid_get_contributor_stats catmaid_get_user_list
#' @examples
#' \dontrun{
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
    bind_rows(stats[c("pre_contributors", "post_contributors")]) %>%
      group_by_(~id) %>%
      summarise_(n=~sum(n)) %>%
      as.data.frame
  } else {
    stats[[paste0(sub("s$","",type),'_contributors')]]
  }

  stats.summ <- inner_join(df, uls, by='id') %>%
    arrange_(~desc(n)) %>%
    mutate_(pct=~n/sum(n)*100, cpct=~cumsum(pct))
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
#' @importFrom dplyr mutate_ filter_ %>%
#' @rdname summarise_contribution
suggest_authorship <- function(x, auth=5.0, ackn=c(3000, 100)) {
  if(length(ackn)>1)
    ackn=ifelse(attr(x,'type')=="nodes", ackn[1], ackn[2])
  x %>%
    filter_(~n >= ackn) %>%
    mutate_(action = ~ifelse(pct >= auth, "auth", "ack"))
}

#' @description \code{write_authors} produces a list of authors
#' @export
#' @rdname summarise_contribution
write_authors <- function(x, ...) {
  if(!'action' %in% names(x)) x=suggest_authorship(x, ...)
  xauth=x[x$action=='auth',]
  cat(paste(xauth$full_name, collapse=", "))
}

#' @description \code{write_ack} produces an acknowledgements wording.
#' @export
#' @rdname summarise_contribution
write_ack <- function(x, ...) {
  if (!'action' %in% names(x))
    x = suggest_authorship(x, ...)
  xack = x[x$action == 'ack', ]

  cat(
    "We thank",
    paste(xack$full_name, collapse = ", "),
    "for contributing",
    round(sum(xack$pct), digits = 1),
    "% of reconstructed arbour cable."
  )
}
