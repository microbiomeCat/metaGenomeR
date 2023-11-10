#' taxa_summarise
#'
#' @param dt
#' @param samples
#' @param method
#' @param group_by
#'
#' @return
#' @import data.table
#' @import magrittr
#' @export
#'
#' @examples
taxa_summarise <- function(dt,samples,method="RC",group_by){
  UseMethod('taxa_summarise')
}

#' taxa_summarise.default
#'
#' @param dt
#' @param samples
#' @param method
#' @param group_by
#'
#' @return
#' @export
#'
#' @examples
taxa_summarise.default <- function(dt,samples,method="RC",group_by="phylum"){

  levels <- c("domain","phylum","class","order","family","genus","species")
  if(! group_by %in% levels){stop("ERROR: choose group_by from phylum, class, order, family, genus, species! ")}
  levels <- levels[1:which(levels==group_by)]

  if(method == "RC"){
    dt[!"",lapply(.SD,sum),on=group_by,by=levels,.SDcols=samples]
  }else if(method == "PPM"){
    dt[
      j=c(.SD[,levels,with=F], lapply(.SD[,samples,with=F],function(x){x/sum(x)*10^6})),
      .SDcols=c(levels,samples)
    ][
      i=!"",
      j=lapply(.SD, function(x){sum(x)}),
      on=group_by,
      by=levels,
      .SDcols=samples
    ]
  }else if(method == "RPKM"){
    dt[
      j=c(.SD[,levels,with=F], lapply(.SD[,samples,with=F],function(x){x/length/sum(x)*10^9})),
      .SDcols=c(levels,samples)
    ][
      i=!"",
      j=lapply(.SD, function(x){sum(x)}),
      on=group_by,
      by=levels,
      .SDcols=samples
    ]
  }else if(method == "TPM"){
    dt[
      j=c(.SD[,levels,with=F], lapply(.SD[,samples,with=F],function(x){x/length})),
      .SDcols=c(levels,samples)
    ][
      j=c(.SD[,levels,with=F], lapply(.SD[,samples,with=F],function(x){x/sum(x)*10^6 })),
      .SDcols=c(levels,samples)
    ][
      i=!"",
      j=lapply(.SD, function(x){sum(x)}),
      on=group_by,
      by=levels,
      .SDcols=samples
    ]
  }
}

#' taxa_summarise.fst_table
#'
#' @param dt
#' @param samples
#' @param method
#' @param group_by
#'
#' @return
#' @import data.table
#' @import magrittr
#' @import tidyfst
#' @export
#'
#' @examples
taxa_summarise.fst_table <- function(dt,samples,method="RC",group_by="phylum"){

  dt <- dt %>% select_fst(cols=c(samples,group_by,"length"))
  taxa_summarise.default(dt,samples,method,group_by)
}
