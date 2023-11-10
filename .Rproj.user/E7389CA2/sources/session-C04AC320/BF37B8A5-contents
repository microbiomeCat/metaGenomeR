#' func_summarise
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
func_summarise <- function(dt,samples,method="RC",group_by){
  UseMethod('func_summarise')
}


#' Title
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
#'
func_summarise.default <- function(dt,samples,method="RC",group_by){

  if(method == "RC"){
    dt[!"",lapply(.SD,sum),on=group_by[1],by=group_by,.SDcols=samples]
  }else if(method == "PPM"){
    dt[
      j=c(.SD[,1], lapply(.SD[,-1],function(x){x/sum(x)*10^6})),
      .SDcols = c(group_by,samples)
    ][
      i=!"",
      j=lapply(.SD, function(x){sum(x)}),
      on=group_by,
      by=group_by,
      .SDcols=samples
    ]
  }else if(method == "RPKM"){
    dt[
      j=c(.SD[,group_by,with=F], lapply(.SD[,samples,with=F],function(x){x/length/sum(x)*10^9})),
      .SDcols=c(group_by,samples)
    ][
      i=!"",
      j=lapply(.SD, function(x){sum(x)}),
      on=group_by[1],
      by=group_by,
      .SDcols=samples
    ]
  }else if(method == "TPM"){
    dt[
      j=c(.SD[,group_by,with=F], lapply(.SD[,samples,with=F],function(x){x/length})),
      .SDcols=c(group_by,samples)
    ][
      j=c(.SD[,group_by,with=F], lapply(.SD[,samples,with=F],function(x){x/sum(x)*10^6 })),
      .SDcols=c(group_by,samples)
    ][
      i=!"",
      j=lapply(.SD, function(x){sum(x)}),
      on=group_by[1],
      by=group_by,
      .SDcols=samples
    ]
  }
}

#' func_summarise.fst_table
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
func_summarise.fst_table <- function(dt,samples,method="RC",group_by){

  dt <- dt %>% select_fst(cols=c(samples,group_by,"length"))
  func_summarise.default(dt,samples,method,group_by)
}
