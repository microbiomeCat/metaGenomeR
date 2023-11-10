#' split_rows
#'
#' @param data
#' @param by_which
#' @param pattern
#' @param Samples
#'
#' @return
#' @import data.table
#' @import magrittr
#' @import stringr
#' @import tibble
#' @export
#'
#' @examples
split_rows <- function(data,by_which,pattern,Samples){

  temp <- str_split(data[,by_which, with = FALSE][[1]],pattern=pattern)
  rowname_list <- sort(unique(unlist(temp)))
  temp_m <- matrix(rep(0,t=length(Samples)*length(rowname_list)),nrow=length(rowname_list) )
  rownames(temp_m)=rowname_list
  colnames(temp_m)=Samples
  for(i in 1:length(temp) ){
    for(j in 1:length(temp[[i]])){
      temp_m[temp[[i]][j],]=temp_m[temp[[i]][j],]+as.numeric(data[i,Samples, with = FALSE])
    }}
  temp_m <- temp_m %>% as.data.frame() %>% rownames_to_column %>% as.data.table
  colnames(temp_m)[1]=by_which
  return(temp_m)
}
