


#' ConvertWideToLong
#'
#' @param data value
#' @param id id.vars
#' @param var_name variable.name
#' @return data frame
#' @export
#'
ConvertWideToLong <- function(data, id='', var_name=''){
    dlong <- reshape2::melt(data, id.vars=id, variable.name=var_name)

    return(dlong)
}











