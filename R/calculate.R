#' @importFrom stats quantile
#'
NULL




#' Calculate min and max plus function
#'
#' @param data value
#' @param min value
#' @param max value
#' @return min and max values
#' @export
#'
CalBounds <- function(data, minc=.25, maxc=.75){
    # Calculate the quartiles and IQR
    Q1 <- quantile(data, minc)
    Q3 <- quantile(data, maxc)
    IQR <- Q3 - Q1

    # Calculate the lower and upper bounds
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR

    return(c(lower_bound, upper_bound))
}



#' Calculate min and max for boxplot
#'
#' @param data value
#' @return min and max values
#' @export
#'
CalMinMaxBoxplot <- function(data){
    # Calculate lower and upper bounds based on the quartiles and IQR
    bounds <- CalBounds(data, .25, .75)

    lower_bound <- bounds[1]
    upper_bound <- bounds[2]

    # Find the min and max values for the boxplot
    min_val <- min(data[data >= lower_bound])  # Lower whisker
    max_val <- max(data[data <= upper_bound])  # Upper whisker

    return(c(min_val, max_val))
}




#' Calculate min and max plus function
#'
#' @param data value
#' @param min value
#' @param max value
#' @return min and max values
#' @export
#'
CalMinMax <- function(data, minc=.25, maxc=.75){
    # Calculate lower and upper bounds based on the quartiles and IQR
    bounds <- CalBounds(data, minc, maxc)

    lower_bound <- bounds[1]
    upper_bound <- bounds[2]

    # Find the min and max values for the boxplot
    min_val <- min(data[data >= lower_bound])  # Lower whisker
    max_val <- max(data[data <= upper_bound])  # Upper whisker

    return(c(min_val, max_val))
}




#' Call Proportion
#'
#' @param data frame
#' @param x sample name
#' @param group cell type
#'
#' @return data frame
#'
#' @export
#'
CallProportion <- function(data, x='', group='')
{
    x_size <- length(unique(data[[x]]))

    dcount <- data %>% group_by(.data[[x]], .data[[group]]) %>% count()
    colnames(dcount) <- c(x, group, 'count')

    d_totalN <- data.frame(table(data[[x]]))

    dcount$total_count <- d_totalN$Freq[match(dcount[[x]], d_totalN$Var1)]
    dcount$ratio <- round(dcount$count/dcount$total_count * 100, 1)
    # <x> <group> count total_count ratio

    return(dcount)
}





#' Call Proportion 2
#'
#' @param data frame
#' @param x main column
#' @param g1 group1
#' @param g2 group2
#'
#' @return data frame
#'
#' @export
#'
CallProportion2 <- function(data, x='', g1='', g2='')
{
    x_size <- length(unique(data[[x]]))

    dcount <- data %>% group_by(.data[[x]], .data[[g1]], .data[[g2]]) %>% count()
    colnames(dcount) <- c(x, g1, g2, 'count')

    d_totalN <- data.frame(table(data[[x]]))

    dcount$total_count <- d_totalN$Freq[match(dcount[[x]], d_totalN$Var1)]
    dcount$ratio <- round(dcount$count/dcount$total_count * 100, 1)
    # <x> <g1> <g2> count total_count ratio


    return(dcount)
}









