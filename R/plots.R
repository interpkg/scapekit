
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#      Bar plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' Bar plot count
#'
#' @param data frame
#' @param x axis name
#' @param y_lab name
#' @param color color code
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
BarPlotCount <- function(
    data=NULL, 
    x='', 
    y_lab='', 
    color='#681989'
){
    df <- as.data.frame(table(data[[x]]))
    colnames(df) <- c(x, 'n_count')
    
    # barplot
    max_n <- max(df$n_count) * 1.2
    p <- ggplot(df, aes(x=.data[[x]], y=n_count)) +
        geom_bar(fill = color, stat = "identity") +
        geom_text(aes(label = n_count), vjust = 0.5, hjust = -0.1, size = 3) + 
        theme_classic() + 
        aes(x=reorder(.data[[x]], n_count, sum), y=n_count) + 
        coord_flip() + 
        ylim(0, max_n) +
        labs(title='', x="", y=y_lab)

    p
}



