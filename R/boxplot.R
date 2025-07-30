

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#      Box plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' ggboxplot with P-val for group
#'
#' @param data frame
#' @param x sample name
#' @param y value 
#' @param y_lab y lab 
#' @param group cell type
#' @param colors code
#'
#' @return plot
#'
#' @import ggpubr
#'
#' @export
#'
GGboxplotPval <- function(
    data=NULL, x='sample', y='mean_peak_sig', group=NA, 
    x_lab="", y_lab='Mean peak signal', 
    font_size=7, font_size_title=8,
    nolegend=FALSE, add_test=FALSE,
    colors="supp"
){
    p <- ggpubr::ggboxplot(data, x = x, y = y, 
            color = group, palette = colors, 
            outlier.size = .1, 
            bxp.errorbar=F
          )
    p <- p + theme(axis.line=element_line(size=0.5), 
                  axis.ticks = element_line(size = 0.5)) +
            labs(x=x_lab, y = y_lab) +
            theme(plot.title = element_text(hjust = 0.5, size=font_size_title)) +
            theme(text = element_text(size = font_size, face="bold"), axis.text= element_text(size = font_size)) +
            theme(legend.title=element_blank()) +
            theme(legend.key.size = unit(4, 'mm'))

    if (add_test){
        p <- p + stat_compare_means(comparisons = data, label.y = max(data[[y]])*1.2, size=2)
    }

    if (nolegend){
        p <- p + theme(legend.position = "none")
    }

    p 
}





#' ggboxplot with P-val for group
#'
#' @param data frame
#' @param x sample name
#' @param y value 
#' @param y_lab y lab 
#' @param group cell type
#' @param colors code
#'
#' @return plot
#'
#' @import ggpubr
#'
#' @export
#'
GGboxplotWithErrorBar <- function(
    data=NULL, x='sample', y='mean_peak_sig', group=NA, 
    x_lab="", y_lab='Mean peak signal', 
    font_size=7, font_size_title=8,
    nolegend=FALSE, add_test=FALSE,
    colors=""
){
    p <- ggplot(data, aes(x=x, y=y, fill=group, color=group)) +
            stat_boxplot(geom = "errorbar", linetype=1, width = 0.2) +
            geom_boxplot(width=0.5, outlier.shape = NA) +
    if (nchar(colors) > 0){
        p <- p + scale_color_manual(values = colors)
    }

    p <- p + theme(axis.line=element_line(size=0.5), 
                  axis.ticks = element_line(size = 0.5)) +
            labs(x=x_lab, y = y_lab) +
            theme(plot.title = element_text(hjust = 0.5, size=font_size_title)) +
            theme(text = element_text(size = font_size, face="bold"), axis.text= element_text(size = font_size)) +
            theme(legend.title=element_blank()) +
            theme(legend.key.size = unit(4, 'mm'))
            
    if (add_test){
        p <- p + stat_compare_means(comparisons = data, label.y = max(data[[y]])*1.2, size=2)
    }

    if (nolegend){
        p <- p + theme(legend.position = "none")
    }

    p 
}




