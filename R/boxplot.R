

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
    title='', x_lab='', y_lab='Mean peak signal', 
    font_size=7, font_size_title=8,
    nolegend=FALSE, add_test=FALSE,
    colors="supp"
){
    p <- ggpubr::ggboxplot(data, x = x, y = y, 
            color = group, palette = colors, 
            outlier.size = .1, 
            bxp.errorbar=F
          )
    p <- p + theme(axis.line=element_line(size=0.5, colour = "black"), 
                  axis.ticks = element_line(size = 0.5, colour = "black")) +
            labs(title=title, x=x_lab, y = y_lab) +
            theme(plot.title = element_text(hjust = 0.5, size=font_size_title)) +
            theme(text = element_text(size = font_size, face="bold", color = "black"), axis.text= element_text(size = font_size, color = "black")) +
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





#' StandardBoxplot
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
StandardBoxplot <- function(
    data=NULL, x='sample', y='mean_peak_sig', 
    title='', x_lab='', y_lab='Mean peak signal', 
    font_size=7, font_size_title=8,
    nolegend=FALSE, 
    add_test=FALSE, test_method='kruskal.test',
    show_pval_position=10,
    outlier=16,
    alpha=1,
    colors='blue'
){
    p <- ggplot(data, aes_string(x=x, y=y)) +
            stat_boxplot(geom = "errorbar", width = 0.2) +
            geom_boxplot(fill=colors, color="black", alpha=alpha, width=0.5, outlier.shape = outlier, outlier.size = 0.5) +
            theme_classic()

    p <- p + theme(axis.line=element_line(size=0.5, color = "black"), 
                  axis.ticks = element_line(size = 0.5, color = "black")) +
            labs(title =title, x=x_lab, y = y_lab) +
            theme(plot.title = element_text(hjust = 0.5, size=font_size_title, color = "black")) +
            theme(text = element_text(size = font_size, face="bold", color = "black"), axis.text= element_text(size = font_size, color = "black")) +
            theme(legend.title=element_blank()) +
            theme(legend.key.size = unit(4, 'mm'))
            
    if (add_test){
        stat.test <- ggpubr::compare_means(formula = as.formula(paste(y, "~", x)), data = data, method = test_method)
        stat.test <- stat.test %>% mutate(y.position=show_pval_position)
        p <- p + stat_pvalue_manual(stat.test, label = "p = {p.adj}", size=2)
    }

    if (nolegend){
        p <- p + theme(legend.position = "none")
    }

    p 
}




