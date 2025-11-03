

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
        #p <- p + stat_compare_means(comparisons = data, label.y = max(data[[y]])*1.2, size=2)
        max_y <- max(data[[y]]) * 0.9
        p <- p + stat_compare_means(aes(group = .data[[group]]), label = "p.signif", label.y = max_y)
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
    comp_group=NULL,
    test_method="wilcox.test",
    test_label="p.format",
    font_size_pval=2,
    outlier=16,
    alpha=1,
    angle=0,
    colors=NULL
){
    p <- ggplot(data, aes_string(x=x, y=y)) +
            stat_boxplot(geom = "errorbar", width = 0.2) +
            geom_boxplot(aes_string(fill =x), color="black", alpha=alpha, width=0.5, outlier.shape = outlier, outlier.size = 0.5) +
            theme_classic()

    p <- p + theme(axis.line=element_line(size=0.5, color = "black"), 
                  axis.ticks = element_line(size = 0.5, color = "black")) +
            labs(title =title, x=x_lab, y = y_lab) +
            theme(plot.title = element_text(hjust = 0.5, size=font_size_title, color = "black")) +
            theme(text = element_text(size = font_size, face="bold", color = "black"), axis.text= element_text(size = font_size, color = "black")) +
            theme(legend.title=element_blank()) +
            theme(legend.key.size = unit(4, 'mm'))
    
    if (length(colors) > 0){ 
        p <- p + scale_fill_manual(values = colors) 
    }


    if (angle == 45){
        p <- p + theme(axis.text.x = element_text(angle = angle, hjust = 1, vjust=1))
    }
    if (angle == 90){
        p <- p + theme(axis.text.x = element_text(angle = angle, hjust = 1, vjust=0.5))
    }


    if (length(comp_group) > 0){
        comparison_list <- combn(comp_group, length(comp_group), simplify = FALSE)
        
        p <- p + ggpubr::stat_compare_means(comparisons = comparison_list,
                     method = test_method, size=font_size_pval,
                     label = test_label
                )
    }


    if (nolegend){
        p <- p + theme(legend.position = "none")
    }

    p 
}








#' StandardBoxplot with face_wrap
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
StandardFacetWrapBoxplot <- function(
    data=NULL, 
    x='sample', 
    y='mean_peak_sig', 
    split_by=NULL, 
    ncol=3,
    title='', 
    x_lab='', 
    y_lab='Mean peak signal', 
    font_size=6, 
    font_size_title=8,
    nolegend=FALSE, 
    add_test=FALSE, 
    test_method="wilcox.test",
    font_size_pval=2,
    outlier=16,
    alpha=1,
    angle=0,
    colors=NULL
){
    p <- ggplot(data, aes_string(x=x, y=y)) +
            stat_boxplot(geom = "errorbar", width = 0.2) +
            geom_boxplot(aes_string(fill =x), color="black", alpha=alpha, width=0.5, outlier.shape = outlier, outlier.size = 0.5) +
            theme_bw() + 
            theme(panel.grid = element_blank(),
                strip.background = element_rect(fill = "black", color='black'),
                strip.text = element_text(color = "white", size = font_size, face = "bold", margin = margin(t = 2, b=2, unit = "pt")),
            ) 

    p <- p + theme(axis.line=element_line(size=0.5, color = "black"), 
                  axis.ticks = element_line(size = 0.5, color = "black")) +
            labs(title =title, x=x_lab, y = y_lab) +
            theme(plot.title = element_text(hjust = 0.5, size=font_size_title, color = "black")) +
            theme(text = element_text(size = font_size, face="bold", color = "black"), 
                axis.text= element_text(size = font_size, color = "black")) +
            theme(legend.title=element_blank()) +
            theme(legend.key.size = unit(4, 'mm'))

    # x-angle
    if (angle == 90){
        p <- p + theme(axis.text.x = element_text(angle = angle, hjust = 1, vjust=0.5))
    }
    
    if (length(colors) > 0){ 
        p <- p + scale_fill_manual(values = colors) 
    }

    # facet_wrap
    if (length(split_by) > 0){ 
        p <- p + facet_wrap(~ .data[[split_by]], ncol = ncol)
    }

    if (add_test){
        stat.test <- ggpubr::compare_means(formula = as.formula(paste(y, "~", x)), data = data, method = test_method)
        stat.test <- stat.test %>% mutate(y.position=max(data[[y]]) * 1.05)
        p <- p + ggpubr::stat_pvalue_manual(stat.test, size=font_size_pval, label = "p = {p.adj}", xmin = "group1", xmax = "group2",y.position = "y.position")
    }

    if (nolegend){
        p <- p + theme(legend.position = "none")
    }


    p
}



















