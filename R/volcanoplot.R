#' @include calculate.R
#'
NULL




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#      Volcano plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#' Volcano plot DEGs
#'
#' @param df dataframe
#' @param x axis name
#' @param y axis name
#' @param y_lab name
#' @param colors color code
#'
#' @return plot
#'
#' @import ggplot2 ggrepel
#'
#' @export
#'
VolcanoPlot <- function(
    df=NULL, 
    x='logFC',
    y='FDR',
    group='signal',
    gene_col='gene_symbol',
    x_lab='log2(Fold Change)',
    y_lab='-log10(FDR)',
    x_cutoff=0.585,
    pt_size=.3, 
    alpha=.8,
    title='',
    title_size=8,
    text_size=7,
    label=NULL,
    label_size=2,
    axis_line_size=.5,
    colors=c("Up"="#CB4335",  "Down"="#2E86C1", "NotDiff" ="#D2D2D2")
){

    max_y <- max(-log10(df[[y]])) + 0.5

    p <- ggplot(data=df, aes(x=.data[[x]], y=-log10(.data[[y]]), color=group)) +
            geom_point(size=pt_size, shape=16, alpha=alpha) + 
            theme_classic() +
            scale_color_manual(values=colors) +
            theme(legend.title = element_blank()) +
            geom_point(data = subset(df, df[[gene_col]] %in% label), color = 'black', shape=16, size=pt_size) +
            geom_text_repel(data = subset(df, df[[gene_col]] %in% label), aes(label = .data[[gene_col]]), color='black',
                size=label_size, fontface = 'bold', max.overlaps = 100, min.segment.length = 0, segment.size=.2, box.padding=.2)
            
    p <- p + labs(title=title, x=x_lab, y=y_lab) +
            theme(plot.title = element_text(hjust = 0.5, size=title_size, face='bold'), 
                    text = element_text(size=text_size, face='bold')) + 
            theme(
                axis.ticks = element_line(size = axis_line_size),
                axis.ticks.length=unit(1, "mm"),
                axis.line = element_line(colour = 'black', size = axis_line_size) 
            ) + 
            guides(color = guide_legend(override.aes=list(size=2))) +
            ylim(0, max_y)

    p <- p + geom_vline(xintercept = c(-x_cutoff, x_cutoff), linetype="dotted", size=0.2, color='black')
    # -log10(0.05) = 1.3
    p <- p + geom_hline(yintercept = 1.3, linetype="dotted", size=0.2, color='black')

    p
}







