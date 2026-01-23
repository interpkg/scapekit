
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
    y_cutoff=1.3,
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

    p <- ggplot(data=df, aes(x=.data[[x]], y=-log10(.data[[y]]), color=.data[[group]])) +
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
    p <- p + geom_hline(yintercept = y_cutoff, linetype="dotted", size=0.2, color='black')

    p
}




#' Volcano plot 2 DEGs
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
VolcanoPlot2 <- function(
    df=NULL, 
    x='avg_log2FC',
    y='p_val_adj',
    gene_col='gene',
    x_lab='log2(Fold Change)',
    y_lab='-log10(FDR)',
    title='',
    title_size=8,
    x_cutoff=1,
    y_cutoff=1.3,
    lw=0.2,
    ptz=1, 
    alpha=1,
    text_size=7,
    label=NULL,
    label_size=2,
    label_color='#A42D2D',
    show_top_n=0,
    sort_by='x',
    colors=c('0'="#525252", '1'="#A3A3A3", '2'="#CB4335")
){

    df$vp_x <- df[[x]]
    df$vp_y <- -log10(df[[y]])
    df$color_group <- '1'
    df$color_group[abs(df$vp_x) >= x_cutoff & df$vp_y >= y_cutoff] <- '2'
    df$color_group[abs(df$vp_x) < x_cutoff & df$vp_y < y_cutoff] <- '0'

    max_y <- max(df$vp_y) + 0.5

    if (show_top_n > 1){
        diff <- df[df$color_group == '2', ]
        diff$temp_data_group <- 'UP'
        diff$temp_data_group[diff[[x]] < 0] <- 'DOWN'

        top_gene <- NULL

        if (sort_by == 'x'){
            d_up <- diff[diff$temp_data_group=='UP',]
            up <- d_up[order(d_up[[x]], decreasing = TRUE), gene_col]
            topN_up_gene <- up[1:show_top_n]
            
            # sort down
            d_down <- diff[diff$temp_data_group=='DOWN',]
            down <- d_down[order(d_down[[x]]), gene_col]
            topN_down_gene <- down[1:show_top_n]
        
            # merge up + down
            top_gene <- c(topN_up_gene, topN_down_gene)
        } else {
            d_up <- diff[diff$temp_data_group=='UP',]
            up <- d_up[order(d_up[[y]]), gene_col]
            topN_up_gene <- up[1:show_top_n]
            
            # sort down
            d_down <- diff[diff$temp_data_group=='DOWN',]
            down <- d_down[order(d_down[[y]]), gene_col]
            topN_down_gene <- down[1:show_top_n]

            # merge up + down
            top_gene <- c(topN_up_gene, topN_down_gene)
        }
        
        label <- append(label, top_gene)
        
    }

    p <- ggplot(data=df, aes(x=vp_x, y=vp_y, color=color_group)) +
            geom_point(size=ptz, shape=16, alpha=alpha) + 
            theme_bw() +
            scale_color_manual(values=colors) +
            theme(legend.title = element_blank()) +
            geom_text_repel(data = subset(df, df[[gene_col]] %in% label), aes(label = .data[[gene_col]]), 
                color=label_color, size=label_size, fontface = 'bold', 
                max.overlaps = 100, min.segment.length = 0, segment.size=.2, box.padding=.2)
            
    p <- p + labs(title=title, x=x_lab, y=y_lab) +
            theme(plot.title = element_text(hjust = 0.5, size=title_size, face='bold'), 
                text = element_text(size=text_size, face='bold', colour = 'black'),
                axis.text = element_text(size=text_size, face='bold', colour = 'black')
                ) + 
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = 'black', size = lw),
                axis.ticks = element_line(colour = 'black', size = lw),
                axis.ticks.length=unit(1, "mm")
            ) +
            ylim(0, max_y) +
            theme(legend.position = "none")

    p <- p + geom_vline(xintercept = c(-x_cutoff, x_cutoff), linetype="dashed", size=0.2, color='black')
    # -log10(0.05) = 1.3
    p <- p + geom_hline(yintercept = y_cutoff, linetype="dashed", size=0.2, color='black')

    p
}


