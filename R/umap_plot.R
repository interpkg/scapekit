
###############################
##    Scatter - UMAP
###############################



#' UMAP - Signal
#'
#' @param data dataframe
#'
#' @export
#'
UMAPSignal <- function(
    data=NULL, 
    x='UMAP_1', y='UMAP_2', 
    signal='zr_score1', 
    title='',
    order_dec=FALSE,
    line_size=0.1,
    font_size=6,
    pt_size=0.01, 
    legend_title=NULL,
    breaks=waiver(),
    colors=c("#CB4335", "#2E86C1", "#D7DBDD")

){
    # decreasing true
    data <- data[order(data[[signal]], decreasing=order_dec), ]
    
    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]], color=.data[[signal]])) + 
            geom_point(size=pt_size) +
            theme_classic(base_line_size=line_size) +
            ggtitle(title) +
            theme(plot.title = element_text(hjust = 0.5, size=font_size+1, face = "bold")) +
            theme(text=element_text(size=font_size, face="bold"))

    p <- p + scale_color_manual(name=legend_title, breaks=breaks, values=colors) +
            theme(legend.title=element_text(size=font_size, face="bold"),
                legend.key.width = unit(3, 'mm'),
                legend.key.height = unit(4, 'mm'))

    # color_opt: https://ggplot2.tidyverse.org/reference/scale_viridis.html
    # 'magma','inferno','plasma','viridis','cividis','rocket','mako','turbo'
    #p <- p + scale_color_viridis_c(option=color, direction = color_direc, na.value='#E0E0E0')

    return(p)
}






#' UMAPPlot - split per sample
#'
#' @param data dataframe
#'
#' @export
#'
UMAPSignalSplit <- function(data=NULL, 
                x='UMAP_1', y='UMAP_2', 
                sample='Sample', signal='signal', group='group',
                sorted_sample='',
                decreasing_group=FALSE,
                pt_size=0.01,
                title='',
                legend_title='Signal',
                legend_size=1,
                colors=c("High"="#CB4335", "Medium"="#2E86C1", "Low"="#D7DBDD"),
                breaks=c('High', 'Medium', 'Low'),
                ncol=6
){
    # decreasing or not by signal
    data <- data[order(data[[signal]], decreasing=decreasing_group), ]

    if (length(sorted_sample) > 2){
        data[[sample]] <- factor(data[[sample]], levels=sorted_sample)
    }
    
    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]])) + 
            geom_point(aes(color=.data[[group]]), size=pt_size) +
            theme_classic(base_line_size=0.1) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            theme(axis.ticks = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) +
            labs(title=title, x=x, y=y, color=group) +
            theme(plot.title = element_text(hjust = 0.5, size=8)) +
            theme(text = element_text(size = 6, face = "bold"))

    p <- p + scale_color_manual(name = legend_title, breaks=breaks, values=colors) +
            theme(legend.title=element_text(size=5, face = "bold")) +
            guides(color=guide_legend(override.aes=list(size=legend_size)))

    p <- p + facet_wrap(~ .data[[sample]], ncol=ncol) +
        theme(strip.background = element_blank(), strip.text = element_text(size = 5.5, color = "black", face = "bold")) +
        theme(panel.spacing=unit(1, 'mm', data=NULL))
    
    return(p)
}









