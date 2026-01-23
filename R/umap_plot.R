
###############################
##    Scatter - UMAP
###############################

#' UMAP - Signal Score
#'
#' @param data dataframe
#'
#' @export
#'
UMAPSignalScore <- function(
    data=NULL, 
    x='UMAP_1', y='UMAP_2', signal='zr_score1', 
    order_dec=FALSE,
    title='',
    legend_title='Signal',
    set_mid=NULL,
    line_size=0.1,
    font_size=7,
    pt_size=0.1, 
    legend_size=2,
    colors=c("#D7DBDD", "#2E86C1", "#CB4335")

){
    data <- data[order(data[[signal]], decreasing=order_dec), ]
    
    p <-  ggpubr::ggscatter(data, x=x, y=y, color=signal, shape=16, size=pt_size) + 
            theme_classic(base_line_size=line_size) +
            labs(title=title, color=legend_title) +
            theme(plot.title = element_text(hjust = 0.5, size=font_size+1, face = "bold")) +
            theme(text=element_text(size=font_size, face="bold")) +
            theme(legend.title=element_text(size=font_size, face="bold"))

    if (length(set_mid) > 0){
        p <- p + scale_colour_gradientn(colours = colors, rescaler = ~ scales::rescale_mid(.x, mid = set_mid)) 
    } else {
        p <- p + scale_colour_gradientn(colours = colors)
    }

    return(p)
}







#' UMAP - Signal
#'
#' @param data dataframe
#'
#' @export
#'
UMAPSignal <- function(
    data=NULL, 
    x='UMAP_1', y='UMAP_2', 
    group='signal',
    signal='zr_score1', 
    order_dec=FALSE,
    title='',
    line_size=0.1,
    font_size=7,
    pt_size=0.01, 
    legend_size=2,
    legend_title=NULL,
    breaks=waiver(),
    colors=c("#CB4335", "#2E86C1", "#D7DBDD")

){
    # decreasing true 
    # [Important]
    data <- data[order(data[[signal]], decreasing=order_dec), ]
    
    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]], color=.data[[group]])) + 
            geom_point(size=pt_size) +
            theme_classic(base_line_size=line_size) +
            ggtitle(title) +
            theme(plot.title = element_text(hjust = 0.5, size=font_size+1, face = "bold")) +
            theme(text=element_text(size=font_size, face="bold"))

    p <- p + scale_color_manual(name=legend_title, breaks=breaks, values=colors) +
            theme(legend.title=element_text(size=font_size, face="bold")) +
            guides(color = guide_legend(override.aes = list(size = legend_size)))

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
                order_dec=FALSE,
                pt_size=0.01,
                line_size=0.1,
                title='',
                legend_title='Signal',
                legend_size=2,
                colors=c("High"="#CB4335", "Medium"="#2E86C1", "Low"="#D7DBDD"),
                breaks=c('High', 'Medium', 'Low'),
                ncol=6
){
    # decreasing or not by signal
    # [Important]
    data <- data[order(data[[signal]], decreasing=order_dec), ]

    if (length(sorted_sample) > 2){
        data[[sample]] <- factor(data[[sample]], levels=sorted_sample)
    }
    
    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]], color=.data[[group]])) + 
            geom_point(size=pt_size) +
            theme_classic(base_line_size=line_size) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            theme(axis.ticks = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) +
            labs(title=title, x=x, y=y, color=group) +
            theme(plot.title = element_text(hjust = 0.5, size=8)) +
            theme(text = element_text(size = 6, face = "bold"))

    p <- p + scale_color_manual(name = legend_title, breaks=breaks, values=colors) +
            theme(legend.title=element_text(size=5, face = "bold")) +
            guides(color=guide_legend(override.aes=list(size=legend_size)))

    p <- p + facet_wrap(vars(!!sym(sample)), ncol=ncol) +
        theme(strip.background = element_blank(), strip.text = element_text(size = 5.5, color = "black", face = "bold")) +
        theme(panel.spacing=unit(1, 'mm', data=NULL))
    
    return(p)
}









