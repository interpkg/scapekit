

#' Filter markers
FilterMarkersByGroup <- function(
    data=NULL, 
    pct1='pct.1',
    avg_diff='avg_diff',
    group='cluster', 
    gene='gene',
    n=200
){
    # top x
    df <- data %>%
          group_by(.data[[group]]) %>%
          slice_head(n = n)

    df$index <- 1:nrow(df)
    
    # filter
    d_filtered <- df %>% 
            filter(.data[[pct1]] > 0.3) %>% 
            arrange(desc(.data[[avg_diff]])) %>% 
            distinct(gene, .keep_all = TRUE)

    # re-sort by original index
    diff_marker <- d_filtered[order(d_filtered$index),]
    diff_marker$index <- 1:nrow(diff_marker)
    rownames(diff_marker) <- diff_marker$index
    diff_marker$index <- NULL


    return(diff_marker)
}





#' Correct data for heatmap
CorrectInfoByMarkerData <- function(
    mtx=NULL,
    meta=NULL,
    marker_info=NULL,
    pct1='pct.1',
    avg_diff='avg_diff',
    group='cluster',
    gene='gene',
    n=200
){
    library(dplyr)
    #1.markers
    d_filtered_marker <- FilterMarkersByGroup(data=marker_info, pct1=pct1, avg_diff=avg_diff, group=group, gene=gene, n=n)

    
    #2.exp matrix
    marker_genes <- d_filtered_marker[[gene]]
    mtx <- as.data.frame(mtx)[marker_genes, ]

    #3.meta
    meta <- as.data.frame(meta)[colnames(mtx), ]

    return(list(mtx, meta, d_filtered_marker))
}





##########################################
##         Motif/Gene expression
##########################################

#' ComplexHeatmap Motif-TF Groupx
#'
#' @param df dataframe
#'
#' @export
#'
ComplexHeatmap_GroupX <- function(
    df = NULL, 
    meta = NULL,
    group = 'cell_type2',
    sample = NULL,
    marker_info = NULL,
    topn = 10000,
    pct1='pct.1',
    avg_diff='avg_log2FC',
    tf_gene = FALSE,
    levels = NULL,
    scaled = TRUE,
    col_group = NULL,
    col_sample = NULL,
    labels = NULL,
    cluster_rows = TRUE,
    cluster_col = TRUE,
    show_row_dend = TRUE,
    font_size = 6,
    gap = 0.2,
    border = FALSE,
    color_set = 'motif',
    ht_title = "Row Z-Score",
    row_title = NULL,
    show_column_dend = FALSE,
    legend_title = '',
    ht_lgd_direc = "horizontal",
    show_lgd = FALSE,
    outdir = 'ht_temp'
){

    #--------------// Process data
    data_list <- CorrectInfoByMarkerData(mtx = df, meta = meta, marker_info = marker_info, pct1=pct1, avg_diff=avg_diff, n=topn)
    d_mtx <- data_list[[1]]
    data_info <- data_list[[2]]
    marker_info <- data_list[[3]]

    dir.create(outdir)
    write.table(d_mtx, paste0(outdir, '/matrix.tsv'), sep='\t', quote=F, col.names=NA)
    write.table(data_info, paste0(outdir, '/data_info.xls'), sep='\t', quote=F, col.names=NA)
    write.table(marker_info, paste0(outdir, '/marker_info.xls'), sep='\t', quote=F, col.names=NA)
    #---------------//


    # Set row names  
    if (tf_gene){
        marker_info$TF_motif <- paste0(marker_info$TF, '(', marker_info$gene, ')')
        rownames(d_mtx) <- marker_info$TF_motif
    }
    

    #--------------// Split factors
    row_split <- marker_info$cluster
    col_split <- data_info[[group]]

    if (length(levels) > 1){
        row_split <- factor(marker_info$cluster, levels = levels)
        col_split <- factor(data_info[[group]], levels = levels)
    }
    #---------------//



    #--------------- Top annotation
    haT <- HeatmapAnnotation(
        Group = anno_simple(x = data_info[[group]], simple_anno_size = unit(2, "mm"), col = col_group),
        annotation_name_side = "right",
        annotation_name_gp = gpar(fontsize = font_size, fontface = "bold")
    )

    if (length(group) > 0 & length(sample) > 0){
        haT <- HeatmapAnnotation(
                Group = anno_simple(x = data_info[[group]], simple_anno_size = unit(2, "mm"), col=col_group),
                Sample = anno_simple(x = data_info[[sample]], simple_anno_size = unit(2, "mm"), col=col_sample),
                annotation_name_side = "right",
                annotation_name_gp = gpar(fontsize = font_size, fontface="bold")
            )
    }
    #---------------//



    #--------------// RowAnnotation - Label TFs
    haR <- NULL

    if (length(labels) > 0){
        label_at <- which(rownames(d_mtx) %in% labels)

        if (length(label_at) == 0) {
            stop("No matching labels found in rownames(d_mtx). Check the 'labels' input.")
        }
        labels <- rownames(d_mtx)[label_at] 


        haR <- rowAnnotation(
            TF = anno_mark(
                at = label_at,
                labels = labels,
                labels_gp = gpar(fontsize = font_size - .5, fontface = "bold"),
                padding = unit(0.5, "mm"),
                side = "right"
            )
        )
    }
    #--------------// 



    #-------------- Z-score scaling
    if (scaled) { d_mtx <- t(scale(t(d_mtx))) }


    #-------------- Color scale
    if (color_set == 'motif'){
        score_range <- c(-2, -1, 0, 1, 2)
        colors <- c("#440154FF", "#414487FF", "#2A788EFF", "#7AD151FF", "#FDE725FF")
    }
    if (color_set == 'exp'){
        score_range <- c(-2, 0, 2)
        colors <- c("#2E86C1", "white", "#CB4335")
    }
    if (color_set == 'peak'){
        score_range <- c(-1, 0, 1)
        colors <-c("#E5D4CD", "white", "#2286A9")
    }

    col_score = circlize::colorRamp2(score_range, colors)



    ##--------------// Heatmap
    ht <- Heatmap(
        d_mtx,
        col = col_score,
        
        show_row_names = FALSE,
        row_names_gp = gpar(fontsize = font_size),
        show_row_dend = show_row_dend,
        cluster_rows = cluster_rows,
        
        show_column_names = FALSE,
        column_names_rot = 60,
        column_names_gp = gpar(fontsize = font_size, fontface = "bold"),
        show_column_dend = show_column_dend,
        cluster_columns = cluster_col,

        row_split = row_split,
        row_gap = unit(gap, "mm"),
        row_title = row_title,
        row_title_gp = gpar(fontsize = font_size + 1, fontface = "bold"),
        cluster_row_slices = FALSE,
        
        column_split = col_split,
        cluster_column_slices = FALSE,
        column_gap = unit(gap, "mm"),
        column_title_gp = gpar(fontsize = font_size, fontface = "bold"),
        
        border = border,
        
        top_annotation = haT,
        right_annotation = haR,
        
        heatmap_legend_param = list(
            title = legend_title,
            direction = ht_lgd_direc,
            title_position = "lefttop",
            title_gp = gpar(fontsize = font_size, fontface = "bold"), 
            labels_gp = gpar(fontsize = font_size),
            legend_width = unit(2, "cm"),
            grid_height = unit(2, "mm")
        )
    )
    #--------------// 
    


    #--------------// Show legend or not 
    if (show_lgd){
        lgd1 = Legend(title = "Group", labels = names(col_group), legend_gp = gpar(fill = col_group, fontsize = font_size-1), nrow=1)
        lgd2 = Legend(title = "Sample", labels = names(col_sample), legend_gp = gpar(fill = col_sample, fontsize = font_size-1), nrow=1)
        pd = packLegend(list = list(lgd1, lgd2), direction = "horizontal", max_width = unit(10, "cm"), column_gap = unit(5, "mm"), row_gap = unit(5, "mm"))

        #-- draw plot
        draw(ht, heatmap_legend_side = "bottom", annotation_legend_side = "bottom", annotation_legend_list = pd)
    
    } else {

        return(ht)
        #draw(ht, heatmap_legend_side = "bottom")
    }

}














