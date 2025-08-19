

#' Filter markers
FilterMarkersByGroup <- function(
    data=NULL, 
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
            filter(pct.1 > 0.3) %>% 
            arrange(desc(avg_diff)) %>% 
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
    group='cluster',
    gene='gene',
    n=100
){
    library(dplyr)
    #1.markers
    d_filtered_marker <- FilterMarkersByGroup(data=marker_info, group=group, gene=gene, n=n)

    
    #2.exp matrix
    marker_genes <- d_filtered_marker[[gene]]
    mtx <- as.data.frame(mtx)[marker_genes, ]

    #3.meta
    meta <- as.data.frame(meta)[colnames(mtx), ]

    #print('After Correction:')
    #print('Matrix')
    #print(dim(mtx))
    #print('Meta')
    #print(dim(meta))
    #print('Markers')
    #print(dim(d_filtered_marker))

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
ComplexHeatmap_Group <- function(
    df = NULL, 
    meta = NULL,
    group = 'cell_type2',
    marker_info = NULL,
    tf_tag = FALSE,
    levels = NULL,
    scaled = TRUE,
    col_group = NULL,
    labels = NULL,
    cluster_rows = TRUE,
    cluster_col = TRUE,
    show_row_dend = TRUE,
    gap = 0.2,
    border = FALSE,
    color_set = 'motif',
    ht_title = "Row Z-Score",
    row_title = NULL,
    show_column_dend = FALSE,
    legend_title = '',
    ht_lgd_direc = "horizontal",
    outdir = 'ht_temp'
){

    #--------------// Process data
    data_list <- CorrectInfoByMarkerData(mtx = df, meta = meta, marker_info = marker_info)
    d_mtx <- data_list[[1]]
    data_info <- data_list[[2]]
    marker_info <- data_list[[3]]

    dir.create(outdir)
    write.table(d_mtx, paste0(outdir, '/matrix.tsv'), sep='\t', quote=F, col.names=NA)
    write.table(data_info, paste0(outdir, '/data_info.xls'), sep='\t', quote=F, col.names=NA)
    write.table(marker_info, paste0(outdir, '/marker_info.xls'), sep='\t', quote=F, col.names=NA)
    #---------------//


    # Set row names  
    if (tf_tag){
        marker_info$TF_motif <- paste0(marker_info$TF, '(', marker_info$gene, ')')
        rownames(d_mtx) <- marker_info$TF_motif
    }
    

    #--------------// Split factors
    row_split <- factor(marker_info$cluster, levels = levels)
    col_split <- factor(data_info[[group]], levels = levels)
    #---------------//



    #--------------- Top annotation
    haT <- HeatmapAnnotation(
        Group = anno_simple(x = data_info[[group]], simple_anno_size = unit(2, "mm"), col = col_group),
        annotation_name_side = "right",
        annotation_name_gp = gpar(fontsize = 6, fontface = "bold")
    )



    #--------------// rowAnnotation 
    # Right annotation (Motif labels)
    if (is.null(labels) || length(labels) == 0) {
        warning("No labels provided for right annotation. Using all row names.")
        label_at <- seq_len(nrow(d_mtx))
        labels <- rownames(d_mtx)
    } else {
        label_at <- which(rownames(d_mtx) %in% labels)
        if (length(label_at) == 0) {
            stop("No matching labels found in rownames(d_mtx). Check the 'labels' input.")
        }
        labels <- rownames(d_mtx)[label_at]  # Ensure labels match the indices
    }

    haR <- rowAnnotation(
        Motif = anno_mark(
            at = label_at,
            labels = labels,
            labels_gp = gpar(fontsize = 5, fontface = "plain"),  # Increased font size for clarity
            padding = unit(2, "mm"),  # Increased padding for better spacing
            side = "right"
        )
    )
    #--------------// 



    # Z-score scaling
    if (scaled) { d_mtx <- t(scale(t(d_mtx))) }

    # Color scale
    # motif color set
    col_score <- circlize::colorRamp2(c(-2, -1, 0, 1, 2), c("#440154FF", "#414487FF", "#2A788EFF", "#7AD151FF", "#FDE725FF"))
    # exp color set
    if (color_set == 'exp'){ col_score = circlize::colorRamp2(c(-2, 0, 2), c("#2E86C1", "white", "#CB4335")) }

    # Heatmap
    ht <- Heatmap(
        d_mtx,
        col = col_score,
        
        show_row_names = FALSE,
        row_names_gp = gpar(fontsize = 5),
        show_row_dend = show_row_dend,
        cluster_rows = cluster_rows,
        
        show_column_names = FALSE,
        column_names_rot = 60,
        column_names_gp = gpar(fontsize = 5, fontface = "bold"),
        show_column_dend = show_column_dend,
        cluster_columns = cluster_col,

        row_split = row_split,
        row_gap = unit(gap, "mm"),
        row_title = row_title,
        row_title_gp = gpar(fontsize = 6, fontface = "bold"),
        cluster_row_slices = FALSE,
        
        column_split = col_split,
        cluster_column_slices = FALSE,
        column_gap = unit(gap, "mm"),
        column_title_gp = gpar(fontsize = 6, fontface = "bold"),
        
        border = border,
        
        top_annotation = haT,
        right_annotation = haR,
        
        heatmap_legend_param = list(
            title = legend_title,
            direction = ht_lgd_direc,
            title_position = "lefttop",
            title_gp = gpar(fontsize = 5, fontface = "bold"), 
            labels_gp = gpar(fontsize = 5),
            legend_width = unit(2, "cm"),
            grid_height = unit(2, "mm")
        )
    )

    return(ht)
    #draw(ht, heatmap_legend_side = "bottom")
}








#' ComplexHeatmap Motif-TF Group2
#'
#' @param df dataframe
#'
#' @export
#'
ComplexHeatmap_Group2 <- function(
    df = NULL, 
    meta = NULL,
    group = 'cell_type2',
    sample = 'orig.ident',
    marker_info = NULL,
    tf_tag = FALSE,
    levels = NULL,
    scaled = TRUE,
    col_group = NULL,
    col_sample=NULL,
    labels = NULL,
    cluster_rows = TRUE,
    cluster_col = TRUE,
    show_row_dend = TRUE,
    gap = 0.2,
    border = FALSE,
    color_set = 'motif',
    ht_title = "Row Z-Score",
    row_title = NULL,
    show_column_dend = FALSE,
    legend_title = '',
    ht_lgd_direc = "horizontal",
    outdir = 'ht_temp'
){

    #--------------// Process data
    data_list <- CorrectInfoByMarkerData(mtx = df, meta = meta, marker_info = marker_info)
    d_mtx <- data_list[[1]]
    data_info <- data_list[[2]]
    marker_info <- data_list[[3]]

    dir.create(outdir)
    write.table(d_mtx, paste0(outdir, '/matrix.tsv'), sep='\t', quote=F, col.names=NA)
    write.table(data_info, paste0(outdir, '/data_info.xls'), sep='\t', quote=F, col.names=NA)
    write.table(marker_info, paste0(outdir, '/marker_info.xls'), sep='\t', quote=F, col.names=NA)
    #---------------//


    # Set row names  
    if (tf_tag){
        marker_info$TF_motif <- paste0(marker_info$TF, '(', marker_info$gene, ')')
        rownames(d_mtx) <- marker_info$TF_motif
    }
    

    #--------------// Split factors
    row_split <- factor(marker_info$cluster, levels = levels)
    col_split <- factor(data_info[[group]], levels = levels)
    #---------------//



    #--------------- Top annotation
    haT <- HeatmapAnnotation(
                Group = anno_simple(x = data_info[[group]], simple_anno_size = unit(2, "mm"), col=col_group),
                Sample = anno_simple(x = data_info[[sample]], simple_anno_size = unit(2, "mm"), col=col_sample),
                annotation_name_side = "right",
                annotation_name_gp = gpar(fontsize = 6, fontface="bold")
            )



    #--------------// rowAnnotation 
    # Right annotation (Motif labels)
    if (is.null(labels) || length(labels) == 0) {
        warning("No labels provided for right annotation. Using all row names.")
        label_at <- seq_len(nrow(d_mtx))
        labels <- rownames(d_mtx)
    } else {
        label_at <- which(rownames(d_mtx) %in% labels)
        if (length(label_at) == 0) {
            stop("No matching labels found in rownames(d_mtx). Check the 'labels' input.")
        }
        labels <- rownames(d_mtx)[label_at]  # Ensure labels match the indices
    }

    haR <- rowAnnotation(
        Motif = anno_mark(
            at = label_at,
            labels = labels,
            labels_gp = gpar(fontsize = 5, fontface = "plain"),  # Increased font size for clarity
            padding = unit(2, "mm"),  # Increased padding for better spacing
            side = "right"
        )
    )
    #--------------// 



    # Z-score scaling
    if (scaled) { d_mtx <- t(scale(t(d_mtx))) }

    # Color scale
    # motif color set
    col_score <- circlize::colorRamp2(c(-2, -1, 0, 1, 2), c("#440154FF", "#414487FF", "#2A788EFF", "#7AD151FF", "#FDE725FF"))
    # exp color set
    if (color_set == 'exp'){ col_score = circlize::colorRamp2(c(-2, 0, 2), c("#2E86C1", "white", "#CB4335")) }

    # Heatmap
    ht <- Heatmap(
        d_mtx,

        col = col_score,
        
        show_row_names = FALSE,
        row_names_gp = gpar(fontsize = 5),
        show_row_dend = show_row_dend,
        cluster_rows = cluster_rows,
        
        show_column_names = FALSE,
        column_names_rot = 60,
        column_names_gp = gpar(fontsize = 5, fontface = "bold"),
        show_column_dend = show_column_dend,
        cluster_columns = cluster_col,
        
        row_split = row_split,
        row_gap = unit(gap, "mm"),
        row_title = row_title,
        row_title_gp = gpar(fontsize = 6, fontface = "bold"),
        
        cluster_row_slices = FALSE,
        column_split = col_split,
        cluster_column_slices = FALSE,
        column_gap = unit(gap, "mm"),
        column_title_gp = gpar(fontsize = 6, fontface = "bold"),
        
        border = border,
        
        top_annotation = haT,
        right_annotation = haR,
        
        heatmap_legend_param = list(
            title = legend_title,
            direction = ht_lgd_direc,
            title_position = "lefttop",
            title_gp = gpar(fontsize = 5, fontface = "bold"), 
            labels_gp = gpar(fontsize = 5),
            legend_width = unit(2, "cm"),
            grid_height = unit(2, "mm")
        )
    )


    lgd1 = Legend(title = "Group", labels = names(col_group), legend_gp = gpar(fill = col_group, fontsize = 5), nrow=1)
    lgd2 = Legend(title = "Sample", labels = names(col_sample), legend_gp = gpar(fill = col_sample, fontsize = 5), nrow=1)
    pd = packLegend(list = list(lgd1, lgd2), direction = "horizontal", max_width = unit(10, "cm"), column_gap = unit(5, "mm"), row_gap = unit(5, "mm"))

    #-- draw plot
    draw(ht, heatmap_legend_side = "bottom", annotation_legend_side = "bottom", annotation_legend_list = pd)

}





















