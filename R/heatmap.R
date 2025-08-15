
##########################################
##         Diff Markers
##########################################

#' Correct data for heatmap
CorrectInfoByMarkerData <- function(
    mtx=NULL,
    meta=NULL,
    marker_info=NULL
){
    library(dplyr)
    #1.markers
    marker_info <- as.data.frame(marker_info) %>% group_by(gene) %>% filter(n() == 1) %>% ungroup()
    rownames(marker_info) <- 1:nrow(marker_info)
    marker_info$index <- 1:nrow(marker_info)
    
    #2.exp matrix
    marker_genes <- marker_info$gene
    mtx <- as.data.frame(mtx)[marker_genes, ]

    #3.meta
    meta <- as.data.frame(meta)[colnames(mtx), ]

    #print('After Correction:')
    #print('Matrix')
    #print(dim(mtx))
    #print('Meta')
    #print(dim(meta))
    #print('Markers')
    #print(dim(marker_info))

    return(list(mtx, meta, marker_info))
}



#' HeatMap_ExpGroups_Vertical
#'
#' @param data dataframe
#' @param meta top annotation group-1
#' @param marker_info top annotation group-2
#' @param group group name
#' @param sort_group sorted group name
#' @param col_group color group
#' @param show_gene show genes
#' @param scaled scaled or not
#'
#' @export
#'
Heatmap_DiffMarkers_Vert <- function(
    data=NULL,
    meta=NULL,
    marker_info=NULL,
    group='cluster',
    sort_group=NULL,
    col_group=NULL, 
    show_gene=NULL,
    scaled=TRUE,
    row_title='Differentially expressed genes',
    show_column_dend=FALSE,
    legend_title='Expression\nZ-score',
    gap=0.4
) { 

    data_list <- CorrectInfoByMarkerData(mtx=data, meta=meta, marker_info=marker_info)
    mtx <- data_list[[1]]
    meta <- data_list[[2]]
    marker_info <- data_list[[3]]

    # z-score: row z-score
    if (scaled){ mtx <- t(scale(t(mtx))) }

    set.seed(42)
    
    haT <- HeatmapAnnotation(
                Group = anno_simple(x = meta[[group]], simple_anno_size = unit(2, "mm"), col=col_group),
                annotation_name_side = "right",
                annotation_name_gp = gpar(fontsize = 6)
        )

    gene_at <- marker_info$index[marker_info$gene %in% show_gene]
    haR <- rowAnnotation( link = anno_mark(at=gene_at, labels=show_gene, which="bottom", link_width=unit(2,"mm"), labels_gp=gpar(fontsize = 5)) )

    print(1)

    col_score = circlize::colorRamp2(c(-2, 0, 2), c("#2E86C1", "white", "#CB4335"))
    title_ht = "Row Z-Score"

    ht <- Heatmap(
                as.matrix(mtx),
                    
                col = col_score,
                    
                show_row_names = F,
                show_row_dend = F,
                cluster_rows = T,

                show_column_names = F,
                show_column_dend = show_column_dend,
                cluster_columns = T,

                # split
                #column_title = NULL,
                column_title_gp = gpar(fontsize = 6),
                column_split = factor(meta[[group]], levels=sort_group), 
                cluster_column_slices = F,
                column_gap = unit(gap, "mm"),
                
                
                # split rows to three groups
                row_title = row_title, 
                row_title_gp = gpar(fontsize = 6),
                row_split = factor(marker_info$cluster, levels=sort_group),
                cluster_row_slices = F,
                row_gap = unit(gap, "mm"),

                # top-anno
                top_annotation = haT,
                right_annotation = haR,

                # legend
                heatmap_legend_param = list(
                        title = legend_title,
                        title_gp = gpar(fontsize = 5), 
                        labels_gp = gpar(fontsize = 5),
                        grid_width = unit(2, "mm"),
                        grid_height = unit(2, "mm")
                        )
            )

    print(2)

    return(ht)
}





##########################################
##         Motif
##########################################

#' ComplexHeatmap Motif-TF Groupx
#'
#' @param data dataframe
#'
#' @export
#'
HeatmapMotif_Group <- function(
    data = NULL, 
    meta = NULL,
    group = 'cell_type2',
    marker_info = NULL,
    levels = NULL,
    scaled = TRUE,
    col_group = NULL,
    labels = NULL,
    cluster_rows = TRUE,
    show_row_dend = TRUE,
    gap = 0.2,
    border = FALSE,
    ht_title = "Row Z-Score"
){
    # Process data
    data_list <- CorrectInfoByMarkerData(mtx = data, meta = meta, marker_info = marker_info)
    d_mtx <- data_list[[1]]
    data_info <- data_list[[2]]
    marker_info <- data_list[[3]]

    # Set row names
    marker_info$TF_motif <- paste0(marker_info$TF, '(', marker_info$gene, ')')
    rownames(d_mtx) <- marker_info$TF_motif

    # Split factors
    row_split <- factor(marker_info$cluster, levels = levels)
    col_split <- factor(data_info[[group]], levels = levels)

    # Top annotation
    haT <- HeatmapAnnotation(
        Group = anno_simple(x = data_info[[group]], simple_anno_size = unit(2, "mm"), col = col_group),
        annotation_name_side = "right",
        annotation_name_gp = gpar(fontsize = 5, fontface = "bold")
    )

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
            labels_gp = gpar(fontsize = 6, fontface = "plain"),  # Increased font size for clarity
            padding = unit(2, "mm"),  # Increased padding for better spacing
            side = "right"
        )
    )

    # Z-score scaling
    if (scaled) { d_mtx <- t(scale(t(d_mtx))) }

    # Color scale
    col_score <- circlize::colorRamp2(c(-2, -1, 0, 1, 2), c("#440154FF", "#414487FF", "#2A788EFF", "#7AD151FF", "#FDE725FF"))

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
        show_column_dend = FALSE,
        cluster_columns = TRUE,
        row_split = row_split,
        row_gap = unit(gap, "mm"),
        row_title = NULL,
        cluster_row_slices = FALSE,
        column_split = col_split,
        cluster_column_slices = FALSE,
        column_gap = unit(gap, "mm"),
        column_title_gp = gpar(fontsize = 6, fontface = "bold"),
        border = border,
        top_annotation = haT,
        right_annotation = haR,
        heatmap_legend_param = list(
            title = ht_title,
            direction = "horizontal",
            title_position = "lefttop",
            title_gp = gpar(fontsize = 5, fontface = "bold"), 
            labels_gp = gpar(fontsize = 5),
            legend_width = unit(2, "cm"),
            grid_height = unit(2, "mm")
        )
    )

    draw(ht, heatmap_legend_side = "bottom")
}






#' ComplexHeatmap Motif Group-2
#'
#' @param data dataframe
#'
#' @export
#'
HeatmapMotif_Group2 <- function(
    data=NULL, 
    meta=NULL,
    group='cell_type2',
    marker_info=NULL,
    levels=NULL,
    scaled=TRUE,
    col_group=NULL,
    col_sample=NULL,
    labels=NULL,
    gap=0.2,
    border=FALSE,
    ht_title = "Row Z-Score"
){

    data_list <- CorrectInfoByMarkerData(mtx=data, meta=meta, marker_info=marker_info)
    d_mtx <- data_list[[1]]
    data_info <- data_list[[2]]
    marker_info <- data_list[[3]]


    marker_info$TF_motif <- paste0(marker_info$TF, '(', marker_info$gene, ')')
    rownames(d_mtx) <- marker_info$TF_motif

    # split
    row_split <- factor(marker_info$cluster, levels = levels)
    col_split <- factor(data_info[[group]], levels = levels)


    haT <- HeatmapAnnotation(
                Group = anno_simple(x = data_anno[[group]], simple_anno_size = unit(2, "mm"), col=col_group),
                Sample = anno_simple(x = data_anno[[sample_id]], simple_anno_size = unit(2, "mm"), col=col_sample),
                annotation_name_side = "right",
                annotation_name_gp = gpar(fontsize = 5, fontface="bold")
            )

    haR <- rowAnnotation(Motif=anno_mark(at=label_index, labels=labels, labels_gp=gpar(fontsize=5), padding = unit(1, "mm")))


    # z-score
    if (scaled){ d_mtx = t(scale(t(d_mtx))) }

    col_score = circlize::colorRamp2(c(-2, -1, 0, 1, 2), c("#440154FF", "#414487FF", "#2A788EFF", "#7AD151FF", "#FDE725FF"))


    ht <- Heatmap(
                d_mtx,
                
                col = col_score,

                show_row_names = F,
                row_names_gp = gpar(fontsize = 5),
                show_row_dend = TRUE,
                cluster_rows = TRUE,

                show_column_names = F,
                column_names_rot = 60,
                column_names_gp = gpar(fontsize = 5, fontface="bold"),
                show_column_dend = FALSE,
                cluster_columns = TRUE,

                # split row
                row_split = row_split,
                row_gap = unit(gap, "mm"),
                row_title = NULL,
                cluster_row_slices = FALSE,
                #row_title_gp = grid::gpar(fontsize = 7),
                
                # split column
                column_split = col_split,
                cluster_column_slices = FALSE,
                column_gap = unit(gap, "mm"),
                column_title_gp = grid::gpar(fontsize = 6, fontface="bold"),

                border = border,
                
                top_annotation = haT,
                right_annotation = haR,

                # legend
                heatmap_legend_param = list(
                        title = ht_title,
                        direction = "horizontal",
                        title_position = "lefttop",
                        title_gp = gpar(fontsize = 5, fontface="bold"), 
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













