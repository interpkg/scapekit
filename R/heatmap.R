


#' ComplexHeatmap GeneExp Group Default
#'
#' @param data dataframe
#'
#' @export
#'
ComplexHeatmap_Group2 <- function(
    data=NULL, group='', sample_id='',
    row_split=NULL
){

    info <- data[,c(sample_id, group)]
    col_names <- colnames(data)
    features <- col_names[!colnames(data) %in% c(sample_id, group)]
    d_mtx <- as.matrix(t(data[, features]))


    ha <- HeatmapAnnotation(
                Group = info[[group]],
                Sample = info[[sample_id]],
                annotation_name_gp = gpar(fontsize = 7)
            )

    ht_exp <- Heatmap(
                d_mtx,
                    
                show_row_names = T,
                row_names_side = "left",
                row_names_gp = gpar(fontsize = 7),
                show_row_dend = F,
                cluster_rows = T,

                show_column_names = F,
                column_names_rot = 60,
                column_names_gp = gpar(fontsize = 7),
                show_column_dend = T,
                cluster_columns = T,

                # split row
                row_split = row_split,
                row_gap = unit(0.3, "mm"),
                row_title_gp = grid::gpar(fontsize = 7),
                
                # split column
                column_split = factor(info[[group]], levels = unique(info[[group]])),
                cluster_column_slices = FALSE,
                column_gap = unit(0.3, "mm"),
                column_title_gp = grid::gpar(fontsize = 7),
                
                top_annotation = ha,

                # legend
                heatmap_legend_param = list(
                        title_gp = gpar(fontsize = 7), 
                        labels_gp = gpar(fontsize = 6),
                        legend_width = unit(6, "cm")
                    )
            )

    return(ht_exp)
}




#' ComplexHeatmap GeneExp Group-2
#'
#' @param data dataframe
#'
#' @export
#'


ComplexHeatmap_Group2b <- function(
    data=NULL, group='', sample_id='',
    ht_title ='', col_group=NULL, col_sample=NULL,
    zscore=FALSE,
    cluster_columns=TRUE,
    feature_info=NULL, gene='gene',
    show_row_names=TRUE,
    levels=NULL,
    colors=c("blue", "white", "red"),
    limits=NULL,
    max_cutoff=NULL
){

    top_anno <- data[,c(sample_id, group)]
    col_names <- colnames(data)
    features <- col_names[!colnames(data) %in% c(sample_id, group)]
    d_mtx <- as.matrix(t(data[, features]))

    row_names <- rownames(d_mtx)
    feature_info_sorted <- feature_info[row_names,]
    # add new row name to 'd_mtx'
    rownames(d_mtx) <- paste0(feature_info_sorted[[gene]], '(', row_names, ')')

    # split
    row_split <- NULL
    col_split <- NULL
    if (length(levels) > 0){
        row_split <- factor(feature_info_sorted[[group]], levels = levels)
        col_split <- factor(top_anno[[group]], levels = levels)
    } else {
        row_split <- factor(feature_info_sorted[[group]], levels = unique(feature_info_sorted[[group]]))
        col_split <- factor(top_anno[[group]], levels = unique(top_anno[[group]]))
    }
    

    ha <- HeatmapAnnotation(
                Group = anno_simple(x = top_anno[[group]], simple_anno_size = unit(2, "mm"), col=col_group),
                Sample = anno_simple(x = top_anno[[sample_id]], simple_anno_size = unit(2, "mm"), col=col_sample),
                annotation_name_side = "left",
                annotation_name_gp = gpar(fontsize = 7, fontface="bold")
            )

    if (zscore){
        d_mtx = t(scale(t(d_mtx)))
        colors = colorRamp2(c(-2, -1, 0, 1, 2), c("#09103b", "#5f79cf", "white", "#eb6565", "#540506"))
        ht_title = "Row Z-Score"
    }


    color_set <- NULL
    if (length(limits) > 0){
        color_set <- circlize::colorRamp2(c(limits[1], 0, limits[2]), colors)
    } else {
        color_set <- circlize::colorRamp2(c(min(d_mtx), 0, max(d_mtx)), colors)
    }
    

    ht_exp <- Heatmap(
                d_mtx,
                
                col = color_set,

                show_row_names = show_row_names,
                row_names_side = "left",
                row_names_gp = gpar(fontsize = 6),
                show_row_dend = F,
                cluster_rows = F,

                show_column_names = F,
                column_names_rot = 60,
                column_names_gp = gpar(fontsize = 6, fontface="bold"),
                show_column_dend = T,
                cluster_columns = cluster_columns,

                # split row
                row_split = row_split,
                row_gap = unit(0.3, "mm"),
                row_title = NULL,
                cluster_row_slices = FALSE,
                #row_title_gp = grid::gpar(fontsize = 7),
                
                # split column
                column_split = col_split,
                cluster_column_slices = FALSE,
                column_gap = unit(0.3, "mm"),
                column_title_gp = grid::gpar(fontsize = 8, fontface="bold"),
                
                top_annotation = ha,

                # legend
                heatmap_legend_param = list(
                        title = ht_title,
                        direction = "horizontal",
                        title_position = "lefttop",
                        title_gp = gpar(fontsize = 8, fontface="bold"), 
                        labels_gp = gpar(fontsize = 6),
                        legend_width = unit(4, "cm"),
                        grid_height = unit(3, "mm")
                    )
            )

    lgd1 = Legend(title = "Group", labels = names(col_group), legend_gp = gpar(fill = col_group, fontsize = 6), nrow=1)
    lgd2 = Legend(title = "Sample", labels = names(col_sample), legend_gp = gpar(fill = col_sample, fontsize = 6), nrow=1)
    pd = packLegend(list = list(lgd1, lgd2), direction = "horizontal", max_width = unit(10, "cm"), column_gap = unit(5, "mm"), row_gap = unit(5, "mm"))

    #-- draw plot
    draw(ht_exp, heatmap_legend_side = "bottom", annotation_legend_side = "bottom", annotation_legend_list = pd)

}






#' ComplexHeatmap Motif-TF Group-2x
#'
#' @param data dataframe
#'
#' @export
#'
ComplexHeatmapMotif_Group2x <- function(
    data=NULL, group='', sample_id='',
    ht_title ='Motif Score', col_group=NULL, col_sample=NULL,
    zscore=FALSE,
    cluster_columns=TRUE,
    feature_info=NULL, gene='gene',
    labels=NULL,
    levels=NULL,
    colors=c("blue", "white", "red"),
    border=FALSE,
    limits=NULL,
    max_cutoff=NULL
){

    top_anno <- data[,c(sample_id, group)]
    col_names <- colnames(data)
    features <- col_names[!colnames(data) %in% c(sample_id, group)]
    d_mtx <- as.matrix(t(data[, features]))

    row_names <- rownames(d_mtx)
    feature_info_sorted <- feature_info[row_names,]
    # add new row name to 'd_mtx'
    # show - gene(motif_id)
    rownames(d_mtx) <- paste0(feature_info_sorted[[gene]], '(', row_names, ')')

    # show label
    motif_set <- as.data.frame(row.names(d_mtx))
    colnames(motif_set) <- 'motif'
    motif_set$index <- rownames(motif_set)
    label_index <- as.numeric(motif_set[motif_set$motif %in% labels, 'index'])

    # split
    row_split <- NULL
    col_split <- NULL
    if (length(levels) > 0){
        row_split <- factor(feature_info_sorted[[group]], levels = levels)
        col_split <- factor(top_anno[[group]], levels = levels)
    } else {
        row_split <- factor(feature_info_sorted[[group]], levels = unique(feature_info_sorted[[group]]))
        col_split <- factor(top_anno[[group]], levels = unique(top_anno[[group]]))
    }
    

    haT <- HeatmapAnnotation(
                Group = anno_simple(x = top_anno[[group]], simple_anno_size = unit(2, "mm"), col=col_group),
                Sample = anno_simple(x = top_anno[[sample_id]], simple_anno_size = unit(2, "mm"), col=col_sample),
                annotation_name_side = "right",
                annotation_name_gp = gpar(fontsize = 7, fontface="bold")
            )

    haR <- rowAnnotation(Motif=anno_mark(at=label_index, labels=labels, labels_gp=gpar(fontsize=7), padding = unit(1, "mm")))

    if (zscore){
        d_mtx = t(scale(t(d_mtx)))
        colors = colorRamp2(c(-2, -1, 0, 1, 2), c("#09103b", "#5f79cf", "white", "#eb6565", "#540506"))
        ht_title = "Row Z-Score"
    }


    color_set <- NULL
    if (length(limits) > 0){
        color_set <- circlize::colorRamp2(c(limits[1], 0, limits[2]), colors)
    } else {
        color_set <- circlize::colorRamp2(c(min(d_mtx), 0, max(d_mtx)), colors)
    }
    

    ht_map <- Heatmap(
                d_mtx,
                
                col = color_set,

                show_row_names = F,
                row_names_gp = gpar(fontsize = 6),
                show_row_dend = F,
                cluster_rows = F,

                show_column_names = F,
                column_names_rot = 60,
                column_names_gp = gpar(fontsize = 6, fontface="bold"),
                show_column_dend = T,
                cluster_columns = cluster_columns,

                # split row
                row_split = row_split,
                row_gap = unit(0.3, "mm"),
                row_title = NULL,
                cluster_row_slices = FALSE,
                #row_title_gp = grid::gpar(fontsize = 7),
                
                # split column
                column_split = col_split,
                cluster_column_slices = FALSE,
                column_gap = unit(0.3, "mm"),
                column_title_gp = grid::gpar(fontsize = 8, fontface="bold"),

                border = border,
                
                top_annotation = haT,
                right_annotation = haR,

                # legend
                heatmap_legend_param = list(
                        title = ht_title,
                        direction = "horizontal",
                        title_position = "lefttop",
                        title_gp = gpar(fontsize = 8, fontface="bold"), 
                        labels_gp = gpar(fontsize = 6),
                        legend_width = unit(4, "cm"),
                        grid_height = unit(3, "mm")
                    )
            )

    lgd1 = Legend(title = "Group", labels = names(col_group), legend_gp = gpar(fill = col_group, fontsize = 6), nrow=1)
    lgd2 = Legend(title = "Sample", labels = names(col_sample), legend_gp = gpar(fill = col_sample, fontsize = 6), nrow=1)
    pd = packLegend(list = list(lgd1, lgd2), direction = "horizontal", max_width = unit(10, "cm"), column_gap = unit(5, "mm"), row_gap = unit(5, "mm"))

    #-- draw plot
    draw(ht_map, heatmap_legend_side = "bottom", annotation_legend_side = "bottom", annotation_legend_list = pd)

}






#' ComplexHeatmap_GeneExp
#'
#' @param data dataframe
#'
#' @export
#'
# ComplexHeatmap_GeneExp(f_matrix='geneExp.tsv', f_group='info.tsv', f_gene='geneList.txt', title_group='Cell Type', outfile='geneExp.heatmap.pdf')
ComplexHeatmap_GeneExp <- function(
    data
){

    cor_start = as.integer(min(mtx))
    cor_end = as.integer(max(mtx)*0.6)
    n_break = 4
    color_heatmap <- circlize::colorRamp2(seq(cor_start, cor_end, length.out = n_break), c("#0C0550", "#1FC5C3", "#1FC526", "#FFFB1B"))
    #color_heatmap <- colorRamp2(c(0, 1, 2, 3), c("#0C0550", "#1FC5C3", "#1FC526", "#FFFB1B"))
    # color_heatmap <- circlize::colorRamp2(seq(cor_start, cor_end, length.out = 3), c("#4B2991", "#fffcd1", "#D7191C"))
    
    color_set <- ColorBrainSet('cell_type2')

    # top anno
    top_anno <- HeatmapAnnotation(
            'Cell type' = anno_simple(x = group_info$group, simple_anno_size = unit(2, "mm"), col = color_brain_s1),
                annotation_name_side = "left", annotation_name_gp = gpar(fontsize = 7),
                annotation_legend_param = list(title = "")
        )
    

    # show row name with mark
    # https://jokergoo.github.io/ComplexHeatmap/reference/anno_mark.html
    ha = rowAnnotation('gene' = anno_mark(at = c(1:dim(mtx)[1]), labels = rownames(mtx), labels_gp = gpar(col = "black", fontsize = 5)))
    if (length(target_gene) > 30){ ha = NULL }

    # heatmap
    hm <- Heatmap(
                as.matrix(mtx),

                col = color_heatmap,

                row_dend_gp = gpar(col = "#CACACA"),
                column_dend_gp = gpar(col = "#CACACA"),

                show_row_names = F,
                show_row_dend = T,
                cluster_rows = T,

                show_column_names = F,
                show_column_dend = T,
                cluster_columns = T,

                # split column
                column_split = factor(group_info$group, levels = unique(group_info$group)),
                cluster_column_slices = F,
                column_gap = unit(0.5, "mm"),
                column_title = NULL,
                
                top_annotation = top_anno,
                right_annotation = ha,
                
                # legend
                heatmap_legend_param = list(
                        title = 'Expression',
                        direction = "horizontal",
                        title_position = "lefttop",
                        title_gp = gpar(fontsize = 7), 
                        labels_gp = gpar(fontsize = 6),
                        grid_width = unit(4, "mm"),
                        grid_height = unit(2.5, "mm")
                        ),

                use_raster = FALSE
            )
    
    # set legend color
    lgd <- Legend(labels=names(color_set), legend_gp=gpar(fill=color_set), 
            title=c(title_group), title_gp=gpar(fontsize=7), by_row=FALSE, ncol=6,
            labels_gp=gpar(fontsize=6), grid_width=unit(2.5, "mm"), grid_height=unit(3, "mm"))

    pd = packLegend(lgd, 
            direction = "horizontal", max_width = unit(10, "cm"), 
            column_gap = unit(5, "mm"), row_gap = unit(5, "mm")
        )

    # pdf
    pdf(outfile, width = 5, height = 3, useDingbats=FALSE)
    draw(hm, heatmap_legend_side = "bottom", annotation_legend_side = "bottom", annotation_legend_list = pd, merge_legend = F)
    dev.off()
}





#' ComplexHeatmap Rank
#'
#' @param data dataframe
#'
#' @export
#'
ComplexHeatmap_Rank <- function(){
    # input <- '/Users/hsun41/Documents/MackLab/projects/1.proj_plagl1/analysis/human.multiome/hs.integ.epn_19s.analysis.v2/infercnv_sample.snatac.hmm/multiome.malignant.All/analysis.All/markers_peaks/out_peakMarkers.st2_vs_rest/ST2.rankedMotifs.xls'
    d <- read.table(input, sep='\t', header=T)
    d$motif_rank <- -log10(d$pvalue)  # Motif probability



    # make label
    label_anno <- d[,'motif.name']
    names(label_anno) <- rownames(d)
    label_anno <- label_anno[1:10]

    label_gene_at <- as.numeric(names(label_anno))
    label_gene <- as.vector(label_anno)

    #
    # row_ha = rowAnnotation(link = anno_mark(at=label_gene_at, labels=label_gene, labels_gp=gpar(fontsize = 6), extend=unit(0.5,'mm'), link_width=unit(0.1,'mm')))
    row_ha = rowAnnotation( link = anno_mark(at=label_gene_at, labels=label_gene, labels_gp=gpar(fontsize = 6)))

    ht <- Heatmap(d$motif_rank, 
                name = "-log10(pvalue)", 
                col = colorRampPalette(c('#2E86C1', '#F9E79F', '#E74C3C'))(100),

                show_row_names = F,
                show_row_dend = F,
                cluster_rows = T,

                show_column_names = F,

                row_title = 'Motif Rank', 
                row_title_gp = gpar(fontsize = 6),
                right_annotation = row_ha,

                heatmap_legend_param = list(
                    #title_position = "lefttop-rot",
                    #direction = "vertical",
                    title_position = "lefttop",
                    direction = "horizontal", 
                    title_gp = gpar(fontsize = 7), 
                    labels_gp = gpar(fontsize = 6),
                    grid_width = unit(1.5, "mm"),
                    grid_height = unit(1.5, "mm")
                )
            )


    #lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.25, 0.5, 0.75, 1))

    pdf(out, width = 1.1, height = 1.5, useDingbats=FALSE)
    draw(ht, heatmap_legend_side = "bottom", show_heatmap_legend=TRUE)
    dev.off()
}






#' ComplexHeatmap Peak Group
#'
#' @param data dataframe
#'
#' @export
#'
HeatMap_Peaks_Groups <- function(data=NULL, metadata=NULL, top_anno_info=NULL, outfile='heatmap.peak.pdf')
{   
    col_subgroup <- c(
        "ST1" = "#EFC41C",                  
        "ST2" = "#D25727"
    )
    SortedGroupList <- c('ST1', 'ST2')

    col_peak_group <- c(
        "ST1" = "#EFC41C",                  
        "ST2" = "#D25727",
        "Shared" = "#038181"
    )
    SortedGeneGroupList <- c('ST1', 'ST2', 'Shared')


    
    top_anno <- HeatmapAnnotation(
                Peaks = anno_simple(x = top_anno_info$group, simple_anno_size = unit(1, "mm"), col=col_peak_group),
                annotation_name_side = "left",
                annotation_name_gp = gpar(fontsize = 6)
        )


    # z-score: row z-score
    #scaled_mat = scale(t(data))
    #col_zscore = colorRamp2(c(-2, -1, 0, 1, 2), c("#09103b", "#5f79cf", "white", "#eb6565", "#540506"))
    col_zscore = colorRamp2(c(-0.5, 0, 0.5), c("blue", "white", "red"))

    ht <- Heatmap(
                #as.matrix(t(data - 0.5)),
                as.matrix(t(data - 0.5)),
                    
                col = col_zscore,
                    
                show_row_names = F,
                #row_names_gp = gpar(fontsize = 6),
                show_row_dend = F,
                cluster_rows = T,
                row_dend_reorder = FALSE,

                show_column_names = F,
                show_column_dend = F,
                cluster_columns = T,
                
                # heatmap cell size
                #width = unit(4, "mm")*NC,
                #height = unit(0.05, "mm")*NR,


                # split
                #column_title = NULL,
                column_title_gp = gpar(fontsize = 8),
                column_split = factor(top_anno_info$group, levels=SortedGeneGroupList),
                cluster_column_slices = F,
                column_gap = unit(0.5, "mm"),
                

                row_title = '', 
                row_title_gp = gpar(fontsize = 7),

                # split rows to three groups
                row_split = factor(metadata$subgroup, levels=SortedGroupList), 
                cluster_row_slices = F,
                row_gap = unit(0.5, "mm"),

                # top-anno
                top_annotation = top_anno,

                # legend
                heatmap_legend_param = list(
                        title = "Peak signal",
                        title_position = "leftcenter",
                        direction = "horizontal",
                        title_gp = gpar(fontsize = 6), 
                        labels_gp = gpar(fontsize = 5),
                        grid_width = unit(2.5, "mm"),
                        grid_height = unit(2, "mm")
                        )
            )

    pdf(outfile, width = 2.8, height = 1.3, useDingbats=FALSE)
    draw(ht, heatmap_legend_side = "bottom")
    dev.off()
}





#' ComplexHeatmap Cluster
#'
#' @param data dataframe
#'
#' @export
#'
ComplexHeatmap_Cluster <- function(data){

    NR=dim(data)[1]
    NC=dim(data)[2]

    ht <- ComplexHeatmap::Heatmap(as.matrix(df_new), 
            cluster_columns = F,
            name = "Mean motif signal", col = c('blue','#F9E79F','red'),
            row_names_side = "left", row_dend_side = "right", row_dend_width = unit(3, "mm"),
            column_names_side = "top", column_dend_side = "bottom", column_dend_height = unit(3, "mm"), 
            column_names_gp = grid::gpar(fontsize = 6),
            row_names_gp = grid::gpar(fontsize = 6),
            column_names_rot = 45,

            width = unit(5, "mm")*NC,
            height = unit(3, "mm")*NR,

            # legend
            heatmap_legend_param = list(
                            title = 'Motif activity (Mean)',
                            title_position = "leftcenter",
                            direction = "horizontal",
                            title_gp = gpar(fontsize = 5), 
                            labels_gp = gpar(fontsize = 5),
                            grid_width = unit(1.5, "mm"),
                            grid_height = unit(1.5, "mm")
                            )
            )

    
    #width = 2, height = 2
    #draw(ht, heatmap_legend_side = "bottom")

}








#' ComplexHeatmap Motif
#'
#' @param data dataframe
#'
#' @export
#'
ComplexHeatmap_Motif <- function(data){

library(ComplexHeatmap)
library(circlize)


NR=dim(df_new)[1]
NC=dim(df_new)[2]

ht <- ComplexHeatmap::Heatmap(as.matrix(df_new), 
        cluster_columns = F,
        name = "Mean motif signal", col = c('blue','#F9E79F','red'),
        row_names_side = "left", row_dend_side = "right", row_dend_width = unit(3, "mm"),
        column_names_side = "top", column_dend_side = "bottom", column_dend_height = unit(3, "mm"), 
        column_names_gp = grid::gpar(fontsize = 6),
        row_names_gp = grid::gpar(fontsize = 6),
        column_names_rot = 45,

        width = unit(5, "mm")*NC,
        height = unit(3, "mm")*NR,

        # legend
        heatmap_legend_param = list(
                        title = 'Motif activity (Mean)',
                        title_position = "leftcenter",
                        direction = "horizontal",
                        title_gp = gpar(fontsize = 5), 
                        labels_gp = gpar(fontsize = 5),
                        grid_width = unit(1.5, "mm"),
                        grid_height = unit(1.5, "mm")
                        )
        )

outfile <- paste0(outdir, '/out_heatmap.avgMotifSig.plag_family.v2.pdf')
pdf(outfile, width = 2, height = 2, useDingbats=FALSE)
draw(ht, heatmap_legend_side = "bottom")
dev.off()

}



#' ComplexHeatmap GeneExp
#'
#' @param data dataframe
#'
#' @export
#'
ComplexHeatmap_GeneExp2 <- function(){

    d <- d_pf[,c('motif', 'motif.name', 'pvalue')]
    colnames(d) <- c('motif', 'gene', 'PF')
    d$ST1 <- d_st1$pvalue[match(d$motif, d_st1$motif)]
    d$ST2 <- d_st2$pvalue[match(d$motif, d_st2$motif)]
    rownames(d) <- d$motif

    d_tar <- d[motif,]
    rownames(d_tar) <- d_tar$gene
    #rownames(d_tar) <- paste0(d_tar$gene, ' (', d_tar$motif, ')')
    d_tar$gene <- NULL
    d_tar$motif <- NULL
    d_tar <- -log10(d_tar)

    d_tar2 <- t(d_tar)

    scaled_mat = t(scale(t(d_tar2)))
    col_zscore = colorRamp2(c(-2, -1, 0, 1, 2), c("#09103b", "#5f79cf", "white", "#eb6565", "#540506"))
    title_ht = "Row Z-Score"

    NR=dim(scaled_mat)[1]
    NC=dim(scaled_mat)[2]

    ht <- Heatmap(as.matrix(scaled_mat),     
                    col = col_zscore,
                    show_row_names = T,
                    show_row_dend = F,
                    cluster_rows = T,
                    row_dend_reorder = FALSE,

                    show_column_names = T,
                    show_column_dend = F,
                    cluster_columns = T,
                    # heatmap cell size
                    width = unit(3, "mm")*NC,
                    height = unit(3, "mm")*NR,

                    row_names_gp = gpar(fontsize = 6),
                    column_names_gp = gpar(fontsize = 5),

                    # legend
                    heatmap_legend_param = list(
                            title = title_ht,
                            #title_position = "leftcenter-rot",
                            direction = "horizontal",
                            title_gp = gpar(fontsize = 6), 
                            labels_gp = gpar(fontsize = 5),
                            grid_width = unit(2.5, "mm"),
                            grid_height = unit(2, "mm")
                            )
                )
                        

    outfile <- paste0(outdir, '/heatmap.top10motifs.pdf')
    pdf(outfile, width = 3, height = 3, useDingbats=FALSE)
    draw(ht, heatmap_legend_side = "bottom")
    dev.off()

}








#' ComplexHeatmap GeneExp
#'
#' @param data dataframe
#'
#' @export
#'
ComplexHeatmap_Simple1 <- function(){
    # sample-level
    # tar_ct <- 'Neuronal-Like'
    df_avgSig_sample_ct <- df %>% group_by(sample, cell_type2) %>% summarize(PLAGL1 = mean(MA1615.1), PLAGL2 = mean(MA1548.1), PLAG1 = mean(MA0163.1), .groups = 'drop')
    df_avgSig_sample_ct <- data.frame(df_avgSig_sample_ct)
    df_avgSig_sample_ct2 <- df_avgSig_sample_ct %>% filter(cell_type2==tar_ct)

    rownames(df_avgSig_sample_ct2) <- df_avgSig_sample_ct2$sample
    df_avgSig_sample_ct2$sample <- NULL
    df_avgSig_sample_ct2$cell_type2 <- NULL


    # RColorBrewer::brewer.pal(9, "RdBu")
    matrix_avgSig_sample_ct2 <- as.matrix(t(df_avgSig_sample_ct2))

    max_val <- quantile(matrix_avgSig_sample_ct2, probs = c(0, 0.25, 0.5, 0.75, 0.85, 1))[[5]]
    ht <- ComplexHeatmap::Heatmap(matrix_avgSig_sample_ct2, 
            name = "Mean motif signal", col = colorRamp2(c(min(matrix_avgSig_sample_ct2), 0, max_val), c("navy", "white", "firebrick3")),
            row_names_side = "left", row_dend_side = "right", row_dend_width = unit(3, "mm"),
            column_names_side = "bottom", show_column_dend = F, column_dend_height = unit(3, "mm"), 
            column_names_gp = grid::gpar(fontsize = 6),
            row_names_gp = grid::gpar(fontsize = 7),
            column_names_rot = 45,
            # legend
            heatmap_legend_param = list(
                    title_position = "lefttop",
                    direction = "horizontal",
                    title_gp = grid::gpar(fontsize = 7), 
                    labels_gp = grid::gpar(fontsize = 6),
                    grid_width = unit(2, "mm"),
                    grid_height = unit(2, "mm")
                )
            )


    formated_tar_ct <- str_replace_all(tar_ct, "/", "_")
    outfile <- paste0(outdir, '/out_heatmap.avgMotifSig.plag_family.sample.', formated_tar_ct, '.pdf')
    pdf(outfile, width = 3, height = 1.5, useDingbats=FALSE)
    draw(ht, heatmap_legend_side = "bottom", column_title = tar_ct, column_title_gp = grid::gpar(fontsize = 8))
    dev.off()

}







