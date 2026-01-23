
#' Extract top N genes from Up/Down
#'
#' @param df data frame
#'
#' @export
#'
ShowTopNGenes <- function(
    df=NULL,
    n=10,
    gene='gene',
    group='signal',
    logfc='log2FC'
){
    # sort up
    d_up <- df[df[[group]]=='Up',]
    up <- d_up[order(d_up[[logfc]], decreasing = TRUE), gene]
    topN_up_gene <- up[1:n]
    
    # sort down
    d_down <- df[df[[group]]=='Down',]
    down <- d_down[order(d_down[[logfc]]), gene]
    topN_down_gene <- down[1:n]

    # merge up + down
    top_gene <- c(topN_up_gene, topN_down_gene)

    return(top_gene)
}








