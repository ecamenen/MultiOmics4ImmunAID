#' @export
plot_corr <- function(x, stats, clean_name = TRUE) {
    if (clean_name) {
        colnames(x) <- get_var_names(colnames(x), stats)
        x <- clean_names(x)
    }
    x <- as.data.frame(x)
    M <- get_corr(x, TRUE)
    p.mat <- get_corr(x)

    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

    corrplot(
        M,
        # method="color",
        col = col(200),
        type = "upper",
        order = "hclust",
        addCoef.col = "black",
        # Ajout du coefficient de corrélation
        tl.col = "black",
        tl.srt = 45,
        # Rotation des etiquettes de textes
        # Combiner avec le niveau de significativité
        p.mat = p.mat,
        sig.level = 0.01,
        addgrid.col = NA,
        insig = "blank",
        # Cacher les coefficients de corrélation sur la diagonale
        diag = FALSE
    )
}
