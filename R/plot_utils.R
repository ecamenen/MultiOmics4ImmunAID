#' @export
plot_corr <- function(x, y, clean_name = TRUE) {
    if (clean_name) {
        colnames(x) <- get_var_names(colnames(x), y)
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

#' @export
plot_normal <- function(x) {
    x %>%
        select(get_not_normal(x)$vars) %>%
        plot_normality()
}

get_colors <- function() {
    c(
        "#cd5b45",
        "#71ad65",
        "#3c78b4",
        "#ffc600",
        "#b448af",
        "#9d9d9d",
        "#abcaef",
        "#4a6f43",
        "#f0e500",
        "#efb8f0",
        "black",
        "#d6d6d6"
    )
}
