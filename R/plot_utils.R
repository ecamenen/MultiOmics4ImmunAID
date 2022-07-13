#' @export
plot_corr <- function(x, y, clean_name = TRUE) {
    if (clean_name) {
        colnames(x) <- get_var_names(colnames(x), y)
        x <- clean_names(x)
    }
    x <- as.data.frame(x)
    M <- get_corr(x, TRUE)
    p.mat <- get_corr(x)

    corrplot(
        M,
        # method="color",
        col = get_colors(),
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

theme_perso0 <- function(p, colors = get_colors()) {
    p +
        xlab("") +
        ylab("") +
        facet_wrap(~name, scales = "free") +
        theme_classic() +
        guides(
            color = "none",
            fill = "none",
            x = "none"
        ) +
        scale_color_manual(values = colors, na.value = "black") +
        scale_fill_manual(values = colors, na.value = "black")
}

theme_perso <- function(
    p,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
    theme_perso0(p, colors = colors) +
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size = 10 * cex),
            axis.title = element_text(face = "bold.italic", size = cex_sub),
            strip.text = element_text(
                size = cex_main,
                face = "bold",
                hjust = 0.5,
                margin = margin(0.5, 0.5, 0.5, 0.5)
            )
        )
}
